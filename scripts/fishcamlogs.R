####Load Packages######
source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("readr")
lp("hms")
lp("readbulk")
lp("readxl")
lp("plotly")

#######import buzzer times from logs######

fold<-"FC3logs"

#bulk read from folder specified, use read_log to read log files.
logs<-read_bulk(directory = paste0("odata/", fold), extension = ".log", fun = read_log)

#filter only rows with buzzer time
buzztime<-logs%>%
  filter(X5=="Buzzer")

buzztime$Site <- ifelse(buzztime$X1 >= '2022-08-12' & buzztime$X1 <= '2022-08-23', "Taylor Islet", 
                        ifelse(buzztime$X1 >= '2022-08-25' & buzztime$X1 <= '2022-09-06', "Ohiat Island", 
                               ifelse(buzztime$X1 >= '2022-09-08' & buzztime$X1 <= '2022-09-16', "Danger Rocks", 0)))

#create dataframe for FC1 (Only run this when fold<-"FC1logs")
#FC1buzz<-buzztime
FC1buzz$Cam<-1
  
#create dataframe for FC2 (Only run this when fold<-"FC2logs")
#FC2buzz<-buzztime
FC2buzz$Cam<-2

#create dataframe for FC3 (Only run this when fold<-"FC3logs")
#FC3buzz<-buzztime
FC3buzz$Cam<-3

#merge three dataframes together
logtimes<-rbind(FC1buzz,FC2buzz,FC3buzz)
logtimes<-logtimes%>%
  select(-c(3:7))%>%
  rename("Date" = "X1", "LogTime" = "X2")

#export to wdata
write.csv(logtimes, "wdata/logtimes.csv", row.names = FALSE)

#######Calculating Mean Drift per day per camera#####

#load in updated logtimes file with AMAR buzzer times added and out of water
#times removed
buzzdrift<-read.csv("odata/logtimes_20240104_final.csv", header = TRUE)

#remove additional columns from .csv
buzzdrift<-buzzdrift%>%
  #select(-c(8:14))%>%
  drop_na(Drift)
#convert Drift to time
buzzdrift$Drift<-as.POSIXct(buzzdrift$Drift, format= "%H:%M:%OS")

str(buzzdrift)

#Calculate mean and sd of drift by day for each cam and site
driftmean<- buzzdrift%>%
  group_by(Site, Cam, Date)%>%
  summarise(meanDrift= mean(Drift), sdDrift= sd(Drift))

#remove date from meanDrift  column
driftmean2<- driftmean %>%
  separate(meanDrift, into = c("WrongDate","meanDrift"), sep = " ", remove = TRUE)%>%
  select(-c(4)) #remove WrongDate column

write.csv(driftmean2,"wdata/driftmean.csv", row.names = FALSE)

#NOTE - manually added in missing dates for fishcam 2 Ohiat Island  using
#mean of day before and after (saved in odata as driftmean_missingFC2added.csv)
#mean per day inspected for large/supsicious variations (one change made, now all looks good.)

######add buzzerdrift times to localization files (use files that have been annotated for Fish Sounds)#####

#import localization files annotated for Fish Sounds
filtlocs<-"Taylor_Islet_LA_2022_14_filtered_2_10_FS"#change folder name here

FSlocs<-imp_raven(path = paste0("wdata/", filtlocs), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.
FSlocs<-FSlocs%>%
  select(-c(110))%>% #a weird additional column was added at the end so needed to remove (may not always need this
  filter(grepl("Spectrogram", View))%>% #currently filtering out Waveform view, see *** below for more on what still need to be done.
  filter(grepl("f|u|F|U", s))%>%#filter to only keep files labelled as fish sound (FS)
  rename("Time" = "Begin Clock Time")

#***** NEED TO FIGURE OUT HOW TO MERGE WAVEFORM DATA INTO SPECTROGRAM COLUMNS

str(FSlocs)

#import edited driftmean csv
driftedit<-read.csv("odata/driftmean_missingFC2added.csv", header = TRUE)
str(driftedit)

#change date column to match format of localization raven tables.
driftedit2<- driftedit%>%
  mutate(across("Date", str_replace, "2022-08-", "2022/8/"))%>%
  mutate(across("Date", str_replace, "2022-09-", "2022/9/"))%>%
  rename("Begin Date" = "Date")
  


str(driftedit2)

#join fish sound localization files to buzzerdrift times (this will throw a many to many error, this is good and because of
#the different values for each of the 3 cameras.  There should be a line for each localization tied to each camera)
locswdrift<-left_join(FSlocs, driftedit2, by = "Begin Date")
#NOTE:Environment window will only show a max of 50 columns so depending on how many
#columns you have in table you may need to scroll using Cols: tab

#convert meanDrift and Time columns to time
locswdrift$meanDrift<- as.POSIXct(locswdrift$meanDrift, format= "%H:%M:%OS")
locswdrift$Time<- as.POSIXct(locswdrift$Time, format= "%H:%M:%OS")
#subtract drift time from localization to get adjusted time of fish calls.
locswdrift$locadjust<-as_hms(locswdrift$Time - locswdrift$meanDrift)

####pull time stamps off video filenames and add to dataframe####

#Creating column with video file names and pulling out dates from filenames (e.g. 2557_FishCam03_20220823T204721.521716Z_1600x1200_awb-auto_exp-night_fr-10_q-20_sh-0_b-50_c-0_i-400_sat-0)

# create dataframe with list of all video files in folder
data_files<- data.frame(vidnames=list.files("odata/Taylor_Islet_LA_2022_FC3_Videos"))

#pull time stamp out of video file names
data_files2<- data_files %>%
  #separate file number from file name and put in column "filenum" (e.g. 2557) and remove _FishCam text completely 
  separate(vidnames, into = c("filenum","fileend"), sep = "_FishCam0", remove = FALSE)%>% 
  #chop up fileend into camera number (e.g. 03), underscore, and then pull apart Year, Month,Day, Hour, Min, Sec, Millisec (sep = pulls apart at # from start of fileend string)
  separate(fileend, into = c("Cam", "ext", "y", "m", "d", "T","hr", "mn", "sc", "remaining"), sep = c(1,2,6,8,10,11,13,15,23) )%>%
  #remove unnecessary columns (underscore, T between Date/Time, rest of file name)
  select(-ext,-T,-remaining)%>%
  #format date/time columns into same format as buzzerdrift times
  mutate(vidstarttime = as.POSIXct(paste(y, m, d,hr, mn, sc), format="%Y %m %d %H %M %OS"))
str(data_files2)
#add five minutes to start time to create start and end video times columns
data_files2$videndtime<-data_files2$vidstarttime+minutes(5)
#pull date from POSIX vidstarttime column
data_files2$'Begin Date'<- as_date(data_files2$vidstarttime)
#turn vidstarttime column into only time without date
data_files2$vidstarttime<- as_hms(data_files2$vidstarttime)
data_files2$videndtime<- as_hms(data_files2$videndtime)
#remove unnecessary date/time single columns and change date format to match other dataframes
data_files3<-data_files2%>%
  select(-c(4:9))%>%
  mutate(across("Begin Date", str_replace, "2022-08-", "2022/8/"))%>%
  mutate(across("Begin Date", str_replace, "2022-09-", "2022/9/"))%>%
  mutate_at(c("Cam"),as.integer)#change Cam to integer to match locsdrift dataframe

####Check if adjusted localization times fall between video times and paste video filenames/times in new matching column####

#join locswdrift dataframe to data_files3 matching at Begin Date and Cam
locswdrift2<-left_join(locswdrift, data_files3, by = c("Begin Date", "Cam"))

#check if localization time falls between video start and end times (if yes put video file name, if no it will say check manually)
#some adjusted localization times don't seem to fit in any video file, inspect why this is. 
locswdrift2$videofile<- ifelse(locswdrift2$locadjust>=locswdrift2$vidstarttime & 
                             locswdrift2$locadjust<=locswdrift2$videndtime,  locswdrift2$vidnames, NA)
locswdrift2$videotime<-as_hms(locswdrift2$locadjust-locswdrift2$vidstarttime)

#*****######remove any columns that don't match to a particular video file 
#*NOTE:  may need to double check this once full datasets are in
locswdrift2<- locswdrift2%>%
  filter(!is.na(videofile))


####Create plots of localization coordinates####

#change folder name here for other study sites
coord<-"Taylor_Islet_LA_2022_14"

#start of import/filter/export function
filt<-function(loc, 
               err_span=2,#specifies error span filter limit
               box_width=10 #specifies distance either side of array center filter limit (e.g. 1m any direction. If you want higher limit for vertical(y) axis you can adjust within function (e.g. box_width*3 = 3m))
){
  
  localizations<- list.files(locswdrift2)# create object with list of all files in loc folder
  data_files
  out_dir<-paste0("wdata/",loc,"_filtered_",err_span,"_",box_width,"/") # creates out directory in working data(wdata) folder with
  dir.create(out_dir, showWarnings = FALSE) #create new folder for filtered data that specifies filter parameters (e.g. Taylor_Islet_LA_2022_filtered_2_1)
  
  rows_along <- function(locswdrift2) seq(nrow(locswdrift2))
  for (i in rows_along(locswdrift2))
    print(i)
  
  
  
  {
    
    sound<- plot_ly(i, x=~x_m, y=~y_m, z=~z_m, colors = colors,
                    marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150))
    # layout(yaxis = list(range = c(-2,2)), xaxis = list (range = c(-2,2)), zaxis = list(range = c(-2,2)))
    
    sound <- sound %>% layout(title = 'Fish Sound Coordinate',
                              scene = list(domain=list(x=c(-2,2),y=c(-2,2),
                                                       # select the type of aspectmode
                                                       aspectmode='cube')))
    sound 
    
    #create plot for each row
    #export plot to folder with file name ??
    #create column in locswdrift2 named plot with name of .png file for each row
    
    #importing data
    yourobject<-imp_raven(path=paste0("odata/", loc),# the path to the FOLDER where your selection table is
                          files=data_files[i],# a vector of files to import or the name of a single file
                          all.data = TRUE)#if FALSE only a portion of the columns are imported
    
    #filter data
    locfilter<- yourobject %>%
      # select(-Class, -Sound.type, -Software)%>% # can use this format to remove unnecessary columns if you want
      filter(x_err_span_m <err_span, y_err_span_m <err_span, z_err_span_m <err_span, 
             x_m <=box_width,x_m >=-box_width, y_m <=box_width,y_m >=-box_width,  z_m <=box_width,z_m >=-box_width*3)
    
    #write data
    write.table(locfilter,# the object you want to export as a selection table 
                file = paste0(out_dir, data_files[i]),# the path and file name using the file extension .txt
                sep = "\t", #how to delineate data
                row.names = FALSE, #row names will mess things up
                quote = FALSE)#putting things in quotes will mess things up.
    
  }}

filt(loc)

#create plot for each row
sound<- plot_ly(locswdrift, x=~x_m, y=~y_m, z=~z_m, colors = colors,
                marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150))
 # layout(yaxis = list(range = c(-2,2)), xaxis = list (range = c(-2,2)), zaxis = list(range = c(-2,2)))

sound <- sound %>% layout(title = 'Fish Sound Coordinate',
                   scene = list(domain=list(x=c(-2,2),y=c(-2,2),
                   # select the type of aspectmode
                    aspectmode='cube')))
sound
png("wdata/sound.png")
  

#export plot to folder with file name ??
out_dir<-paste0("wdata/locplots/")
#create column in locswdrift2 named plot with name of .png file for each row
png(paste0(out_dir, Selection[i],"_" videofile[i],".png"))





