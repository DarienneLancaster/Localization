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
lp("withr")
lp("ggplot2")
lp("processx")
lp("smplot2")  #package that allows you to add correlation stats to graphs

######

#######import buzzer times from logs######

# fold<-"FC3logs"
# 
# #bulk read from folder specified, use read_log to read log files.
# logs<-read_bulk(directory = paste0("odata/", fold), extension = ".log", fun = read_log)
# 
# #filter only rows with buzzer time
# buzztime<-logs%>%
#   filter(X5=="Buzzer")
# 
# buzztime$Site <- ifelse(buzztime$X1 >= '2022-08-12' & buzztime$X1 <= '2022-08-23', "Taylor Islet", 
#                         ifelse(buzztime$X1 >= '2022-08-25' & buzztime$X1 <= '2022-09-06', "Ohiat Island", 
#                                ifelse(buzztime$X1 >= '2022-09-08' & buzztime$X1 <= '2022-09-16', "Danger Rocks", 0)))
# 
# #create dataframe for FC1 (Only run this when fold<-"FC1logs")
# FC1buzz<-buzztime
# FC1buzz$Cam<-1
#   
# #create dataframe for FC2 (Only run this when fold<-"FC2logs")
# FC2buzz<-buzztime
# FC2buzz$Cam<-2
# 
# #create dataframe for FC3 (Only run this when fold<-"FC3logs")
# FC3buzz<-buzztime
# FC3buzz$Cam<-3
# 
# #merge three dataframes together
# logtimes<-rbind(FC1buzz,FC2buzz,FC3buzz)
# logtimes<-logtimes%>%
#   dplyr::select(-c(3:7))%>%
#   rename("Date" = "X1", "LogTime" = "X2")
# 
# #export to wdata
# write.csv(logtimes, "wdata/logtimes.csv", row.names = FALSE)
######

#######Calculating Time Mean Drift per day per camera#####

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
  dplyr::select(-c(4)) #remove WrongDate column

write.csv(driftmean2,"wdata/driftmean.csv", row.names = FALSE)

#NOTE - manually added in missing dates for fishcam 2 Ohiat Island  using
#mean of day before and after (saved in odata as driftmean_missingFC2added.csv)
#mean per day inspected for large/supsicious variations (one change made, now all looks good.)
######

######add buzzerdrift times to localization files (use files that have been annotated for Fish Sounds)#####

#import localization files annotated for Fish Sounds

#use this folder if you want to run all FS annotated localization data together
filtlocs<-"All_Localizations_Daylight_LA_filtered_2_1_FS"#change folder name here


# #use this folder if you want to run summary of how many sounds were localized at TI
# filtlocs<-"TI_AllLocalizations_filtered_2_1"#change folder name here

# #use this code if you're just making localization plots of completed files
 #filtlocs<-"FS_for_locplots"#change folder name here

FSlocs<-imp_raven(path = paste0("odata/", filtlocs), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.
FSlocs<-FSlocs%>%
  #dplyr::select(-c(110:111))%>% #a weird additional column was added at the end so needed to remove (may not always need this
  dplyr::select(-c(106))%>% 
  filter(grepl("Spectrogram", View))%>% #currently filtering out Waveform view, see *** below for more on what still need to be done.
  filter(grepl("f|u|F|U|e", s))%>%#filter to only keep files labelled as fish sound (FS)
  rename("Time" = "Begin Clock Time")
str(FSlocs)

#import edited driftmean csv
driftedit<-read.csv("odata/driftmean_missingFC2added.csv", header = TRUE)
str(driftedit)

#change date column to match format of localization raven tables.
driftedit2<- driftedit%>%
  #mutate(across("Date", str_replace, "2022-08-", "2022/8/"))%>%
 # mutate(across("Date", str_replace, "2022-09-", "2022/9/"))%>%
  rename("Begin Date" = "Date")
  


str(driftedit2)
str(FSlocs)

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
#####

####pull time stamps off video filenames and add to dataframe####

#Creating column with video file names and pulling out dates from filenames (e.g. 2557_FishCam03_20220823T204721.521716Z_1600x1200_awb-auto_exp-night_fr-10_q-20_sh-0_b-50_c-0_i-400_sat-0)

#load file name csv from odata (created in script MP4 filenames)
data_files<-read.csv("odata/MP4filenames.csv",header = TRUE)

#pull time stamp out of video file names
data_files2<- data_files %>%
  #separate file number from file name and put in column "filenum" (e.g. 2557) and remove _FishCam text completely 
  separate(vidnames, into = c("filenum","fileend"), sep = "_FishCam0", remove = FALSE)%>% 
  #chop up fileend into camera number (e.g. 03), underscore, and then pull apart Year, Month,Day, Hour, Min, Sec, Millisec (sep = pulls apart at # from start of fileend string)
  separate(fileend, into = c("Cam", "ext", "y", "m", "d", "T","hr", "mn", "sc", "remaining"), sep = c(1,2,6,8,10,11,13,15,23) )%>%
  #remove unnecessary columns (underscore, T between Date/Time, rest of file name)
  dplyr::select(-ext,-T,-remaining)%>%
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
  dplyr::select(-c(4:9))%>%
  mutate(across("Begin Date", str_replace, "2022-08-0", "2022/8/"))%>%
  mutate(across("Begin Date", str_replace, "2022-08-", "2022/8/"))%>%
  mutate(across("Begin Date", str_replace, "2022-09-0", "2022/9/"))%>%
  mutate(across("Begin Date", str_replace, "2022-09-", "2022/9/"))%>%
  mutate_at(c("Cam"),as.integer)#change Cam to integer to match locsdrift dataframe
#####

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
#####

####add filenames for fc1,2,3 to the same row for each selection####

locstest<-locswdrift2

locstest1<-locstest%>%
  filter(Cam==1)%>%
  dplyr::select(Selection,'Begin Date Time',Site, vidnames, filenum, videotime,)

locstest2<-locstest%>%
  filter(Cam==2)%>%
  dplyr::select(Selection,'Begin Date Time',Site, vidnames, filenum, videotime,)

locstest3<-locstest%>%
  filter(Cam==3)
  

locstest12<- left_join(locstest3,locstest1, by= c("Selection","Begin Date Time", "Site"))

locstestall<-left_join(locstest12,locstest2, by= c("Selection","Begin Date Time", "Site"))
locstestall<-locstestall%>%
  rename("vidnames3"="vidnames.x", "filenum3"="filenum.x", "videotime3"="videotime.x","vidnames1"="vidnames.y", "filenum1"="filenum.y", "videotime1"="videotime.y","vidnames2"="vidnames", "filenum2"="filenum", "videotime2"="videotime")
#####

####arrange data in order by date, filenum, and video time####
#add zeros in front of filenum column so that they will order correctly
locstestall$filenum3<-with_options(
  c(scipen = 999), 
  str_pad(locstestall$filenum3, 5, pad = "0")
)

#arrange however you like
locsarr<-locstestall%>%
  group_by(`Begin Date`, filenum3)%>%
  arrange(videotime3, .by_group = TRUE)

#truncated dataframe that's easier to look at when doing video annotation
minilocsarr<-locsarr%>%
  dplyr::select(c(1,19,90:101,106:110,112,117:129))%>% 
  relocate(1, .after = last_col())

#####

####Create plots of localization coordinates####

## create for loop to make plot for each row (localization) and save plot with selection # and video file as name##
write.csv(minilocsarr,"wdata/minilocsarr.csv", row.names = FALSE)
test<-minilocsarr%>%
  #filter(Site=="Danger Rocks")%>%
  mutate(across("videotime3", str_replace, ":", "."))%>%
  mutate(across("videotime3", str_replace, ":", "."))
  

#loop to create 3D plot of each localization
# for( i in 1:nrow(test)){
# sound<- plot_ly(test, x=~x_m[i], y=~y_m[i], z=~z_m[i], mode= 'markers')
# sound<-layout(sound, scene = list(xaxis = list(title = "Bottom", range = c(1,-1)), 
#                                   yaxis = list(title = "Side", range = c(1,-1)), 
#                                   zaxis = list(title = "Vertical", range = c(-2,2))))
# sound<-layout(sound, title= paste0(test$videofile[i],"\n","Selection = ",test$Selection[i],"\n","Time = ",test$videotime[i],"  z_coord =",test$z_m[i]) )
# orca(p=sound, file = paste0("wdata/3DPlots/","plot",test$Selection[i],"_", test$videofile[i],".png"), )   #save plot with appropriate filename in new folder
#   plotname<-paste0("plot",test$Selection[i],"_", test$videofile[i],".png") #create name for each plot and paste it into dataframe under new column plot
#   test$plot[i]<-plotname
# }
#####

# ####Code to plot all points together (used for testing code)####
# sound<- plot_ly(FSlocs, x=~x_m, y=~y_m, z=~z_m, mode= 'markers', marker = list(size=1, color="black"))
# sound<-layout(sound, scene = list(xaxis = list(title = "Sea Floor", range = c(1,-1)),
#                                    yaxis = list(title = "Sea Floor", range = c(1,-1)),
#                                    zaxis = list(title = "Vertical", range = c(-2,2))))
# sound<-layout(sound, title= "localization errors <20cm" )
# sound
# orca(p=sound, file = "wdata/3DPlots/TIAllLocalizations_filtered_2_1.png")
# ########

# ####ORGINAL GOOD CODE - 2D loop for plotting coordinates with ggplot####
# for( i in 1:nrow(test)){
#   print(ggplot(test, aes(x=x_m[i] , y=y_m[i]))+ #create scatterplot for x and y coordinates
#           geom_point())+
#     expand_limits(x=c(-1,1), y=c(-1, 1))+
#     ggtitle(paste0("Selection#=",test$Selection[i]," s = ",test$s[i],"\n",test$vidnames3[i],"\n",test$videotime3[i], "\n","z_coord=",test$z_m[i], "\n",test$videotime2[i], " ", test$vidnames2[i], "\n", test$videotime1[i], " ", test$vidnames1[i]))+
#     theme(plot.title = element_text(size=4), aspect.ratio = 10/10,
#           axis.text = element_text(size=4),
#           axis.title = element_text(size = 4))
# 
#   ggsave(filename = paste0(test$Site[i],"_",test$filenum3[i],"_",test$videotime3[i],"_",test$Selection[i], ".png"), path="wdata/Localization_Plots", width = 8, height = 8, units = "cm") #save plot with appropriate filename in new folder
#   plotname<-paste0("plot",test$Selection[i],"_", test$videofile[i],".png") #create name for each plot and paste it into dataframe under new column plot
#   test$plot[i]<-plotname
# }

####Updated localization plot code with error bars, call type and AMAR file name####

 # for( i in 1:nrow(test)){
 #   print(ggplot(test, aes(x=x_m[i] , y=y_m[i]))+ #create scatterplot for x and y coordinates
 #         geom_point(size=0.5))+
 #          geom_errorbar(aes(ymin= y_err_low_m[i], ymax= y_err_high_m[i]), width=.05, size = 0.1, colour = "orange")+
 #          geom_errorbarh(aes(xmin= x_err_low_m[i], xmax= x_err_high_m[i]), height=.05, size = 0.1, colour = "orange")+
 #     expand_limits(x=c(-1,1), y=c(-1, 1))+
 #     ggtitle(paste0("Selection#=",test$Selection[i]," s=",test$s[i],"  t=",test$t[i],"\n",test$vidnames3[i],"\n",
 #                    test$videotime3[i], "\n","z_coord=",test$z_m[i], "\n",test$videotime2[i], " ", 
 #                    test$vidnames2[i], "\n", test$videotime1[i], " ", test$vidnames1[i], 
 #                    "\n", test$selec.file[i]))+
 #     theme(plot.title = element_text(size=4), aspect.ratio = 10/10,
 #           axis.text = element_text(size=4),
 #           axis.title = element_text(size = 4))
 # 
 #   ggsave(filename = paste0(test$Site[i],"_",test$filenum3[i],"_",test$videotime3[i],"_",test$Selection[i], ".png"), path="wdata/Localization_Plots_wError", width = 8, height = 8, units = "cm") #save plot with appropriate filename in new folder
 #   plotname<-paste0("plot",test$Selection[i],"_", test$videofile[i],".png") #create name for each plot and paste it into dataframe under new column plot
 #   test$plot[i]<-plotname
 # }


#####

####add in EventMeasure fish ID information and append to localization dataframe####

fishTIstart<-read.csv("odata/EM_annotations_Final/Taylor_Islet/localizations_TI_Aug13to16_20250128.csv", header = TRUE, skip = 4 )
fishTI16<-read.csv("odata/EM_annotations_Final/Taylor_Islet/localizations_TI_Aug16to23_20250116.csv", header = TRUE, skip = 4 )
fishDR<-read.csv("odata/EM_annotations_Final/Danger_Rocks/localizations_20250213DR.csv", header = TRUE, skip = 4 )

fish<-rbind(fishTIstart, fishTI16,fishDR)#combine datasets from each site

fish1<-fish%>%
  dplyr::select(-c(3:21))%>% #get rid of unnecessary columns
  separate(Notes, into = c("fishnum", "ID_confidence","Comments"), sep = "_")%>% #separate out values in Notes column into separate columns
  separate(Filename, into = c("vidnum","CamName", "Date"), sep = "_", remove = FALSE)%>%
  separate(Date, into = c("Date"), sep = "T")

#add zeros to start of fish num so it will order correctly  
fish1$fishnum<-with_options(
  c(scipen = 999), 
  str_pad(fish1$fishnum, 4, pad = "0")
)
#add zeros to start of Localization ID
fish1$Selection<-with_options(
  c(scipen = 999), 
  str_pad(fish1$Selection, 5, pad = "0")
)

# #add zeros to start of vidnum
# fish1$vidnum<-with_options(
#   c(scipen = 999), 
#   str_pad(fish1$vidnum, 5, pad = "0")
# )
 #combine fishnum and Date
fish1<-fish1%>%
unite(fishID, fishnum, Date, sep = "_", remove = FALSE)
fish1$vidnum<-as.numeric(fish1$vidnum)

#fix fish ID for across date annot
str(fish1)

fish1$fishID[fish1$fishID == "0001_20220815" & fish1$Selection == "01212"] <- "0001_20220816"

#how to count number of calls per unique fish
#create separate frames for enter exit

fishE<-fish1%>%
  filter(grepl("e", Enter_Exit))

fishX<-fish1%>%
  filter(grepl("x", Enter_Exit))

#join enter and exit frames together by FishID
fishEX<- left_join(fishE,fishX, by= c("fishID"))
#####

####calculate total time in FOV using vidnums and number of frames per 5min file.####
fishEX$totframes<-fishEX$vidnum.y-fishEX$vidnum.x
fishEX$totframes<-replace(fishEX$totframes,fishEX$totframes==0, "NA")
str(fishEX)
fishEX$totframes<-as.numeric(fishEX$totframes)

fishEX$totframes<-ifelse(is.na(fishEX$totframes),(fishEX$Frame.y-fishEX$Frame.x),
                         ifelse(fishEX$totframes!="NA",((2995-fishEX$Frame.x)+((fishEX$totframes-1)*2995)+(fishEX$Frame.y)),"check"))
fishEX$tottime<-as_hms(fishEX$totframes/9.983333333)#convert to time (there are 9.9833333 frames per second)

#keep only ID column and tottime column
fishEX<-fishEX%>%
  dplyr::select(fishID, tottime)
#bind fishEX to main fish1 table
fish1<-left_join(fish1,fishEX,by="fishID")

Ftotsounds<-fish1%>%
  filter(!grepl("e|x", Enter_Exit))%>%
  count(fishID,Selection)%>%
  count(fishID)

fish1<-fish1%>%
  left_join(Ftotsounds, by="fishID")%>%
  rename("soundsperfish"="n","videofile"="Filename")
#####

###############################
#join video data to audio data (use files that have edited selection boxes)

###load edited Raven selection tables
N<-imp_raven(path = paste0("odata/FS_selection_tables_Edited_and_Original_combo"), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.

#fix error in Edits column name
#create new dataframe with only tables with column named Edtis
N22<-N%>%
  dplyr::select(-106)%>% #remove duplicate column for selec.file
  filter(!is.na(Edtis))%>%
  dplyr::select(-Edits)%>%
  rename(Edits = Edtis)

#create new dataframe with bad Edtis column removed
N33<-N%>%
  dplyr::select(-106)%>% #remove duplicate column for selec.file
  filter(is.na(Edtis))%>%
  dplyr::select(-Edtis)

#join fixed up dataframe N22 to N33
N44<-rbind(N22, N33)

N1<-N44%>%
  separate(Edits, into = c("Edit","Selection_N"), sep = "_", remove = FALSE)%>% #separate Edits column into two columns
  filter(Edit !="r")%>% #remove any column marked with an r for Remove (these are bad selections)
  # filter(Edit =="a"| Edit == "n")%>% #keep only rows marked a (additional selection), or n (new selection - replaces bad selection boxes)
  rename_all(~ gsub(" ", ".", gsub("\\(", ".", gsub("\\)", ".", gsub("/", ".", gsub("%", ".", gsub("-", ".", .))))))) #make column names match my subbing . in for all special symbols and spaces

###Move Selection numbers for original localizations into new column Selection_N (only for rows where Selection_N is blank)
N1$Selection<-as.character(N1$Selection)

N11 <- N1 %>%
  mutate(Selection_N = if_else(is.na(Selection_N), Selection, Selection_N))

#Create dataframe with only additional selections (a)
N_a<-N11%>%
  filter(Edit == "a")

#remove old localization data that has been replaced by new annotations (marked with n in Edits column)
N12 <- N11 %>%
  filter(Edit != "a")%>% #remove additional selections
  group_by(Begin.Path, Selection_N) %>% # group by Begin.Path and Selection_N
  filter(!(Edit == "" & n() > 1)) %>% #remove row were Begin.Path and Selection_N have same value and where Edit column is blank (n() >1 checks for duplicate rows with same Selection Number)
  filter(s != "")%>% #remove unlabled localizations (e.g. not noise, FS or unknown sounds)
  ungroup()

#rejoin additional annotations to N12 dataframe
N13<-rbind(N12, N_a)
levels(as.factor(N13$Edit))

N14<-N13%>%
  mutate(Edit = if_else(Edit == "", "e", Edit))%>%
  dplyr::select(-Edits)

############################################################################
#join original sound localization files to video to match videofilename to Amar file name.  
#Then keep only needed columns to join to edited selection data

####join edited EM fish file to AMAR localization file####
#add zeros to start of Selection so it will match EM format  
locsarr$Selection<-with_options(
  c(scipen = 999), 
  str_pad(locsarr$Selection, 5, pad = "0")
)

#create dataframe of AMAR localizations/sound measurements/fish ID annotations
SoundnVid<-locsarr%>%left_join(fish1, by= c("videofile", "Selection"))%>%
  relocate(1, .after = last_col())%>%
  filter(!grepl("e|x|m", Enter_Exit))%>% #removes columns with enter, exit, and missing (e.g. could not locate source of sound)
  #dplyr::select(-c(Cam:vidnames3, vidstarttime:videndtime,videotime3:videotime2))%>%
  filter(!is.na(fishnum))%>% #filters out any localizations that haven't been annotated yet  (Need to figure out how to flag if there's an annotation issue from my EM annotations - like typo in selection  #)
  separate(n, into = c("bs","notes"), sep = "_", remove = FALSE)

SnV<-SoundnVid%>%
  rename_all(~ gsub(" ", ".", gsub("\\(", ".", gsub("\\)", ".", gsub("/", ".", gsub("%", ".", gsub("-", ".", .)))))))%>% #make column names match my subbing . in for all special symbols and spaces
  # filter(Selection == 3621)%>%
  #filter(Begin.Path =="AMAR173.4.20220911T020952Z.wav")
  mutate(Selection_N = Selection)%>% 
  dplyr::select(Begin.Path, Site:Selection_N)%>%
  mutate(Selection_N = as.character(Selection_N))%>%
  dplyr::select(-Selection)

#add zeroes to edited selections in edited sound data so it will match original data
N14$Selection_N<-with_options(
  c(scipen = 999), 
  str_pad(N14$Selection_N, 5, pad = "0")
)

#join edited selection tables to video data
SnV_edit<-left_join(N14, SnV, by = c("Begin.Path", "Selection_N"))

#remove any rows where selections were never matched to a fish in the video
SnV_edit<-SnV_edit%>%
  filter(!is.na(fishID))

#check for errors in s and t columns and fix
levels(as.factor(SnV_edit$t))

SnV_edit1<-SnV_edit%>%
  mutate(t = recode(t, "dg" = "g", "gdg" = "g", "ds" = "d"))%>%
  mutate(t = ifelse(s == "e" & t == "", "e", t))

write.csv(SnV_edit1,"wdata/SoundnVid_20250213.csv", row.names = FALSE)
#####

####create new localization plots for fish measurements in EventMeasure####

# FishM<-SoundnVid%>%
#   filter(!Enter_Exit %in% c("m", "e", "x"))%>%
#   filter(ID_confidence != 3)%>%
#   group_by(fishID) %>%
#   slice_head(n = 1) %>%
#   ungroup() 
#   
# 
# levels(as.factor(FishM$ID_confidence))
# 
# ####Create localization plots for measuring fish with extra info like species and comments in plot details####
# 
# FishM<-FishM%>%
#   #filter(Site=="Danger Rocks")%>%
#   mutate(across("videotime3", str_replace, ":", "."))%>%
#   mutate(across("videotime3", str_replace, ":", "."))

# for( i in 1:nrow(FishM)){
#   print(ggplot(FishM, aes(x=x_m[i] , y=y_m[i]))+ #create scatterplot for x and y coordinates
#           geom_point(size=0.5))+
#     geom_errorbar(aes(ymin= y_err_low_m[i], ymax= y_err_high_m[i]), width=.05, size = 0.1, colour = "orange")+
#     geom_errorbarh(aes(xmin= x_err_low_m[i], xmax= x_err_high_m[i]), height=.05, size = 0.1, colour = "orange")+
#     expand_limits(x=c(-1,1), y=c(-1, 1))+
#     ggtitle(paste0("Selection#=",FishM$Selection[i], " fishID =", FishM$fishID[i], " Note=", FishM$Comments[i],"\n",FishM$vidnames3[i],"\n",
#                    FishM$videotime3[i], "\n",FishM$Genus[i]," ",FishM$Species[i], " " ,"z_coord=",FishM$z_m[i], "\n",FishM$videotime2[i], " ", 
#                    FishM$vidnames2[i], "\n", FishM$videotime1[i], " ", FishM$vidnames1[i], 
#                    "\n", FishM$selec.file[i]))+
#     theme(plot.title = element_text(size=4), aspect.ratio = 10/10,
#           axis.text = element_text(size=4),
#           axis.title = element_text(size = 4))
#   
#   ggsave(filename = paste0(FishM$Site[i],"_",FishM$filenum3[i],"_",FishM$videotime3[i],"_",FishM$Selection[i],"_", FishM$fishID[i], ".png"), path="wdata/Loc_Plots_forMeasurements", width = 8, height = 8, units = "cm") #save plot with appropriate filename in new folder
#   plotname<-paste0("plot",FishM$Selection[i],"_", FishM$videofile[i],".png") #create name for each plot and paste it into dataframe under new column plot
#   FishM$plot[i]<-plotname
# }

##########match fish measurements to existing soundnvid data##########

fish_length<-read.csv("odata/fish_measurements_20241125.csv", header = TRUE, skip = 4 )

fish_lengthIJ<-read.csv("odata/TI_ImageJ_screenshots/TI_fishmeasurements_nonstereo.csv", header = TRUE)

#############################################################################
#check relationship between EventMeasure and ImageJ fish length measurements

#add zeros to start of Localization ID
fish_lengthIJ$fishID<-with_options(
  c(scipen = 999), 
  str_pad(fish_lengthIJ$fishID, 4, pad = "0")
)

#create fishID column and average 2 length measurements
fish_lengthIJD<-fish_lengthIJ%>%
  filter(grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  unite(fishID, fishID, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_lengthIJ = mean(Length, na.rm = TRUE),
            sd_lengthIJ = sd(Length, na.rm = TRUE)) %>%
  ungroup()

#add zeros to start of Localization ID
fish_length$Selection<-with_options(
  c(scipen = 999), 
  str_pad(fish_length$Selection, 4, pad = "0")
)

###do the same for EventMeasure data###
fish_lengthEMD<-fish_length%>%
  dplyr::select(-c(3:5,7:28, 33))%>% #get rid of unnecessary columns
  filter(grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  separate(Filename, into = c("vidnum","CamName", "Date"), sep = "_", remove = FALSE)%>%
  separate(Date, into = c("Date"), sep = "T")%>%
  rename(Length =Length..mm.)%>%
  unite(fishID, Selection, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_lengthEM = mean(Length, na.rm = TRUE),
            sd_lengthEM = sd(Length, na.rm = TRUE)) %>%
  ungroup()

#join EM and IJ duplicate measure dataframes
dup_meas<-left_join(fish_lengthEMD, fish_lengthIJD, by = "fishID")

#subract EM lengths from IJ lengths and calculate mean and SD difference
dup_meas$difference<- dup_meas$mean_lengthEM-dup_meas$mean_lengthIJ

mean(dup_meas$difference)
sd(dup_meas$difference)
#EventMeasure lengths are an average of 17mm longer than ImageJ measurements with a standard deviation of 48mm. 
#Strong siginificant positive correlation between measurements (rho =0.7, p=0.02)

#Pearsons correlation between measurements
measure_cor<-cor.test(dup_meas$mean_lengthEM, dup_meas$mean_lengthIJ,  method="pearson") #exact = FALSE gets rid of impact of tied values on ranking
print(measure_cor)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

# 
measurements <- ggplot(dup_meas, aes(x = mean_lengthEM, y = mean_lengthIJ)) +
  geom_point(color = "deepskyblue") +
  labs(title = "a)") +
  sm_statCorr(corr_method="pearson", color = "deepskyblue3", linetype = "dashed")+
  labs(x = "EM", y = "IJ")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
measurements 

#############################################################################################
###create new dataframe with fish measurements joined to sound data

#create fishID column and average 2 length measurements
fish_lengthIJ1<-fish_lengthIJ%>%
  filter(!grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  unite(fishID, fishID, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE),
            sd_length = sd(Length, na.rm = TRUE)) %>%
  ungroup()

duplicated(fish_lengthIJ1$fishID)

###do the same for EventMeasure data###
fish_length1<-fish_length%>%
  dplyr::select(-c(3:5,7:28, 33:34))%>% #get rid of unnecessary columns
  separate(Filename, into = c("vidnum","CamName", "Date"), sep = "_", remove = FALSE)%>%
  separate(Date, into = c("Date"), sep = "T")%>%
  rename(Length =Length..mm.)

#combine fishnum (Selection) and Date
fish_length1<-fish_length1%>%
  unite(fishID, Selection, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE),
            sd_length = sd(Length, na.rm = TRUE)) %>%
  ungroup()

duplicated(fish_length1$fishID)

#join EM and ImageJ measurements
measures<- rbind(fish_length1, fish_lengthIJ1)
duplicated(measures$fishID)

#link measures to sound dataframe
SoundnVid1<-left_join(SnV_edit1, measures, by = "fishID")

#remove caurinus outlier value (change large length (lingcod measurement) to na)
SoundnVid2<-SoundnVid1%>%
  mutate(mean_length = ifelse(Species == "caurinus" & mean_length > 500, NA, mean_length))

write.csv(SoundnVid2, "wdata/Sound_Species_Behaviour_Length_20250213.csv", row.names = FALSE)

### look at fish length histogram by species

FishLength<-SoundnVid2%>%
  group_by(fishID) %>%
  slice_head(n = 1) %>%
  filter(!is.na(mean_length))%>% ## get rid of NAs in length (these fish haven't been annotated yet)
  ungroup()

#histogram of fish lengths by species (bins of 5)
ggplot(FishLength, aes(x = mean_length)) +
  geom_histogram(binwidth = 5, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Mean Length by Species", x = "Mean Length", y = "Count") +
  coord_cartesian(ylim = c(0, 8)) +  # Fix y-axis range from 0 to 8
  theme_classic()


