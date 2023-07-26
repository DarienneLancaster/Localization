source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("readr")
lp("hms")

#load buzzer drift csv
buzzerd<-read.csv("odata/Large Array buzzer times 20230726.csv", stringsAsFactors = FALSE)
head(buzzerd)
glimpse(buzzerd)
str(buzzerd)
#colnames(buzzerd)[1]<-"Deployment_ID" #rename first column, this is necessary sometimes if a spreadsheet has been opened on a Mac and there's a weird column name


#remove extra columns from buzzerd (this is useful code if you end up having extra columns in your imported dataframe)
#buzzerdrift <- buzzerd %>% 
# select(-13:-42) %>%
#filter(!is.na(Deployment_ID))

#convert FishCam buzzer time columns to time format and subtract log times from AMAR times to calculate drift
buzzerd2<-buzzerd%>%
  mutate(FC1L=as.POSIXct(paste(Date,FC1L), format="%Y-%m-%d %H:%M:%OS"),
         FC1A=as.POSIXct(paste(Date,FC1A), format="%Y-%m-%d %H:%M:%OS"),
         FC1drift=as_hms(FC1A - FC1L),
         
         FC2L=as.POSIXct(paste(Date,FC2L), format="%Y-%m-%d %H:%M:%OS"),
         FC2A=as.POSIXct(paste(Date,FC2A), format="%Y-%m-%d %H:%M:%OS"),
         FC2drift=as_hms(FC2A - FC2L),
         
         FC3L=as.POSIXct(paste(Date,FC3L), format="%Y-%m-%d %H:%M:%OS"),
         FC3A=as.POSIXct(paste(Date,FC3A), format="%Y-%m-%d %H:%M:%OS"),
         FC3drift=as_hms(FC3A - FC3L))

#Creating column with video file names and pulling out dates from filenames (e.g. 2557_FishCam03_20220823T204721.521716Z_1600x1200_awb-auto_exp-night_fr-10_q-20_sh-0_b-50_c-0_i-400_sat-0)

# create dataframe with list of all video files in folder
data_files<- data.frame(vidnames=list.files("odata/Taylor_Islet_LA_2022_FC3_Videos"))

#pull time stamp out of video file names
data_files2<- data_files %>%
  #separate file number from file name and put in column "filenum" (e.g. 2557) and remove _FishCam text completely 
  separate(vidnames, into = c("filenum","fileend"), sep = "_FishCam", remove = FALSE)%>% 
  #chop up fileend into camera number (e.g. 03), underscore, and then pull apart Year, Month,Day, Hour, Min, Sec, Millisec (sep = pulls apart at # from start of fileend string)
  separate(fileend, into = c("camnum", "ext", "y", "m", "d", "T","hr", "mn", "sc", "remaining"), sep = c(2,3,7,9,11,12,14,16,24) )%>%
  #remove unnecessary columns (underscore, T between Date/Time, rest of file name)
  select(-ext,-T,-remaining)%>%
  #format date/time columns into same format as buzzerdrift times
  mutate(vidstarttime = as.POSIXct(paste(y, m, d,hr, mn, sc), format="%Y %m %d %H %M %OS"))

second(data_files2$vidstarttime)#shows whether milliseconds are actually part of the time stamp data

#TO DO
#Create dataframe with localization info + buzzerdrift times + video file times and names
#Create new column with drift times added to video times (e.g. 20:47:21 + FC3drift)
#do interval calculation to predict which file each localization will occur in (factoring in buzzer drift times)


