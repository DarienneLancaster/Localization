####Load Packages######
source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("readr")
lp("hms")
lp("readbulk")
lp("readxl")

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
  select(-c(8:14))%>%
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
filtlocs<-"TestFSLocs" #change folder name here

FSlocs<-imp_raven(path = paste0("wdata/", filtlocs), all.data =  TRUE)
FSlocs<-FSlocs%>%
  select(-c(37))%>% #a weird additional column was added at the end so needed to remove (may not always need this
  filter(f=="FS")%>%#filter to only keep files labelled as fish sound (FS)
  rename("Time" = "Begin Clock Time")

str(FSlocs)

#import edited driftmean csv
driftedit<-read.csv("odata/driftmean_missingFC2added.csv", header = TRUE)
str(driftedit)

#change date column to match format of localization raven tables.
driftedit2<- driftedit%>%
  mutate(across("Date", str_replace, "2022-08-", "2022/8/"))%>%
  mutate(across("Date", str_replace, "2022-09-", "2022/8/"))%>%
  rename("Begin Date" = "Date")
  


str(driftedit2)

#join fish sound localization files to buzzerdrift times (this will throw a many to many error, this is good and because of
#the different values for each of the 3 cameras.  There should be a line for each localization tied to each camera)
locswdrift<-left_join(FSlocs, driftedit2, by = "Begin Date")

#convert meanDrift and Time columns to time
locswdrift$meanDrift<- as.POSIXct(locswdrift$meanDrift, format= "%H:%M:%OS")
locswdrift$Time<- as.POSIXct(locswdrift$Time, format= "%H:%M:%OS")
#subtract drift time from localization to get adjusted time of fish calls.
locswdrift$locadjust<-as_hms(locswdrift$Time - locswdrift$meanDrift)

  







