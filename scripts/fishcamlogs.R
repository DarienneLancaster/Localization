####Load Packages######
source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("readr")
lp("hms")
lp("readbulk")

######
#import buzzer times from logs

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
  

