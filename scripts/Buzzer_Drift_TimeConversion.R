install.packages("lubridate")
library(lubridate)
install.packages("hms")
library(hms)
install.packages("dplyr")
library(dplyr)

setwd("C:/Users/dlanc/Documents/PhD/Supervision/Emma Toulouse Makay VIU practicum 2022 to 2023/Buzzer Drift")
getwd()

buzzerd<-read.csv("Large Array buzzer times 20230718.csv", header = TRUE)
buzzerdrift <- buzzerd %>% 
  select(-X,-X.1,-X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11,
         -X.12, -X.13, -X.14,-X.15,-X.16,-X.17,-X.18,-X.19,-X.20,-X.21,-X.22,
         -X.23,-X.24,-X.25,-X.26,-X.27,-X.28,-X.29)

str(buzzerdrift)
head(buzzerdrift)

#FishCam01 convert to time and calculate total time drift
buzzerdrift$FishCam01_Log_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam01_Log_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam01_Log_Buzzer_Time)


buzzerdrift$FishCam01_AMAR_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam01_AMAR_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam01_AMAR_Buzzer_Time)

buzzerdrift$FishCam01_Drift <- (buzzerdrift$FishCam01_AMAR_Buzzer_Time - buzzerdrift$FishCam01_Log_Buzzer_Time)
buzzerdrift$FishCam01_Drift<- as_hms(buzzerdrift$FishCam01_Drift)

#FishCam02 convert to time and calculate total time drift
buzzerdrift$FishCam02_Log_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam02_Log_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam02_Log_Buzzer_Time)


buzzerdrift$FishCam02_AMAR_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam02_AMAR_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam02_AMAR_Buzzer_Time)

buzzerdrift$FishCam02_Drift <- (buzzerdrift$FishCam02_AMAR_Buzzer_Time - buzzerdrift$FishCam02_Log_Buzzer_Time)
buzzerdrift$FishCam02_Drift<- as_hms(buzzerdrift$FishCam02_Drift)

#FishCam03 convert to time and calculate total time drift
buzzerdrift$FishCam03_Log_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam03_Log_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam03_Log_Buzzer_Time)


buzzerdrift$FishCam03_AMAR_Buzzer_Time<-as.POSIXct(buzzerdrift$FishCam03_AMAR_Buzzer_Time, format= "%H:%M:%OS")
class(buzzerdrift$FishCam03_AMAR_Buzzer_Time)

buzzerdrift$FishCam03_Drift <- (buzzerdrift$FishCam03_AMAR_Buzzer_Time - buzzerdrift$FishCam03_Log_Buzzer_Time)
buzzerdrift$FishCam03_Drift<- as_hms(buzzerdrift$FishCam03_Drift)
