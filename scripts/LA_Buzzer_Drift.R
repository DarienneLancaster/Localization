source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("readr")
lp("hms")

#load buzzer drift csv
buzzerd<-read_csv("odata/LA_Buzzer_Drift_20230725 .csv")
head(buzzerd)

#remove extra columns from buzzerd
buzzerdrift <- buzzerd %>% 
  select(-13:-42)

str(buzzerdrift)

#add milliseconds to times


#subtract buzzer log time (L) from AMAR buzzer time (A)

buzzerdrift$FC1drift <- (buzzerdrift$FC1A - buzzerdrift$FC1L)
buzzerdrift$FC1drift<- as_hms(buzzerdrift$FC1drift)

buzzerdrift$FC2drift <- (buzzerdrift$FC2A - buzzerdrift$FC2L)
buzzerdrift$FC2drift<- as_hms(buzzerdrift$FC2drift)

buzzerdrift$FC3drift <- (buzzerdrift$FC3A - buzzerdrift$FC3L)
buzzerdrift$FC3drift<- as_hms(buzzerdrift$FC3drift)

#Add column with video file names (e.g. 2557_FishCam03_20220823T204721.521716Z_1600x1200_awb-auto_exp-night_fr-10_q-20_sh-0_b-50_c-0_i-400_sat-0)

#Create column with time from video names (e.g. 20:47:21)
#possible problem with 3 digit vs 4 digit start numbers

#Add drift times to video times (e.g. 20:47:21 + FC3drift)


