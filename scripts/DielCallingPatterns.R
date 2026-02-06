
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
lp("vegan")
lp("gridExtra")
lp("stringr")

#TAYLOR ISLET SPL

SPL_TIss<-read.csv("odata/Sunrise_Sunset_Files/TI_sunrise_sunset/SPL_TIss_20to1000Hz.csv")

str(SPL_TIss)

#subtract 7 hours from time to change UTC time to BC time (PST)
#pull date and time into separate columns


SPL_TIss2 <- SPL_TIss %>%
  mutate(
    datetime = ymd_hms(`Date`) - hours(7)
  ) %>%
  filter(
    hour(datetime) >= 6,
    hour(datetime) < 20
  )%>%
  mutate(
    date = as.Date(datetime),
    time_BC = format(datetime, "%H:%M:%S")
  )

#Count number of minutes where SPL is >106

#histogram
hist(SPL_TIss2$SPL,
     main = "Histogram of SPL Values",
     xlab = "SPL",
     ylab = "Frequency",
     breaks = 30)

#above and below 106 dB
SPL_TIss2 %>%
  summarise(
    total = n(),
    above_106 = sum(SPL > 106, na.rm = TRUE),
    below_or_equal_106 = sum(SPL <= 106, na.rm = TRUE),
    pct_above_106 = above_106 / total * 100,
    pct_below_or_equal_106 = below_or_equal_106 / total * 100
  )

#above and below 110 dB
SPL_TIss2 %>%
  summarise(
    total = n(),
    above_110 = sum(SPL > 110, na.rm = TRUE),
    below_or_equal_110 = sum(SPL <= 110, na.rm = TRUE),
    pct_above_110 = above_110 / total * 100,
    pct_below_or_equal_110 = below_or_equal_110 / total * 100
  )


#Danger Rocks SPL

SPL_DRss<-read.csv("odata/Sunrise_Sunset_Files/DR_sunrise_sunset/SPL_DRss_20to1000Hz.csv")

str(SPL_DRss)

#subtract 7 hours from time to change UTC time to BC time (PST)
#pull date and time into separate columns


SPL_DRss2 <- SPL_DRss %>%
  mutate(
    datetime = ymd_hms(`Date`) - hours(7)
  ) %>%
  filter(
    hour(datetime) >= 6,
    hour(datetime) < 20
  )%>%
  mutate(
    date = as.Date(datetime),
    time_BC = format(datetime, "%H:%M:%S")
  )

#Count number of minutes where SPL is >106

#histogram
hist(SPL_DRss2$SPL,
     main = "Histogram of SPL Values",
     xlab = "SPL",
     ylab = "Frequency",
     breaks = 30)

#above and below 106 dB
SPL_DRss2 %>%
  summarise(
    total = n(),
    above_106 = sum(SPL > 106, na.rm = TRUE),
    below_or_equal_106 = sum(SPL <= 106, na.rm = TRUE),
    pct_above_106 = above_106 / total * 100,
    pct_below_or_equal_106 = below_or_equal_106 / total * 100
  )

#above and below 110 dB
SPL_DRss2 %>%
  summarise(
    total = n(),
    above_110 = sum(SPL > 110, na.rm = TRUE),
    below_or_equal_110 = sum(SPL <= 110, na.rm = TRUE),
    pct_above_110 = above_110 / total * 100,
    pct_below_or_equal_110 = below_or_equal_110 / total * 100
  )

#combine DR and TI dataframe

SPL_all<- rbind(SPL_TIss2, SPL_DRss2)

#create column that rounds to the nearest minute
SPL_all1<-SPL_all%>%
  mutate(
    Time_rounded = round_date(datetime, unit = "minute")
  )

# check calls per minute from localization data. (use successfully localized data or original localization data that has all the FishSounds?)

FS_dat<- read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250616.csv")

#convert time into BC - pacific standard time from UTC
#add time from file name to actual time of fish sound within file to determine precise time fish sound was made

FS_dat <- FS_dat %>%
  mutate(
    datetime = ymd_hms(
      str_replace(
        str_extract(Begin.File, "\\d{8}T\\d{6}"),
        "T",
        " "
      )
    ) - hours(7),
    
    Time_of_FS = datetime + seconds(Begin.Time..s.),
    
    Date_BC = as.Date(Time_of_FS),                          # date only
    Time_of_FS_BC = format(Time_of_FS, "%H:%M:%S")          # time only
  )

#Test- filter to only keep files from Aug 13 and Sept 9
FS_dat_sub <- FS_dat %>%
  filter(Date_BC %in% as.Date(c("2022-08-13", "2022-09-09"))) %>%
  select(Date_BC, Time_of_FS_BC, Time_of_FS, s, Genus, Species) %>%
  mutate(
    Time_rounded = round_date(Time_of_FS, unit = "minute")
  )

#join SPL to FS data

SPL_FS<-left_join(SPL_all1, FS_dat_sub, by = "Time_rounded" )

#up next - how to visualize number of fish sounds occuring at different SPL levels. 


SPL_FS1<- SPL_FS%>%
  filter(!is.na(Date_BC), s == "f")

max(SPL_FS1$SPL)
min(SPL_FS1$SPL)
mean(SPL_FS1$SPL)
sd(SPL_FS1$SPL)
which.max(SPL_FS1$SPL)


