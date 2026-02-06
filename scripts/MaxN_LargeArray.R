#Large Array MaxN hourly annotations (6am to 8pm - 5 min after 8pm)

#packages

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



#to do
#group by file name, genus, species, frame number (this should give MaxN per species for each file)
#calculate aggregated MaxN for all fish (e.g. maxn of coppers = 3, maxn of quillback = 1, total fish max n for that file = 4)
#calculate mean hourly MaxN (show error bars - give n of each hour to account for some days that may be missing)
#calculate mean daily MaxN
#calculate mean MaxN for full deployment and just simple MaxN for each species (not averaged)

#plot hourly mean MaxN as boxplots

#TAYLOR ISLET Hourly MaxN


Max_TI<-read.csv("odata/Sunrise_Sunset_Files/EventMeasure_SunriseSunsetVideoAnnotations/TI/VideoAnnotationMaxN_TI_Point measurements_20260201.csv", header = TRUE, skip = 4 )

#calculate MaxN per file for each species
Max_TI2 <- Max_TI %>%
  filter(!is.na(Genus), Genus != "") %>%          # remove blank Genus
  group_by(Filename, Genus, Species, Frame) %>%
  summarise(
    MaxN = sum(Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Filename, Genus, Species) %>%
  slice_max(MaxN, n = 1, with_ties = FALSE) %>%   # keep max per combo
  ungroup()

#pull out timestamp data from filenames and create a new column for BC time hours 
str(Max_TI2)


Max_TI3 <- Max_TI2 %>%
  mutate(
    Species = if_else(is.na(Species) | Species == "", "spp", Species),
    latin = paste(Genus, Species)
  ) %>%
  group_by(Filename, latin) %>%
  # Rename the species before summarising
  mutate(
    latin = if_else(latin == "Sebastes flavidus", "Sebastes melanops", latin)
  ) %>%
  summarise(
    MaxN = sum(MaxN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Filename,
    latin,
    fill = list(MaxN = 0)
  )%>%
  mutate(
    hour = as.integer(str_extract(Filename, "(?<=T)\\d{2}")),
    BC_hour = (hour - 7) %% 24)

# Add common names

Max_TI3$Common<- ifelse(Max_TI3$latin == "Sebastes caurinus", "Copper Rockfish",
                         ifelse(Max_TI3$latin == "Sebastes maliger", "Quillback Rockfish",
                                ifelse(Max_TI3$latin == "Sebastes pinniger", "Canary Rockfish",
                                       ifelse(Max_TI3$latin == "Sebastes miniatus", "Vermillion Rockfish",
                                              ifelse(Max_TI3$latin == "Sebastes melanops", "Black Rockfish",
                                                     ifelse(Max_TI3$latin == "Ophiodon elongatus", "Lingcod",
                                                            ifelse(Max_TI3$latin == "Rhinogobiops nicholsii", "Black Eyed Gobie",
                                                                   ifelse(Max_TI3$latin == "Scorpaenichthys marmoratus", "Cabezon",
                                                                          ifelse(Max_TI3$latin == "Platichthys stellatus", "Flatfish spp",
                                                                                 ifelse(Max_TI3$latin == "Oxylebius pictus", "Painted Greenling",
                                                            ifelse(Max_TI3$latin == "Hexagrammos decagrammus", "Kelp Greenling",
                                                                   ifelse(Max_TI3$latin == "Rhacochilus vacca", "Pile Perch", "Sebastes spp"))))))))))))

#calculate mean MaxN per hour for each species
meanMaxN_TI <- Max_TI3 %>%
  group_by(Common, BC_hour) %>%
  summarise(
    mean_MaxN = mean(MaxN, na.rm = TRUE),
    sd_MaxN   = sd(MaxN, na.rm = TRUE),
    .groups = "drop"
  )

#filter outliers (cabezon and non-soniferous fish pictus, gobies, remove unidentifies sebastes spp.)

meanMaxN_TI_RRF<-meanMaxN_TI%>%
  filter(Common != "Flatfish spp",Common != "Cabezon",Common != "other",Common != "Black Eyed Gobie")

#plot mean MaxN per species

# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
  "Vermillion Rockfish" = "#FF3333",
  "Flatfish spp" = "#99FFFF",
  "Pile Perch" = "#9900CC",
  "Painted Greenling" = "#990000",
  "Black Eyed Gobie" = "#CCCCCC",
  "Cabezon" = "#663300",
  "Kelp Greenling" = "#CCCC00",
  "Sebastes spp" = "black"
)


##
meanMaxN <- ggplot(meanMaxN_TI_RRF, 
                   aes(x = BC_hour, y = mean_MaxN, fill = Common)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(mean_MaxN - sd_MaxN, 0),
      ymax = mean_MaxN + sd_MaxN
    ),
    width = 0.2
  ) +
  facet_wrap(~ Common, scales = "fixed") +
  scale_x_continuous(breaks = 6:20) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Hour",
    y = "Mean MaxN",
    title = "Mean MaxN by Hour for soniferous species",
    fill = "Species"
  ) +
  theme_classic() +
  theme(legend.position = "none")

meanMaxN
ggsave("figures/CH3/MaxN_meanHourly_TI.png", plot = meanMaxN, width = 10, height = 10, dpi = 300)


#Basic MaxN by species (not averaged across full deployment)

BasicMaxN_TI <- Max_TI3 %>%
  group_by(Common) %>%
  slice_max(MaxN, n = 1, with_ties = FALSE) %>%
  ungroup()

#bar plot maxN all species
BasicMaxN <- ggplot(BasicMaxN_TI, aes(x = Common, y = MaxN, fill = Common)) +
  geom_col(color = "black") +   # adds black border
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Species",
    y = "MaxN",
    title = "MaxN full deployment - Taylor Islet"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

BasicMaxN
ggsave("figures/CH3/MaxN_FullDeployment_TI.png", plot = BasicMaxN, width = 10, height = 10, dpi = 300)


########################
#calculate mean MaxN per species across full deployment

mmfull_TI <- Max_TI3 %>%
  group_by(Common) %>%
  summarise(
    n         = sum(!is.na(MaxN)),     # number of values used
    mean_MaxN = mean(MaxN, na.rm = TRUE),
    sd_MaxN   = sd(MaxN, na.rm = TRUE),
    .groups = "drop"
  )

#plot mean MaxN per species

mmfull_TIbar <- ggplot(mmfull_TI, 
                       aes(x = Common, y = mean_MaxN, fill = Common)) +
  geom_col(color = "black") +
  geom_errorbar(
    aes(
      ymin = pmax(mean_MaxN - sd_MaxN, 0),
      ymax = mean_MaxN + sd_MaxN
    ),
    width = 0.2
  ) +
  scale_y_continuous(limits = c(0, 5.5)) +
  # geom_text(                        #this code is good for plotting n values if they are not the same across sites (this site n= 138 (~14 files a day for 10 days))
  #   aes(label = paste0("n = ", n)),
  #   vjust = -0.5,
  #   size = 3.5
  # ) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "
    ",
    y = "Mean MaxN",
    title = "Taylor Islet"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold")
  )

mmfull_TIbar

ggsave("figures/CH3/MaxN_meanFullDeployment_TI.png", plot = mmfull_TIbar, width = 10, height = 10, dpi = 300)

##

##################################################
#Calculate time to first detection of each species



TI_time <- Max_TI %>%
  mutate(
    DateTime = Filename %>%
      str_extract("\\d{8}T\\d{6}") %>%
      str_replace("T", "") %>%
      as.POSIXct(format = "%Y%m%d%H%M%S", tz = "UTC")
  ) %>% 
  mutate(
    latin = paste(Genus, Species)
  ) %>%
  mutate(
    Species = if_else(Species == "flavidus", "melanops", Species)
  )%>%
  mutate(
    latin = if_else(latin == "Sebastes flavidus", "Sebastes melanops", latin)
  )%>%
  arrange(DateTime) %>%
  mutate(
    seconds = Frame / 10,
    Det_Time = DateTime + seconds
  ) %>%
  filter(X.8 != "No_L") %>%
  mutate(
    vid_time = 0 + 300 * (dense_rank(DateTime) - 1)
  )%>%
  mutate(mins_to_detect = (seconds+vid_time)/60)%>%
  filter(!is.na(Genus), Genus != "")   %>% # remove blank Genus
  filter(!is.na(Species), Species != "")   %>%         
  group_by(Species) %>%          # group by species
  slice(1) %>%                   # keep first occurrence
  ungroup()%>%
  arrange(DateTime)%>%
  mutate(hours_to_detect = mins_to_detect/60)

TI_time$Common<- ifelse(TI_time$latin == "Sebastes caurinus", "Copper Rockfish",
                        ifelse(TI_time$latin == "Sebastes maliger", "Quillback Rockfish",
                               ifelse(TI_time$latin == "Sebastes pinniger", "Canary Rockfish",
                                      ifelse(TI_time$latin == "Sebastes miniatus", "Vermillion Rockfish",
                                             ifelse(TI_time$latin == "Sebastes melanops", "Black Rockfish",
                                                    ifelse(TI_time$latin == "Sebastes flavidus", "Black Rockfish",
                                                    ifelse(TI_time$latin == "Ophiodon elongatus", "Lingcod",
                                                           ifelse(TI_time$latin == "Rhinogobiops nicholsii", "Black Eyed Gobie",
                                                                  ifelse(TI_time$latin == "Scorpaenichthys marmoratus", "Cabezon",
                                                                         ifelse(TI_time$latin == "Platichthys stellatus", "Flatfish spp",
                                                                                ifelse(TI_time$latin == "Oxylebius pictus", "Painted Greenling",
                                                                                       ifelse(TI_time$latin == "Hexagrammos decagrammus", "Kelp Greenling",
                                                                                              ifelse(TI_time$latin == "Rhacochilus vacca", "Pile Perch", "other")))))))))))))


Det_time_TI <- TI_time %>%
  group_by(Common) %>%
  summarise(mean_mins = mean(mins_to_detect, na.rm = TRUE)) %>%
  arrange(mean_mins) %>%                       # order by mean_mins
  mutate(Common = factor(Common, levels = Common)) %>%  # fix order
  ggplot(aes(x = Common, y = mean_mins, fill = Common)) +
  geom_col(color = "black") +
  scale_fill_manual(values = custom_colors) +
  theme_bw() +
  labs(
    x = "Common",
    y = "Mean Minutes to Detect",
    title = "Detection Time by Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  theme_classic()

Det_time_TI

ggsave("figures/CH3/TimetoDetection_TI.png", plot = Det_time_TI, width = 10, height = 10, dpi = 300)

#DANGER ROCKS

Max_DR<-read.csv("odata/Sunrise_Sunset_Files/EventMeasure_SunriseSunsetVideoAnnotations/VideoAnnotationMaxN_DR_Point measurements_20260205.csv", header = TRUE, skip = 4 )

#calculate MaxN per file for each species
Max_DR2 <- Max_DR %>%
  filter(!is.na(Genus), Genus != "") %>%          # remove blank Genus
  group_by(Filename, Genus, Species, Frame) %>%
  summarise(
    MaxN = sum(Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Filename, Genus, Species) %>%
  slice_max(MaxN, n = 1, with_ties = FALSE) %>%   # keep max per combo
  ungroup()

#pull out timestamp data from filenames and create a new column for BC time hours 
str(Max_DR2)


Max_DR3 <- Max_DR2 %>%
  mutate(
    Species = if_else(is.na(Species) | Species == "", "spp", Species),
    latin = paste(Genus, Species)
  ) %>%
  group_by(Filename, latin) %>%
  # Rename the species before summarising
  mutate(
    latin = if_else(latin == "Sebastes flavidus", "Sebastes melanops", latin)
  ) %>%
  summarise(
    MaxN = sum(MaxN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Filename,
    latin,
    fill = list(MaxN = 0)
  )%>%
  mutate(
    hour = as.integer(str_extract(Filename, "(?<=T)\\d{2}")),
    BC_hour = (hour - 7) %% 24)

# Add common names

Max_DR3$Common<- ifelse(Max_DR3$latin == "Sebastes caurinus", "Copper Rockfish",
                        ifelse(Max_DR3$latin == "Sebastes maliger", "Quillback Rockfish",
                               ifelse(Max_DR3$latin == "Sebastes pinniger", "Canary Rockfish",
                                      ifelse(Max_DR3$latin == "Sebastes miniatus", "Vermillion Rockfish",
                                             ifelse(Max_DR3$latin == "Sebastes melanops", "Black Rockfish",
                                                    ifelse(Max_DR3$latin == "Ophiodon elongatus", "Lingcod",
                                                           ifelse(Max_DR3$latin == "Rhinogobiops nicholsii", "Black Eyed Gobie",
                                                                  ifelse(Max_DR3$latin == "Scorpaenichthys marmoratus", "Cabezon",
                                                                         ifelse(Max_DR3$latin == "Platichthys stellatus", "Flatfish spp",
                                                                                ifelse(Max_DR3$latin == "Oxylebius pictus", "Painted Greenling",
                                                                                       ifelse(Max_DR3$latin == "Hexagrammos decagrammus", "Kelp Greenling",
                                                                                              ifelse(Max_DR3$latin == "Rhacochilus vacca", "Pile Perch", "Sebastes spp"))))))))))))

#calculate mean MaxN per hour for each species

meanMaxN_DR <- Max_DR3 %>%
  group_by(Common, BC_hour) %>%
  summarise(
    mean_MaxN = mean(MaxN, na.rm = TRUE),
    sd_MaxN   = sd(MaxN, na.rm = TRUE),
    .groups = "drop"
  )

#filter outliers (cabezon and non-soniferous fish pictus, gobies, remove unidentifies sebastes spp.)

meanMaxN_DR_RRF<-meanMaxN_DR%>%
  filter(Common != "Flatfish spp",Common != "Cabezon",Common != "other",Common != "Black Eyed Gobie")

#plot mean MaxN per species

# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
  "Vermillion Rockfish" = "#FF3333",
  "Flatfish spp" = "#99FFFF",
  "Pile Perch" = "#9900CC",
  "Painted Greenling" = "#990000",
  "Black Eyed Gobie" = "#CCCCCC",
  "Cabezon" = "#663300",
  "Kelp Greenling" = "#CCCC00",
  "Sebastes spp" = "black"
)

##
meanMaxN <- ggplot(meanMaxN_DR_RRF, 
                   aes(x = BC_hour, y = mean_MaxN, fill = Common)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = pmax(mean_MaxN - sd_MaxN, 0),
      ymax = mean_MaxN + sd_MaxN
    ),
    width = 0.2
  ) +
  facet_wrap(~ Common, scales = "fixed") +
  scale_x_continuous(breaks = 6:20) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Hour",
    y = "Mean MaxN",
    title = "Mean MaxN by Hour for soniferous species",
    fill = "Species"
  ) +
  theme_classic() +
  theme(legend.position = "none")

meanMaxN

ggsave("figures/CH3/MaxN_meanHourly_DR.png", plot = meanMaxN, width = 10, height = 10, dpi = 300)


#Basic MaxN by species (not averaged across full deployment)

BasicMaxN_DR <- Max_DR3 %>%
  group_by(Common) %>%
  slice_max(MaxN, n = 1, with_ties = FALSE) %>%
  ungroup()

#bar plot maxN all species
BasicMaxN <- ggplot(BasicMaxN_DR, aes(x = Common, y = MaxN, fill = Common)) +
  geom_col(color = "black") +   # adds black border
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Species",
    y = "MaxN",
    title = "MaxN full deployment - Taylor Islet"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

BasicMaxN

ggsave("figures/CH3/MaxN_FullDeployment_DR.png", plot = BasicMaxN, width = 10, height = 10, dpi = 300)

########################
#calculate mean MaxN per species across full deployment

mmfull_DR <- Max_DR3 %>%
  group_by(Common) %>%
  summarise(
    n         = sum(!is.na(MaxN)),     # number of values used
    mean_MaxN = mean(MaxN, na.rm = TRUE),
    sd_MaxN   = sd(MaxN, na.rm = TRUE),
    .groups = "drop"
  )

#plot mean MaxN per species
mmfull_DRbar <- ggplot(mmfull_DR, 
                       aes(x = Common, y = mean_MaxN, fill = Common)) +
  geom_col(color = "black") +
  geom_errorbar(
    aes(
      ymin = pmax(mean_MaxN - sd_MaxN, 0),
      ymax = mean_MaxN + sd_MaxN
    ),
    width = 0.2
  ) +
  scale_y_continuous(limits = c(0, 5.5)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "
    ",
    y = "Mean MaxN",
    title = "Danger Rocks"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold")
  )

mmfull_DRbar

ggsave("figures/CH3/MaxN_meanFullDeployment_DR.png", plot = mmfull_DRbar, width = 10, height = 10, dpi = 300)

lp("patchwork")

mmfull<-mmfull_TIbar+ mmfull_DRbar 
mmfull

ggsave("figures/CH3/MaxN_meanFullDeployment_bothsites.png", plot = mmfull, width = 10, height = 10, dpi = 300)

##################################################
#Calculate time to first detection of each species


DR_time <- Max_DR %>%
  mutate(
    DateTime = Filename %>%
      str_extract("\\d{8}T\\d{6}") %>%
      str_replace("T", "") %>%
      as.POSIXct(format = "%Y%m%d%H%M%S", tz = "UTC")
  ) %>% 
  mutate(
    latin = paste(Genus, Species)
  ) %>%
  mutate(
    Species = if_else(Species == "flavidus", "melanops", Species)
  )%>%
  arrange(DateTime) %>%
  mutate(
    seconds = Frame / 10,
    Det_Time = DateTime + seconds
  ) %>%
  filter(X.8 != "No_L") %>%
  mutate(
    vid_time = 0 + 300 * (dense_rank(DateTime) - 1)
  )%>%
  mutate(mins_to_detect = (seconds+vid_time)/60)%>%
  filter(!is.na(Genus), Genus != "")   %>% # remove blank Genus
  filter(!is.na(Species), Species != "")   %>%         
  group_by(Species) %>%          # group by species
  slice(1) %>%                   # keep first occurrence
  ungroup()%>%
  arrange(DateTime)%>%
  mutate(hours_to_detect = mins_to_detect/60)

DR_time$Common<- ifelse(DR_time$latin == "Sebastes caurinus", "Copper Rockfish",
                        ifelse(DR_time$latin == "Sebastes maliger", "Quillback Rockfish",
                               ifelse(DR_time$latin == "Sebastes pinniger", "Canary Rockfish",
                                      ifelse(DR_time$latin == "Sebastes miniatus", "Vermillion Rockfish",
                                             ifelse(DR_time$latin == "Sebastes melanops", "Black Rockfish",
                                                    ifelse(DR_time$latin == "Ophiodon elongatus", "Lingcod",
                                                           ifelse(DR_time$latin == "Rhinogobiops nicholsii", "Black Eyed Gobie",
                                                                  ifelse(DR_time$latin == "Scorpaenichthys marmoratus", "Cabezon",
                                                                         ifelse(DR_time$latin == "Platichthys stellatus", "Flatfish spp",
                                                                                ifelse(DR_time$latin == "Oxylebius pictus", "Painted Greenling",
                                                                                       ifelse(DR_time$latin == "Hexagrammos decagrammus", "Kelp Greenling",
                                                                                              ifelse(DR_time$latin == "Rhacochilus vacca", "Pile Perch", "other"))))))))))))


Det_time_DR <- DR_time %>%
  group_by(Common) %>%
  summarise(mean_mins = mean(mins_to_detect, na.rm = TRUE)) %>%
  arrange(mean_mins) %>%                       # order by mean_mins
  mutate(Common = factor(Common, levels = Common)) %>%  # fix order
  ggplot(aes(x = Common, y = mean_mins, fill = Common)) +
  geom_col(color = "black") +
  scale_fill_manual(values = custom_colors) +
  theme_bw() +
  labs(
    x = "Common",
    y = "Mean Minutes to Detect",
    title = "Detection Time by Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  theme_classic()

Det_time_DR

ggsave("figures/CH3/TimetoDetection_DR.png", plot = Det_time_DR, width = 10, height = 10, dpi = 300)

############################################################################################################################

#Pull in TI and DR 1 min SPL

SPL_TI<-read.csv("odata/SPL_LargeArray/SPL_1min_fulldeployment_TI_20to1000Hz.csv", header = TRUE)
str(SPL_TI)

SPL_TI <- SPL_TI %>%
  mutate(
    Date = as.POSIXct(Date, tz = "UTC"),   # convert to POSIXct
    BC_time = Date - hours(7)               # subtract 7 hours
  )

SPL_TI2 <- SPL_TI %>%
  mutate(
    Date = as.POSIXct(Date, tz = "UTC"),
    BC_time = Date - hours(7)
  ) %>%
  filter(
    (hour(BC_time) > 6 | (hour(BC_time) == 6 & minute(BC_time) >= 0)) &
      (hour(BC_time) < 20 | (hour(BC_time) == 20 & minute(BC_time) <= 5)))
# there are 9156 minutes of audio between sunrise and sunset for taylor islet
# there are 4872 minutes with an SPL <= 113dB, so %53 of the minutes are useable for monitoring rockfish sounds. 

#####################
#danger Rocks

SPL_DR<-read.csv("odata/SPL_LargeArray/SPL_1min_fulldeployment_DR_20to1000Hz.csv", header = TRUE)
str(SPL_DR)

SPL_DR <- SPL_DR %>%
  mutate(
    Date = as.POSIXct(Date, tz = "UTC"),   # convert to POSIXct
    BC_time = Date - hours(7)               # subtract 7 hours
  )

SPL_DR2 <- SPL_DR %>%
  mutate(
    Date = as.POSIXct(Date, tz = "UTC"),
    BC_time = Date - hours(7)
  ) %>%
  filter(
    (hour(BC_time) > 6 | (hour(BC_time) == 6 & minute(BC_time) >= 0)) &
      (hour(BC_time) < 20 | (hour(BC_time) == 20 & minute(BC_time) <= 5)))

SPL_LA<-rbind(SPL_TI2, SPL_DR2)
str(SPL_LA)

# there are 6648 minutes of audio between sunrise and sunset for taylor islet
# there are 5585 minutes with an SPL <= 113dB, so %84 of the minutes are useable for monitoring rockfish sounds. 


###########
#bring in fish sound data. Use only sounds identified to species (can later check the total fish sounds identified in the video?)

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

str(FS_dat)

### join fish sounds to SPL values
lp("data.table")

# Convert to data.table
setDT(FS_dat)
setDT(SPL_LA)

# Ensure POSIXct (important!)
FS_dat[, Time_of_FS := as.POSIXct(Time_of_FS)]
SPL_LA[, BC_time := as.POSIXct(BC_time)]

# Set keys
setkey(FS_dat, Time_of_FS)
setkey(SPL_LA, BC_time)

# Nearest-time join
FS_joined <- SPL_LA[
  FS_dat,
  on = .(BC_time = Time_of_FS),
  roll = "nearest"
]

FS_joined1<-FS_joined%>%
  select(-c(5,6,36:85))

SPL_summary <- FS_joined1 %>%
  summarise(
    min_SPL   = min(SPL, na.rm = TRUE),
    q25_SPL   = quantile(SPL, 0.25, na.rm = TRUE),
    mean_SPL  = mean(SPL, na.rm = TRUE),
    q75_SPL   = quantile(SPL, 0.75, na.rm = TRUE),
    max_SPL   = max(SPL, na.rm = TRUE)
  )

SPL_summary_spec <-FS_joined1 %>%
  group_by(Site,Species) %>%
  summarise(
    min_SPL   = min(SPL, na.rm = TRUE),
    q25_SPL   = quantile(SPL, 0.25, na.rm = TRUE),
    mean_SPL  = mean(SPL, na.rm = TRUE),
    q75_SPL   = quantile(SPL, 0.75, na.rm = TRUE),
    max_SPL   = max(SPL, na.rm = TRUE)
  )

##calculate time of first detection of each species at each site

FS_first_detect <- FS_joined1 %>%
  group_by(Site, Species) %>%
  slice_min(BC_time, n = 1, with_ties = FALSE) %>%
  ungroup()

#now need to calculate how many minutes had SPLs below 113dB between time of first recording and time of detection. 

SPL_LA_filt<-SPL_LA%>%
  filter(SPL <= 113)%>%
  arrange(Site_name, BC_time)%>%
  group_by(Site_name)%>%
  mutate(minute = row_number()-1)%>%
  ungroup()

#match first fish sound for each species to time stamp with correct minutes value (total minutes of monitoring time with SPL <113dB)
dt1 <- as.data.table(FS_first_detect)
dt2 <- as.data.table(SPL_LA_filt)

setkey(dt2, BC_time)   # key on SPL_LA_filt

# join dt1 to dt2 using nearest timestamp from dt2
FS_detect <- dt2[dt1, on = "BC_time", roll = "nearest"]

FS_detect$hours_detect<-FS_detect$minute/60

#################
#compare minutes to detection with video vs. acoustics

Vid_time<-rbind(DR_time, TI_time)

Vid_time<-Vid_time%>%
  rename(Site = OpCode)%>%
  rename(hours_detect = hours_to_detect)

#add common names to acoustic data

FS_detect <- FS_detect %>%
  mutate(latin = paste(Genus, Species))%>%
  filter(Species != "")

FS_detect$Common<- ifelse(FS_detect$latin == "Sebastes caurinus", "Copper Rockfish",
                        ifelse(FS_detect$latin == "Sebastes maliger", "Quillback Rockfish",
                               ifelse(FS_detect$latin == "Sebastes pinniger", "Canary Rockfish",
                                      ifelse(FS_detect$latin == "Sebastes miniatus", "Vermillion Rockfish",
                                             ifelse(FS_detect$latin == "Sebastes melanops", "Black Rockfish",
                                                    ifelse(FS_detect$latin == "Ophiodon elongatus", "Lingcod",
                                                           ifelse(FS_detect$latin == "Rhinogobiops nicholsii", "Black Eyed Gobie",
                                                                  ifelse(FS_detect$latin == "Scorpaenichthys marmoratus", "Cabezon",
                                                                         ifelse(FS_detect$latin == "Platichthys stellatus", "Flatfish spp",
                                                                                ifelse(FS_detect$latin == "Oxylebius pictus", "Painted Greenling",
                                                                                       ifelse(FS_detect$latin == "Hexagrammos decagrammus", "Kelp Greenling",
                                                                                              ifelse(FS_detect$latin == "Rhacochilus vacca", "Pile Perch", "other"))))))))))))

FS_detect1 <- FS_detect %>%
  select(Site, Common, hours_detect)%>%
  mutate(Method  = "Acoustics")

Vid_time1 <- Vid_time %>%
  select(Site, Common, hours_detect)%>%
  mutate(Method  = "Video")

Method_detect<-rbind(FS_detect1, Vid_time1)

Method_detect_TI<-Method_detect%>%
  filter(Site == "Taylor Islet")%>%
  complete(
    Common,
    Method,
    fill = list(hours_detect = 0))

Method_detect_DR<-Method_detect%>%
  filter(Site == "Danger Rocks")%>%
  complete(
    Common,
    Method,
    fill = list(hours_detect = 0))

Detect_TI<-ggplot(Method_detect_TI, aes(x = Common, y = hours_detect, fill = Method)) +
  geom_col(position = "dodge") +
  labs(
    x = "Common Species",
    y = "Hours Detect",
    title = "Detection Hours by Method"
  ) +
  theme_classic()
Detect_TI

lp("ggpattern")
library(ggpattern)

Detect_TI <- ggplot(Method_detect_TI, aes(x = Common, y = hours_detect, fill = Common, alpha = Method)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = custom_colors) +
  scale_alpha_manual(values = c("Acoustics" = 1, "Video" = 0.5)) +
  labs(
    x = "Common Species",
    y = "Hours Detect",
    title = "Detection Hours by Method"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

Detect_TI



#Calculate mean calls per hour, per day and total calls by species
