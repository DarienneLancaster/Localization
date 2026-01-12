#load in selection tables from Taylor Islet, Ohiat Island, and Danger Rocks
#One 30min file annotated from each 6hr window (e.g. midnight to 6am, 6am to 12 noon, Noon to 6pm, 6pm to midnight)
#files annotated for fish sounds, boat passage, and other animal sounds

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



########################
#Taylor Islet

TI_24hr<-imp_raven(path = "odata/24hrAnnotations_AllSites_Selection_Tables/Taylor_Islet/selections", all.data =  TRUE, only.spectro.view = FALSE)

#filter to keep only rows between 3am and noon UTM (this is 8pm and 5am - dusk to dawn)
#this should have the quietest audio files
TI_24hr$time_parsed <- as.POSIXct(
  TI_24hr$`Begin Clock Time`,
  format = "%H:%M:%OS",
  tz = "UTC"
)

TI_24hr_filtered <- TI_24hr[
  format(TI_24hr$time_parsed, "%H:%M:%S") >= "03:00:00" &
    format(TI_24hr$time_parsed, "%H:%M:%S") <= "12:00:00",
]

#summarize number of fish sounds and other sounds per sound file 
summary_totalsTI <- TI_24hr_filtered %>%
  group_by(`End File`, Class, Call_Type) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

####################################
#Danger Rocks

DR_24hr<-imp_raven(path = "odata/24hrAnnotations_AllSites_Selection_Tables/Danger_Rocks", all.data =  TRUE, only.spectro.view = FALSE)

#filter to keep only rows between 3am and noon UTM (this is 8pm and 5am - dusk to dawn)
#this should have the quietest audio files
DR_24hr$time_parsed <- as.POSIXct(
  DR_24hr$`Begin Clock Time`,
  format = "%H:%M:%OS",
  tz = "UTC"
)

DR_24hr_filtered <- DR_24hr[
  format(DR_24hr$time_parsed, "%H:%M:%S") >= "03:00:00" &
    format(DR_24hr$time_parsed, "%H:%M:%S") <= "12:00:00",
]

#summarize number of fish sounds and other sounds per sound file 
summary_totalsDR <- DR_24hr_filtered %>%
  group_by(`End File`, Class, Call_Type) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )



#Find Boat Noise
summary_BOAT_DR <- DR_24hr %>%
  group_by(`End File`, Class, Call_Type, Noise_Type) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

####################################
#Ohiat Island

OH_24hr<-imp_raven(path = "odata/24hrAnnotations_AllSites_Selection_Tables/Ohiat_Island", all.data =  TRUE, only.spectro.view = FALSE)

#filter to keep only rows between 3am and noon UTM (this is 8pm and 5am - dusk to dawn)
#this should have the quietest audio files
OH_24hr$time_parsed <- as.POSIXct(
  OH_24hr$`Begin Clock Time`,
  format = "%H:%M:%OS",
  tz = "UTC"
)

OH_24hr_filtered <- OH_24hr[
  format(OH_24hr$time_parsed, "%H:%M:%S") >= "03:00:00" &
    format(OH_24hr$time_parsed, "%H:%M:%S") <= "12:00:00",
]

#summarize number of fish sounds and other sounds per sound file 
summary_totalsOH <- OH_24hr_filtered %>%
  group_by(`End File`, Class, Call_Type) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )



#Find Boat Noise
summary_BOAT_OH <- OH_24hr %>%
  group_by(`End File`, Class, Call_Type, Noise_Type) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

###################################################################
#Create polar plots of # of fish calls per hour
#######################################################################

#TAYLOR ISLET

FSperHour_TI <- TI_24hr %>%
  mutate(
    Begin_Hour = format(
      floor_date(
        parse_date_time(`Begin Clock Time`, orders = "HMS"),
        unit = "hour"
      ),
      "%H:%M:%S"
    )
  ) %>%
  group_by(`End File`, Class, `Begin Date`, Begin_Hour) %>%
  summarise(
    total = n(),
    .groups = "drop"
  ) %>%
  filter(Class == "FS") %>%
  mutate(
    Begin_Hour = factor(
      Begin_Hour,
      levels = c(
        sprintf("%02d:00:00", 12:23),
        sprintf("%02d:00:00", 0:11)
      )
    )
  )

##
#calculate mean and SD of number of fish sounds per hour (this accounts for the number of file annotated for each hour which is not equal across days)
FSperhour_TI22<-FSperHour_TI%>%
  group_by(Begin_Hour)%>%
  summarize(FSmean = mean(total),
            FSsd = sd(total))

# Step 1: Extract first two characters
FSperhour_TI22$Hour <- substr(FSperhour_TI22$Begin_Hour, 1, 2)

# Step 2: Remove leading zeros
FSperhour_TI22$Hour <- sub("^0", "", FSperhour_TI22$Hour)

# Step 3: Convert to numeric
FSperhour_TI22$Hour <- as.numeric(FSperhour_TI22$Hour)

FS_hour_summary1 <- FSperhour_TI22 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##
#TI polar plot
#######################

FS_hour_summary1$maxBackground<-FS_hour_summary1$FSmean + FS_hour_summary1$FSsd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- max(FS_hour_summary1$maxBackground, na.rm = TRUE)

# Create background data for Diel
bg <- data.frame(
  Hour = 0:23
) %>%
  mutate(
    Diel = ifelse(Hour >= 6 & Hour < 20, "day", "night"),
    ymin = 0,
    ymax = max_count
  )

# use this to rotate hours
rotation_radians <- -187 * pi / 180  # clockwise rotation

lp("ggnewscale")
library(ggnewscale)
library(viridis)

TIpolar<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_manual(values = c("day" = "#FFD700", "night" = "#1E90FF")) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = FS_hour_summary1,
    aes(x = Hour, y = FSmean, fill = FSmean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = FS_hour_summary1,
    aes(
      x = Hour,
      ymin = FSmean - FSsd,
      ymax = FSmean + FSsd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "Fish sounds",
    limits = c(0, 783),
    breaks = seq(0, 783, 200),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(0, 960),
    oob = scales::squish
  ) +
  
  coord_polar(start = rotation_radians) +
  scale_x_continuous(
    breaks = 0:23,
    labels = sprintf("%02d:00", 0:23)
  ) +
  labs(
    x = "",
    y = "",
    title = "Taylor Islet"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank(),                 # Keep minor grid lines removed
    legend.position = "none"
  )

TIpolar


####

#DANGER ROCKS
#Summarize number of fish sounds per hour per site


FSperHour_DR <- DR_24hr %>%
  mutate(
    Begin_Hour = format(
      floor_date(
        parse_date_time(`Begin Clock Time`, orders = "HMS"),
        unit = "hour"
      ),
      "%H:%M:%S"
    )
  ) %>%
  group_by(`End File`, Class, `Begin Date`, Begin_Hour) %>%
  summarise(
    total = n(),
    .groups = "drop"
  ) %>%
  filter(Class == "FS") %>%
  mutate(
    Begin_Hour = factor(
      Begin_Hour,
      levels = c(
        sprintf("%02d:00:00", 12:23),
        sprintf("%02d:00:00", 0:11)
      )
    )
  )

##
#calculate mean and SD of number of fish sounds per hour (this accounts for the number of file annotated for each hour which is not equal across days)
FSperhour_DR22<-FSperHour_DR%>%
  group_by(Begin_Hour)%>%
  summarize(FSmean = mean(total),
            FSsd = sd(total))

# Step 1: Extract first two characters
FSperhour_DR22$Hour <- substr(FSperhour_DR22$Begin_Hour, 1, 2)

# Step 2: Remove leading zeros
FSperhour_DR22$Hour <- sub("^0", "", FSperhour_DR22$Hour)

# Step 3: Convert to numeric
FSperhour_DR22$Hour <- as.numeric(FSperhour_DR22$Hour)

FS_hour_summary1 <- FSperhour_DR22 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##

FS_hour_summary1$maxBackground<-FS_hour_summary1$FSmean + FS_hour_summary1$FSsd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- 960
  
  #max(FS_hour_summary1$maxBackground, na.rm = TRUE)

# Create background data for Diel
bg <- data.frame(
  Hour = 0:23
) %>%
  mutate(
    Diel = ifelse(Hour >= 6 & Hour < 20, "day", "night"),
    ymin = 0,
    ymax = max_count
  )

# use this to rotate hours
rotation_radians <- -187 * pi / 180  # clockwise rotation

lp("ggnewscale")
library(ggnewscale)
library(viridis)

DRpolar<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_manual(
    values = c("day" = "#FFD700", "night" = "#1E90FF"),
    name = NULL  # removes the legend title
  ) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = FS_hour_summary1,
    aes(x = Hour, y = FSmean, fill = FSmean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = FS_hour_summary1,
    aes(
      x = Hour,
      ymin = FSmean - FSsd,
      ymax = FSmean + FSsd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "Fish sounds",
    limits = c(0, 763),
    breaks = seq(0, 763, 200),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(0, 960),
    oob = scales::squish
  ) +
  
  coord_polar(start = rotation_radians) +
  scale_x_continuous(
    breaks = 0:23,
    labels = sprintf("%02d:00", 0:23)
  ) +
  labs(
    x = "",
    y = "",
    title = "Danger Rocks"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank(),                 # Keep minor grid lines removed
    #legend.position = "none"
  )

DRpolar

####

#OHIAT ISLAND

#Summarize number of fish sounds per hour per site


FSperHour_OH <- OH_24hr %>%
  mutate(
    Begin_Hour = format(
      floor_date(
        parse_date_time(`Begin Clock Time`, orders = "HMS"),
        unit = "hour"
      ),
      "%H:%M:%S"
    )
  ) %>%
  group_by(`End File`, Class, `Begin Date`, Begin_Hour) %>%
  summarise(
    total = n(),
    .groups = "drop"
  ) %>%
  filter(Class == "FS") %>%
  mutate(
    Begin_Hour = factor(
      Begin_Hour,
      levels = c(
        sprintf("%02d:00:00", 12:23),
        sprintf("%02d:00:00", 0:11)
      )
    )
  )

##
#calculate mean and SD of number of fish sounds per hour (this accounts for the number of file annotated for each hour which is not equal across days)
FSperhour_OH22<-FSperHour_OH%>%
  group_by(Begin_Hour)%>%
  summarize(FSmean = mean(total),
            FSsd = sd(total))

# Step 1: Extract first two characters
FSperhour_OH22$Hour <- substr(FSperhour_OH22$Begin_Hour, 1, 2)

# Step 2: Remove leading zeros
FSperhour_OH22$Hour <- sub("^0", "", FSperhour_OH22$Hour)

# Step 3: Convert to numeric
FSperhour_OH22$Hour <- as.numeric(FSperhour_OH22$Hour)

FS_hour_summary1 <- FSperhour_OH22 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##

FS_hour_summary1$maxBackground<-FS_hour_summary1$FSmean + FS_hour_summary1$FSsd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- 960

#max(FS_hour_summary1$maxBackground, na.rm = TRUE)

# Create background data for Diel
bg <- data.frame(
  Hour = 0:23
) %>%
  mutate(
    Diel = ifelse(Hour >= 6 & Hour < 20, "day", "night"),
    ymin = 0,
    ymax = max_count
  )

# use this to rotate hours
rotation_radians <- -187 * pi / 180  # clockwise rotation

lp("ggnewscale")
library(ggnewscale)
library(viridis)

OHpolar<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_manual(values = c("day" = "#FFD700", "night" = "#1E90FF")) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = FS_hour_summary1,
    aes(x = Hour, y = FSmean, fill = FSmean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = FS_hour_summary1,
    aes(
      x = Hour,
      ymin = FSmean - FSsd,
      ymax = FSmean + FSsd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "Fish sounds",
    limits = c(0, 763),
    breaks = seq(0, 763, 200),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(0, 960),
    oob = scales::squish
  ) +
  
  coord_polar(start = rotation_radians) +
  scale_x_continuous(
    breaks = 0:23,
    labels = sprintf("%02d:00", 0:23)
  ) +
  labs(
    x = "",
    y = "",
    title = "Ohiat Island"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank(),                 # Keep minor grid lines removed
    legend.position = "none"
  )

OHpolar

############
#make one plot

lp("patchwork")

combined_polar <- TIpolar + OHpolar + DRpolar + 
  plot_layout(nrow = 1)  # 1 row, 3 columns

combined_polar

ggsave(
  filename = "combined_polar.png",       # output file name
  plot = combined_polar,                 # plot object
  path = "figures/CH3",                  # folder path
  width = 16,                            # width in inches
  height = 6,                            # height in inches
  dpi = 300                              # resolution
)

#####

#Boat Noise

CPAperHour_OHB <- OH_24hr %>%
  mutate(
    Begin_Hour = format(
      floor_date(
        parse_date_time(`Begin Clock Time`, orders = "HMS"),
        unit = "hour"
      ),
      "%H:%M:%S"
    )
  ) %>%
  group_by(`End File`, Class, Call_Type, Noise_Type, `Begin Date`, Begin_Hour) %>%
  summarise(
    total = n(),
    .groups = "drop"
  ) %>%
  filter(Call_Type == "CPA") %>%
  mutate(
    Begin_Hour = factor(
      Begin_Hour,
      levels = c(
        sprintf("%02d:00:00", 12:23),
        sprintf("%02d:00:00", 0:11)
      )
    )
  )

#look at CPA

##
#calculate mean and SD of number of fish sounds per hour (this accounts for the number of file annotated for each hour which is not equal across days)
CPA_OH<-CPAperHour_OHB%>%
  group_by(Begin_Hour)%>%
  summarize(CPAmean = mean(total),
            CPAsd = sd(total))

# Step 1: Extract first two characters
CPA_OH$Hour <- substr(CPA_OH$Begin_Hour, 1, 2)

# Step 2: Remove leading zeros
CPA_OH$Hour <- sub("^0", "",CPA_OH$Hour)

# Step 3: Convert to numeric
CPA_OH$Hour <- as.numeric(CPA_OH$Hour)

CPA_OH2 <- CPA_OH%>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##

CPA_OH2$maxBackground<-CPA_OH2$FSmean + CPA_OH2$FSsd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- 2

#max(FS_hour_summary1$maxBackground, na.rm = TRUE)

# Create background data for Diel
bg <- data.frame(
  Hour = 0:23
) %>%
  mutate(
    Diel = ifelse(Hour >= 6 & Hour < 20, "day", "night"),
    ymin = 0,
    ymax = max_count
  )

# use this to rotate hours
rotation_radians <- -187 * pi / 180  # clockwise rotation

lp("ggnewscale")
library(ggnewscale)
library(viridis)

OHpolar_B<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_manual(values = c("day" = "#FFD700", "night" = "#1E90FF")) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = CPA_OH2,
    aes(x = Hour, y = CPAmean, fill = CPAmean),
    stat = "identity",
    color = "black"
  ) +
  # geom_errorbar(
  #   data = FS_hour_summary1,
  #   aes(
  #     x = Hour,
  #     ymin = FSmean - FSsd,
  #     ymax = FSmean + FSsd
  #   ),
  #   width = 0.3,
  #   color = "black",
  #   linewidth = 0.6
  # )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "Fish sounds",
    limits = c(0, 3),
    breaks = seq(0, 3, 1),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(0, 3),
    oob = scales::squish
  ) +
  
  coord_polar(start = rotation_radians) +
  scale_x_continuous(
    breaks = 0:23,
    labels = sprintf("%02d:00", 0:23)
  ) +
  labs(
    x = "",
    y = "",
    title = "Ohiat Island"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank()
  )

OHpolar_B
