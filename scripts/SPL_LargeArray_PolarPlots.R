#SPL polar plots for Large Array deployment sites

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

###################################################################
#Create polar plots of SPL per hour
#######################################################################

#TAYLOR ISLET SPL

SPL_TI2<-read.csv("odata/24hrAnnotations_AllSites_Selection_Tables/SPL/SPL_TI.csv")

#separate Date column into date and time
SPL_TI2 <- SPL_TI2 %>%
  separate(Date, into = c("Date", "Time"), sep = " ")
str(SPL_TI22)

#convert to date and time format
SPL_TI22 <- SPL_TI2 %>%
  mutate(
    Date = as.Date(Date),
    Time = as_hms(Time)
  )

#average SPL across days and hours (this is arithmetic mean not energy averaging - 
#better for diel pattern analysis as each entry contributes equally to mean rather than one extremely loud event dominating energy averaged SPL)

SPL_hourlyTI <- SPL_TI22 %>%
  mutate(
    UTC = hour(Time)   # hour of day: 0–23
  ) %>%
  group_by(UTC) %>%
  summarise(
    n = n(),
    SPL_mean = mean(SPL, na.rm = TRUE),
    SPL_sd   = sd(SPL, na.rm = TRUE),
    .groups = "drop"
  )

#convert from UTC to pacific standard time (daylight savings - summertime)
SPL_hourlyTI2<- SPL_hourlyTI%>%
  mutate(
    # Convert factor to numeric, subtract 7, and wrap around 24 hours
    Hour = (as.numeric(as.character(UTC)) - 7) %% 24
  )

#Convert to numeric
SPL_hourlyTI2$Hour <- as.numeric(SPL_hourlyTI2$Hour)


SPL_hourlyTI_sum <- SPL_hourlyTI2 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##
#TI polar plot
#######################

SPL_hourlyTI_sum$maxBackground<-SPL_hourlyTI_sum$SPL_mean + SPL_hourlyTI_sum$SPL_sd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- max(SPL_hourlyTI_sum$maxBackground, na.rm = TRUE)
max_count

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

TIpolar_SPL<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  scale_fill_manual(values = c("day" = "#FFFF33", "night" = "#000066"), name = NULL) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = SPL_hourlyTI_sum,
    aes(x = Hour, y = SPL_mean, fill = SPL_mean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = SPL_hourlyTI_sum,
    aes(
      x = Hour,
      ymin = SPL_mean - SPL_sd,
      ymax = SPL_mean + SPL_sd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "SPL",
    limits = c(90, 120),
    breaks = seq(90, 120, 5),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(90, 122),
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

TIpolar_SPL


#Danger Rocks SPL

SPL_DR2<-read.csv("odata/24hrAnnotations_AllSites_Selection_Tables/SPL/SPL_DR.csv")

#separate Date column into date and time
SPL_DR2 <- SPL_DR2 %>%
  separate(Date, into = c("Date", "Time"), sep = " ")
str(SPL_DR2)

#convert to date and time format
SPL_DR22 <- SPL_DR2 %>%
  mutate(
    Date = as.Date(Date),
    Time = as_hms(Time)
  )

#average SPL across days and hours (this is arithmetic mean not energy averaging - 
#better for diel pattern analysis as each entry contributes equally to mean rather than one extremely loud event dominating energy averaged SPL)

SPL_hourlyDR <- SPL_DR22 %>%
  mutate(
    UTC = hour(Time)   # hour of day: 0–23
  ) %>%
  group_by(UTC) %>%
  summarise(
    n = n(),
    SPL_mean = mean(SPL, na.rm = TRUE),
    SPL_sd   = sd(SPL, na.rm = TRUE),
    .groups = "drop"
  )

#convert from UTC to pacific standard time (daylight savings - summertime)
SPL_hourlyDR2<- SPL_hourlyDR%>%
  mutate(
    # Convert factor to numeric, subtract 7, and wrap around 24 hours
    Hour = (as.numeric(as.character(UTC)) - 7) %% 24
  )

#Convert to numeric
SPL_hourlyDR2$Hour <- as.numeric(SPL_hourlyDR2$Hour)


SPL_hourlyDR_sum <- SPL_hourlyDR2 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##
#DR polar plot
#######################

SPL_hourlyDR_sum$maxBackground<-SPL_hourlyDR_sum$SPL_mean + SPL_hourlyDR_sum$SPL_sd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- max(SPL_hourlyDR_sum$maxBackground, na.rm = TRUE)
max_count

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

DRpolar_SPL<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  scale_fill_manual(values = c("day" = "#FFFF33", "night" = "#000066"), name = NULL) + #name=NULL removes title from diel legend
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = SPL_hourlyDR_sum,
    aes(x = Hour, y = SPL_mean, fill = SPL_mean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = SPL_hourlyDR_sum,
    aes(
      x = Hour,
      ymin = SPL_mean - SPL_sd,
      ymax = SPL_mean + SPL_sd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "SPL",
    limits = c(90, 120),
    breaks = seq(90, 120, 5),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(90, 122),
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
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank(),                 # Keep minor grid lines removed
    legend.position = "right"
  )

DRpolar_SPL

#####################
#Ohiat Island SPL

SPL_OH2<-read.csv("odata/24hrAnnotations_AllSites_Selection_Tables/SPL/SPL_OH.csv")

#separate Date column into date and time
SPL_OH2 <- SPL_OH2 %>%
  separate(Date, into = c("Date", "Time"), sep = " ")
str(SPL_OH2)

#convert to date and time format
SPL_OH22 <- SPL_OH2 %>%
  mutate(
    Date = as.Date(Date),
    Time = as_hms(Time)
  )

#average SPL across days and hours (this is arithmetic mean not energy averaging - 
#better for diel pattern analysis as each entry contributes equally to mean rather than one extremely loud event dominating energy averaged SPL)

SPL_hourlyOH <- SPL_OH22 %>%
  mutate(
    UTC = hour(Time)   # hour of day: 0–23
  ) %>%
  group_by(UTC) %>%
  summarise(
    n = n(),
    SPL_mean = mean(SPL, na.rm = TRUE),
    SPL_sd   = sd(SPL, na.rm = TRUE),
    .groups = "drop"
  )

#convert from UTC to pacific standard time (daylight savings - summertime)
SPL_hourlyOH2<- SPL_hourlyOH%>%
  mutate(
    # Convert factor to numeric, subtract 7, and wrap around 24 hours
    Hour = (as.numeric(as.character(UTC)) - 7) %% 24
  )

#Convert to numeric
SPL_hourlyOH2$Hour <- as.numeric(SPL_hourlyOH2$Hour)


SPL_hourlyOH_sum <- SPL_hourlyOH2 %>%
  mutate(Diel = case_when(
    Hour >= 6 & Hour <= 19 ~ "day",
    (Hour >= 20 & Hour <= 23) | (Hour >= 0 & Hour <= 5) ~ "night"
  ))

##
#OH polar plot
#######################

SPL_hourlyOH_sum$maxBackground<-SPL_hourlyOH_sum$SPL_mean + SPL_hourlyOH_sum$SPL_sd

#Create circular polar plot

library(dplyr)
library(ggplot2)

# Determine max count for plotting background
max_count <- max(SPL_hourlyOH_sum$maxBackground, na.rm = TRUE)
max_count

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

OHpolar_SPL<-ggplot() +
  # Background shading for day/night
  geom_tile(
    data = bg,
    aes(x = Hour, y = (ymin + ymax)/2, height = ymax, width = 1, fill = Diel),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  scale_fill_manual(values = c("day" = "#FFFF33", "night" = "#000066")) +
  
  # Allow a new fill scale for bars
  new_scale_fill() +
  
  # Bars for fish sounds colored by FS_count
  geom_bar(
    data = SPL_hourlyOH_sum,
    aes(x = Hour, y = SPL_mean, fill = SPL_mean),
    stat = "identity",
    color = "black"
  ) +
  geom_errorbar(
    data = SPL_hourlyOH_sum,
    aes(
      x = Hour,
      ymin = SPL_mean - SPL_sd,
      ymax = SPL_mean + SPL_sd
    ),
    width = 0.3,
    color = "black",
    linewidth = 0.6
  )+
  scale_fill_viridis_c(
    option = "viridis",
    name = "SPL",
    limits = c(90, 120),
    breaks = seq(90, 120, 5),
    oob = scales::squish
  ) +
  # Fixed radial scale
  scale_y_continuous(
    limits = c(90, 122),
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
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.0),
    axis.text.x = element_text(size = 14), 
    panel.grid.major = element_line(),  # Show major grid lines
    panel.grid.minor = element_blank(),                 # Keep minor grid lines removed
    legend.position = "none"
  )

OHpolar_SPL

############
#make one plot

lp("patchwork")
lp("cowplot") 
lp("magick")

img_path <- "figures/CH3/Boat_clipart.jpg" 

combined_polar_SPL <- TIpolar_SPL + OHpolar_SPL + DRpolar_SPL + 
  plot_layout(nrow = 1)  # 1 row, 3 columns

combined_with_image <- ggdraw(combined_polar_SPL) +
  draw_image(
    img_path,
    x = 0.88,  # horizontal position (0 = left, 1 = right)
    y = 0.80,  # vertical position (0 = bottom, 1 = top)
    width = 0.1,  # fraction of plot width
    height = 0.1 # fraction of plot height
  )
combined_with_image

ggsave(
  filename = "combined_polar_SPL_BOAT.png",       # output file name
  plot = combined_with_image,                 # plot object
  path = "figures/CH3",                  # folder path
  width = 16,                            # width in inches
  height = 6,                            # height in inches
  dpi = 300                              # resolution
)

fish_img_path <- "figures/CH3/combined_polar_fish.png"
boat_img_path <- "figures/CH3/combined_polar_SPL_BOAT.png"

fish_img <- png::readPNG(fish_img_path)
boat_img <- png::readPNG(boat_img_path)

# convert to grobs for cowplot
fish_grob <- grid::rasterGrob(fish_img, interpolate = TRUE)
boat_grob <- grid::rasterGrob(boat_img, interpolate = TRUE)

combined_images <- plot_grid(
  fish_grob,
  boat_grob,
  ncol = 1,   # 1 column → two rows
  align = "v" # vertically aligned
)
combined_images


ggsave(
  filename = "combined_polar_FS_SPL.png",       # output file name
  plot = combined_images,                 # plot object
  path = "figures/CH3",                  # folder path
  width = 16,                            # width in inches
  height = 6,                            # height in inches
  dpi = 300                              # resolution
)
