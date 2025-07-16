#Script for creating summary tables of fish knocks, grunts, and other sounds
#Script also creates tables with full parameters for each species call for all 47 sound features

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
lp("vegan")
lp("gridExtra")
lp("stringr")
lp("flextable")

#pull .csv from wdata (working data) folder - if .csv is not saved in wdata folder remove wdata from file path
fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250616.csv", header = TRUE)

##############################
#create summary sound features table for call types by species

#create new column with species common names
fishdata$Common<- ifelse(fishdata$Species == "caurinus", "Copper rockfish",
                         ifelse(fishdata$Species == "maliger", "Quillback rockfish",
                                ifelse(fishdata$Species == "pinniger", "Canary rockfish",
                                       ifelse(fishdata$Species == "miniatus", "Vermillion rockfish",
                                              ifelse(fishdata$Species == "melanops", "Black rockfish",
                                                     ifelse(fishdata$Species == "elongatus", "Lingcod",
                                                            ifelse(fishdata$Species == "decagrammus", "Kelp Greenling",
                                                                   ifelse(fishdata$Species == "vacca", "Pile Perch", "other"))))))))

TotalFS<-fishdata%>%
  group_by(Common) %>%
  summarize(TotFS = n())
# 
# #check how many g, d, and e for each species and confidence value
Sum<-fishdata %>%
  group_by(Common, ID_confidence, t) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = ID_confidence, values_from = n, names_prefix = "ID_conf_",
                     values_fill = 0)

Sum<-fishdata %>%
  group_by(ID_confidence) %>%
  summarise(n = n(), .groups = "drop")

## count total # of each call type by species (ID confidence 1 ONLY)
TotalG_ID1 <- fishdata %>%
  filter(t == "g" & 
           Common != "other" &
           (ID_confidence == 1 | 
              (Common == "Quillback rockfish" & ID_confidence %in% c(1, 2)))) %>%
  group_by(Common) %>%
  summarize(TotG_ID1 = n(), .groups = "drop")

TotalD_ID1 <- fishdata %>%
  filter(t == "d" &
           Common != "other" &
           (ID_confidence == 1 | 
              (Common == "Lingcod" & ID_confidence %in% c(1, 2)))) %>%
  group_by(Common) %>%
  summarize(TotG_ID1 = n(), .groups = "drop")


TotalE_ID1<-fishdata%>%
  filter(ID_confidence == 1, t == "e", Common != "other") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotE_ID1 = n())  # Count the rows for each group


#calculate mean and SD for each sound feature for all knocks (d) with ID confidence of 1
##
MeanD <- fishdata %>%
  filter(t == "d" &
           Common != "other" &
           (ID_confidence == 1 |
              (Common == "Lingcod" & ID_confidence %in% c(1, 2)))) %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak, freq_bandwidth, time_duration),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)
##

#knocks - all features
#calculate mean and SD for each sound feature for all knocks (d) with ID confidence of 1
##
MeanD_all <- fishdata %>%
  filter(t == "d" &
           Common != "other" &
           (ID_confidence == 1 | 
              (Common == "Lingcod" & ID_confidence %in% c(1, 2)))) %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak:time_centroid),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  dplyr::select(!c(n,freq_flatness))%>%
  pivot_longer(-Common, names_to = "feature", values_to = "summary") %>%
  pivot_wider(names_from = Common, values_from = summary)%>%
  rename(
    'Black rockfish (n = 28)' = 'Black rockfish',
    'Canary rockfish (n = 253)' = 'Canary rockfish',
    'Copper rockfish (n = 147)' = 'Copper rockfish',
    'Lingcod (n = 52)' = 'Lingcod',
    'Pile Perch (n = 15)' = 'Pile Perch',
    'Quillback rockfish (n = 44)' = 'Quillback rockfish',
    'Vermillion rockfish (n = 2)' = 'Vermillion rockfish')
##

#calculate mean and SD for each sound feature for all grunts (g) with ID confidence of 1
##
MeanG <- fishdata %>%
  filter(ID_confidence == 1, t == "g", Common !="other") %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak, freq_bandwidth, time_duration),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)

#grunts - all features
MeanG_all <- fishdata %>%
  filter(ID_confidence == 1, t == "g", Common !="other") %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak:time_centroid),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  dplyr::select(!c(n,freq_flatness))%>%
  pivot_longer(-Common, names_to = "feature", values_to = "summary") %>%
  pivot_wider(names_from = Common, values_from = summary)%>%
  rename(
    'Black rockfish (n = 45)' = 'Black rockfish',
    'Canary rockfish (n = 2)' = 'Canary rockfish',
    'Copper rockfish (n = 50)' = 'Copper rockfish',
    'Quillback rockfish (n = 9)' = 'Quillback rockfish')

#calculate mean and SD for each sound feature for all unknown sounds (e) with ID confidence of 1

##
MeanE <- fishdata %>%
  filter(ID_confidence == 1, t == "e", Common !="other") %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak, freq_bandwidth, time_duration),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)

#other sounds - all features
MeanE_all <- fishdata %>%
  filter(ID_confidence == 1, t == "e", Common !="other") %>%
  rename(
    high_freq_hz = High.Freq..Hz.,
    low_freq_hz = Low.Freq..Hz.
  ) %>%
  group_by(Common) %>%
  summarise(
    n = n(),  # ← Add count of rows per species
    across(
      c(high_freq_hz, low_freq_hz, freq_peak:time_centroid),
      list(D_mean = ~mean(.x, na.rm = TRUE), D_sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Common, n),  # Keep `n` out of the pivot
    names_to = c("feature", "stat"),
    names_pattern = "(.*)_(D_mean|D_sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(summary = sprintf("%.2f ± %.2f", D_mean, D_sd)) %>%
  dplyr::select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  dplyr::select(!c(n,freq_flatness))%>%
  pivot_longer(-Common, names_to = "feature", values_to = "summary") %>%
  pivot_wider(names_from = Common, values_from = summary)%>%
  rename(
    'Black rockfish (n = 2)' = 'Black rockfish',
    'Canary rockfish (n = 5)' = 'Canary rockfish',
    'Copper rockfish (n = 3)' = 'Copper rockfish')

##############

#knock table with sound features

knock_table<- MeanD%>%
  rename(
    "Knock high frequency (Hz)" = high_freq_hz,
    "Knock low frequency (Hz)" = low_freq_hz,
    "Knock peak frequency (Hz)" = freq_peak,
    "Knock bandwidth (Hz)" = freq_bandwidth,
    "Knock duration (s)" = time_duration,
    "Species" = Common
  )

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

knock_flextable <- flextable(knock_table)
knock_flextable <- colformat_double(
  x = knock_flextable,
  big.mark = ",", digits = 4, na_str = "N/A"
)
knock_flextable  <- line_spacing(knock_flextable , space = 1.5, part = "all")
# knock_flextable <- add_header_row(knock_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
knock_flextable  <- set_table_properties(knock_flextable , align = "right", layout = "autofit")
# Add a title row: "Knocks"
knock_flextable <- theme_vanilla(knock_flextable)
knock_flextable <- width(knock_flextable, width = 1.2)
knock_flextable
save_as_image(x = knock_flextable, path = "figures/knock_table.png")

#knock table ALL FEATURES

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")
knock_flextableALL<- MeanD_all%>%
  rename(
    "Knock feature" = feature
  )
knock_flextableALL <- flextable(knock_flextableALL)

knock_flextableALL
knock_flextableALL <- colformat_double(
  x = knock_flextableALL,
  big.mark = ",", digits = 4, na_str = "N/A"
)
knock_flextableALL  <- line_spacing(knock_flextableALL , space = 1.5, part = "all")

knock_flextableALL  <- set_table_properties(knock_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
knock_flextableALL <- theme_vanilla(knock_flextableALL)
knock_flextableALL <- width(knock_flextableALL, width = 1.2)
knock_flextableALL
save_as_image(x = knock_flextableALL, path = "figures/knock_table_ALLFEATURES.png")


#Grunt Table

grunt_table<- MeanG%>%
  rename(
    "Grunt high frequency (Hz)" = high_freq_hz,
    "Grunt low frequency (Hz)" = low_freq_hz,
    "Grunt peak frequency (Hz)" = freq_peak,
    "Grunt bandwidth (Hz)" = freq_bandwidth,
    "Grunt duration (s)" = time_duration,
    "Species" = Common
  )

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

grunt_flextable <- flextable(grunt_table)
grunt_flextable <- colformat_double(
  x = grunt_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)
grunt_flextable  <- line_spacing(grunt_flextable , space = 1.5, part = "all")
# grunt_flextable <- add_header_row(grunt_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
grunt_flextable  <- set_table_properties(grunt_flextable , align = "right", layout = "autofit")
grunt_flextable <- theme_vanilla(grunt_flextable)
grunt_flextable <- width(grunt_flextable, width = 1.2)
grunt_flextable
save_as_image(x = grunt_flextable, path = "figures/grunt_table.png")

#grunt table ALL FEATURES

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

G_flextableALL<- MeanG_all%>%
  rename(
    "Grunt feature" = feature
  )
G_flextableALL <- flextable(G_flextableALL)

G_flextableALL
G_flextableALL <- colformat_double(
  x = G_flextableALL,
  big.mark = ",", digits = 2, na_str = "N/A"
)
G_flextableALL  <- line_spacing(G_flextableALL , space = 1.5, part = "all")

G_flextableALL  <- set_table_properties(G_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
G_flextableALL <- theme_vanilla(G_flextableALL)
G_flextableALL <- width(G_flextableALL, width = 1.2)
G_flextableALL
save_as_image(x = G_flextableALL, path = "figures/grunt_table_ALLFEATURES.png")



#Other Table
other_table<- MeanE%>%
  rename(
    "Other high frequency (Hz)" = high_freq_hz,
    "Other low frequency (Hz)" = low_freq_hz,
    "Other peak frequency (Hz)" = freq_peak,
    "Other bandwidth (Hz)" = freq_bandwidth,
    "Other duration (s)" = time_duration,
    "Species" = Common
  )

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

other_flextable <- flextable(other_table)
other_flextable <- colformat_double(
  x = other_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)
other_flextable  <- line_spacing(other_flextable , space = 1.5, part = "all")
other_flextable  <- set_table_properties(other_flextable , align = "right", layout = "autofit")
other_flextable <- theme_vanilla(other_flextable)
other_flextable <- width(other_flextable, width = 1.2)
other_flextable
save_as_image(x = other_flextable, path = "figures/other_table.png")

#Other sounds table - ALL FEATURES
set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

other_flextableALL<- MeanE_all%>%
  rename(
    "Other sound feature" = feature
  )
other_flextableALL <- flextable(other_flextableALL)

other_flextableALL
other_flextableALL <- colformat_double(
  x = other_flextableALL,
  big.mark = ",", digits = 2, na_str = "N/A"
)
other_flextableALL  <- line_spacing(other_flextableALL , space = 1.5, part = "all")

other_flextableALL  <- set_table_properties(other_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
other_flextableALL <- theme_vanilla(other_flextableALL)
other_flextableALL <- width(other_flextableALL, width = 1.2)
other_flextableALL
save_as_image(x = other_flextableALL, path = "figures/Other_table_ALLFEATURES.png")



