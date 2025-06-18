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

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250616.csv", header = TRUE)

spectros<-fishdata%>%
  filter(Species == "maliger", ID_confidence == 1|2, t == "g")

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
# 
# TotalFS_ID1 <- fishdata %>%
#   filter((ID_confidence == 1) | 
#            (Common == "Lingcod" & ID_confidence %in% c(1, 2))) %>%
#   group_by(Common) %>%
#   summarize(TotFS_ID1 = n(), .groups = "drop")
# 
# ## count total # of each call type by species (all ID confidence)
# TotalG<-fishdata%>%
#   filter(t == "g") %>%  # Filter to include only rows where ID_confidence is 1
#   group_by(Common) %>%  # Group by Common and ID_confidence
#   summarize(TotG = n())  # Count the rows for each group
# 
# TotalD<-fishdata%>%
#   filter( t == "d") %>%  # Filter to include only rows where ID_confidence is 1
#   group_by(Common) %>%  # Group by Common and ID_confidence
#   summarize(TotD = n())  # Count the rows for each group
# 
# TotalE<-fishdata%>%
#   filter(t == "e") %>%  # Filter to include only rows where ID_confidence is 1
#   group_by(Common) %>%  # Group by Common and ID_confidence
#   summarize(TotE = n())  # Count the rows for each group

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

levels(as.factor(MeanD$Common))
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)
##

#drums - all features
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  select(!c(n,freq_flatness))%>%
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  select(!c(n,freq_flatness))%>%
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
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
  mutate(summary = sprintf("%.1f ± %.1f", D_mean, D_sd)) %>%
  select(Common, n, feature, summary) %>%
  pivot_wider(names_from = feature, values_from = summary)%>%
  select(!c(n,freq_flatness))%>%
  pivot_longer(-Common, names_to = "feature", values_to = "summary") %>%
  pivot_wider(names_from = Common, values_from = summary)%>%
  rename(
    'Black rockfish (n = 2)' = 'Black rockfish',
    'Canary rockfish (n = 5)' = 'Canary rockfish',
    'Copper rockfish (n = 3)' = 'Copper rockfish')

##############
# ### add to summary dataframe
# FSsummary <- TotalFS %>%
#   left_join(TotalFS_ID1, by = "Common") %>%  # Join TotalFS with TotalFS_ID1 by Common
#   left_join(TotalD, by = "Common")%>%
#   left_join(TotalD_ID1, by = "Common")%>%
#   left_join(MeanD, by = "Common")%>%
#   left_join(TotalG, by = "Common")%>%
#   left_join(TotalG_ID1, by = "Common")%>%
#   left_join(MeanG, by = "Common")%>%
#   left_join(TotalE, by = "Common")%>%
#   left_join(TotalE_ID1, by = "Common")%>%
#   left_join(MeanE, by = "Common")
# 
# 
# #full table  
# sound_summary<-flextable(FSsummary)
# 
# print(sound_summary)
##########################
#shortened table

# countFS_table<- FSsummary%>%
#   select(Common, TotFS, TotFS_ID1, TotD_ID1, TotG_ID1, TotE_ID1)%>%
#   filter(Common != "other")%>%
#   rename(
#     "Total Fish Sounds" = TotFS,
#     "Total Fish Sounds (High Confidence)" = TotFS_ID1,
#     "Total Knocks" = TotD_ID1,
#     "Total Grunts" = TotG_ID1,
#     "Total Other"= TotE_ID1,
#     "Species Common Name" = Common
#   )
# 
# set_flextable_defaults(
#   font.size = 10, theme_fun = theme_vanilla,
#   padding = 3,
#   background.color = "white")
# 
# countFS_table_flextable <- flextable(countFS_table)
# countFS_table_flextable <- colformat_double(
#   x = countFS_table_flextable,
#   big.mark = ",", digits = 2, na_str = "N/A"
# )
# 
# countFS_table_flextable  <- line_spacing(countFS_table_flextable , space = 1.5, part = "all")
# # countFS_table_flextable <- add_header_row(countFS_table_flextable,
# #                      colwidths = c(1, 8),
# #                      values = c("", "Sound Features")
# # )
# countFS_table_flextable  <- set_table_properties(countFS_table_flextable , align = "right", layout = "autofit")
# countFS_table_flextable <- theme_vanilla(countFS_table_flextable)
# countFS_table_flextable <- width(countFS_table_flextable, width = 1.2)
# countFS_table_flextable
# save_as_image(x = countFS_table_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/countFS_table.png", zoom = 2)
################

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
  big.mark = ",", digits = 2, na_str = "N/A"
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
save_as_image(x = knock_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/knock_table.png")

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
  big.mark = ",", digits = 2, na_str = "N/A"
)
knock_flextableALL  <- line_spacing(knock_flextableALL , space = 1.5, part = "all")
# knock_flextable <- add_header_row(knock_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
knock_flextableALL  <- set_table_properties(knock_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
knock_flextableALL <- theme_vanilla(knock_flextableALL)
knock_flextableALL <- width(knock_flextableALL, width = 1.2)
knock_flextableALL
save_as_image(x = knock_flextableALL, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/knock_table_ALLFEATURES.png")


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
save_as_image(x = grunt_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/grunt_table.png")

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
# G_flextable <- add_header_row(G_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
G_flextableALL  <- set_table_properties(G_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
G_flextableALL <- theme_vanilla(G_flextableALL)
G_flextableALL <- width(G_flextableALL, width = 1.2)
G_flextableALL
save_as_image(x = G_flextableALL, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/grunt_table_ALLFEATURES.png")



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
# other_flextable <- add_header_row(other_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
other_flextable  <- set_table_properties(other_flextable , align = "right", layout = "autofit")
other_flextable <- theme_vanilla(other_flextable)
other_flextable <- width(other_flextable, width = 1.2)
other_flextable
save_as_image(x = other_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/other_table.png")

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
# other_flextable <- add_header_row(other_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
other_flextableALL  <- set_table_properties(other_flextableALL , align = "right", layout = "autofit")
# Add a title row: "Knocks"
other_flextableALL <- theme_vanilla(other_flextableALL)
other_flextableALL <- width(other_flextableALL, width = 1.2)
other_flextableALL
save_as_image(x = other_flextableALL, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/Other_table_ALLFEATURES.png")


##example code for customizing flextable
#other_flextable <- add_footer_lines(other_flextable, "Daily air quality measurements in New York, May to September 1973.")
#other_flextable <- color(other_flextable, part = "footer", color = "#666666")
#other_flextable <- set_caption(other_flextable, caption = "New York Air Quality Measurements")

