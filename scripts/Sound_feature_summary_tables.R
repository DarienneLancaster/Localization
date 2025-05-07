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

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250221.csv", header = TRUE)

spectros<-fishdata%>%
  filter(Species == "pinniger", ID_confidence == 1, t == "g")

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

TotalFS_ID1<-fishdata%>%
  filter(ID_confidence == 1) %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotFS_ID1 = n())  # Count the rows for each group

## count total # of each call type by species (all ID confidence)
TotalG<-fishdata%>%
  filter(t == "g") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotG = n())  # Count the rows for each group

TotalD<-fishdata%>%
  filter( t == "d") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotD = n())  # Count the rows for each group

TotalE<-fishdata%>%
  filter(t == "e") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotE = n())  # Count the rows for each group

## count total # of each call type by species (ID confidence 1 ONLY)
TotalG_ID1<-fishdata%>%
  filter(ID_confidence == 1, t == "g") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotG_ID1 = n())  # Count the rows for each group

TotalD_ID1<-fishdata%>%
  filter(ID_confidence == 1, t == "d") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotD_ID1 = n())  # Count the rows for each group

TotalE_ID1<-fishdata%>%
  filter(ID_confidence == 1, t == "e") %>%  # Filter to include only rows where ID_confidence is 1
  group_by(Common) %>%  # Group by Common and ID_confidence
  summarize(TotE_ID1 = n())  # Count the rows for each group

#calculate mean and SD for each sound feature for all knocks (d) with ID confidence of 1
MeanD<-fishdata%>%
  filter(ID_confidence == 1, t == "d")%>%
  group_by(Common) %>%  # Group by Common
  summarise(
    D_mean_High_Freq = mean(High.Freq..Hz., na.rm = TRUE),  # Mean for High.Freq..Hz.
    D_sd_High_Freq = sd(High.Freq..Hz., na.rm = TRUE),  
    D_mean_Low_Freq = mean(Low.Freq..Hz., na.rm = TRUE),    # Mean for Low.Freq..Hz.
    D_sd_Low_Freq = sd(Low.Freq..Hz., na.rm = TRUE),  
    across(freq_peak:time_centroid, \(x) mean(x, na.rm = TRUE), .names = "D_mean_{col}"),  # Calculate the mean
    across( freq_peak:time_centroid, \(x) sd(x, na.rm = TRUE), .names = "D_sd_{col}")     # Calculate the SD
  )

#keep only a few columns
MeanD1<-MeanD%>%
  select(Common,D_mean_High_Freq, D_sd_High_Freq, D_mean_Low_Freq, D_sd_Low_Freq,
         D_mean_freq_peak,D_sd_freq_peak,D_mean_freq_bandwidth, D_sd_freq_bandwidth, 
         D_mean_time_duration, D_sd_time_duration)

#calculate mean and SD for each sound feature for all grunts (g) with ID confidence of 1
MeanG<-fishdata%>%
  filter(ID_confidence == 1, t == "g")%>%
  group_by(Common) %>%  # Group by Common
  summarise(
    G_mean_High_Freq = mean(High.Freq..Hz., na.rm = TRUE),  # G_mean for High.Freq..Hz.
    G_sd_High_Freq = sd(High.Freq..Hz., na.rm = TRUE),  
    G_mean_Low_Freq = mean(Low.Freq..Hz., na.rm = TRUE),    # Mean for Low.Freq..Hz.
    G_sd_Low_Freq = sd(Low.Freq..Hz., na.rm = TRUE),  
    across(freq_peak:time_centroid, \(x) mean(x, na.rm = TRUE), .names = "G_mean_{col}"),  # Calculate the mean
    across(freq_peak:time_centroid, \(x) sd(x, na.rm = TRUE), .names = "G_sd_{col}")     # Calculate the SD
  )

#keep only a few columns
MeanG1<-MeanG%>%
  select(Common,G_mean_High_Freq, G_sd_High_Freq, G_mean_Low_Freq, G_sd_Low_Freq, G_mean_High_Freq,
         G_mean_freq_peak,G_sd_freq_peak,G_mean_freq_bandwidth, G_sd_freq_bandwidth, 
         G_mean_time_duration, G_sd_time_duration)

#calculate mean and SD for each sound feature for all unknown sounds (e) with ID confidence of 1
MeanE<-fishdata%>%
  filter(ID_confidence == 1, t == "e")%>%
  group_by(Common) %>%  # Group by Common
  summarise(
    E_mean_High_Freq = mean(High.Freq..Hz., na.rm = TRUE),  # Mean for High.Freq..Hz.
    E_sd_High_Freq = sd(High.Freq..Hz., na.rm = TRUE),  
    E_mean_Low_Freq = mean(Low.Freq..Hz., na.rm = TRUE),    # Mean for Low.Freq..Hz.
    E_sd_Low_Freq = sd(Low.Freq..Hz., na.rm = TRUE),  
    across(freq_peak:time_centroid, \(x) mean(x, na.rm = TRUE), .names = "E_mean_{col}"),  # Calculate the mean
    across(freq_peak:time_centroid, \(x) sd(x, na.rm = TRUE), .names = "E_sd_{col}")     # Calculate the SD
  )

#keep only a few columns
MeanE1<-MeanE%>%
  select(Common,E_mean_High_Freq, E_sd_High_Freq, E_mean_Low_Freq, E_sd_Low_Freq, E_mean_High_Freq,
         E_mean_freq_peak,E_sd_freq_peak,E_mean_freq_bandwidth, E_sd_freq_bandwidth, 
         E_mean_time_duration, E_sd_time_duration)


### add to summary dataframe
FSsummary <- TotalFS %>%
  left_join(TotalFS_ID1, by = "Common") %>%  # Join TotalFS with TotalFS_ID1 by Common
  left_join(TotalD, by = "Common")%>%
  left_join(TotalD_ID1, by = "Common")%>%
  left_join(MeanD1, by = "Common")%>%
  left_join(TotalG, by = "Common")%>%
  left_join(TotalG_ID1, by = "Common")%>%
  left_join(MeanG1, by = "Common")%>%
  left_join(TotalE, by = "Common")%>%
  left_join(TotalE_ID1, by = "Common")%>%
  left_join(MeanE1, by = "Common")


#full table  
sound_summary<-flextable(FSsummary)

print(sound_summary)

#shortened table

countFS_table<- FSsummary%>%
  select(Common, TotFS, TotFS_ID1, TotD_ID1, TotG_ID1, TotE_ID1)%>%
  filter(Common != "other")%>%
  rename(
    "Total Fish Sounds" = TotFS,
    "Total Fish Sounds (High Confidence)" = TotFS_ID1,
    "Total Knocks" = TotD_ID1,
    "Total Grunts" = TotG_ID1,
    "Total Other"= TotE_ID1,
    "Species Common Name" = Common
  )

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

countFS_table_flextable <- flextable(countFS_table)
countFS_table_flextable <- colformat_double(
  x = countFS_table_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

countFS_table_flextable  <- line_spacing(countFS_table_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
countFS_table_flextable  <- set_table_properties(countFS_table_flextable , align = "right", layout = "autofit")
countFS_table_flextable <- theme_vanilla(countFS_table_flextable)
countFS_table_flextable
save_as_image(x = countFS_table_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/countFS_table.png")


#knock table with sound features

knock_table<- FSsummary%>%
  select(Common, D_mean_High_Freq, D_sd_High_Freq, D_mean_Low_Freq, D_sd_Low_Freq,  D_mean_freq_peak, D_sd_freq_peak, D_mean_time_duration, D_sd_time_duration)%>%
  filter(Common != "other")%>%
  filter(Common != "Lingcod")%>%
  filter(Common != "Kelp Greenling")%>%
  rename(
    "Knock mean high freq (Hz)" = D_mean_High_Freq,
    "Knock sd high freq (Hz)" = D_sd_High_Freq,
    "Knock mean low freq (Hz)" = D_mean_Low_Freq,
    "Knock sd low freq (Hz)" = D_sd_Low_Freq,
    "Knock mean peak freq (Hz)" = D_mean_freq_peak,
    "Knock sd peak freq (Hz)" = D_sd_freq_peak,
    "Knock mean duration (s)" = D_mean_time_duration,
    "Knock sd duration (s)" = D_sd_time_duration,
    "Species Common Name" = Common
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
knock_flextable <- theme_vanilla(knock_flextable)
knock_flextable
save_as_image(x = knock_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/knock_table.png")


#Grunt Table

grunt_table<- FSsummary%>%
  select(Common,G_mean_High_Freq, G_sd_High_Freq, G_mean_Low_Freq, G_sd_Low_Freq, G_mean_freq_peak, G_sd_freq_peak, G_mean_time_duration, G_sd_time_duration)%>%
  filter(Common != "other")%>%
  filter(Common != "Lingcod")%>%
  filter(Common != "Kelp Greenling")%>%
  filter(Common != "Vermillion rockfish")%>%
  filter(Common != "Pile Perch")%>%
  rename(
    "Grunt mean high freq (Hz)" = G_mean_High_Freq,
    "Grunt sd high freq (Hz)" = G_sd_High_Freq,
    "Grunt mean low freq (Hz)"= G_mean_Low_Freq,
    "Grunt sd low freq (Hz)" = G_sd_Low_Freq,
    "Grunt mean peak freq (Hz)" = G_mean_freq_peak,
    "Grunt sd peak freq (Hz)" = G_sd_freq_peak,
    "Grunt mean duration (s)" = G_mean_time_duration,
    "Grunt sd duration (s)" = G_sd_time_duration,
    "Species Common Name" = Common
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
grunt_flextable
save_as_image(x = grunt_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/grunt_table.png")


#Other Table
other_table<- FSsummary%>%
  select(Common,E_mean_High_Freq, E_sd_High_Freq, E_mean_Low_Freq, E_sd_Low_Freq, E_sd_High_Freq, E_mean_freq_peak, E_sd_freq_peak, E_mean_time_duration, E_sd_time_duration)%>%
  filter(Common != "other")%>%
  filter(Common != "Lingcod")%>%
  filter(Common != "Kelp Greenling")%>%
  filter(Common != "Vermillion rockfish")%>%
  filter(Common != "Quillback rockfish")%>%
  filter(Common != "Pile Perch")%>%
  rename(
    "Other mean high freq (Hz)" = E_mean_High_Freq,
    "Other sd high freq (Hz)" = E_sd_High_Freq,
    "Other mean low freq (Hz)" = E_mean_Low_Freq,
    "Other sd low freq (Hz)" = E_sd_Low_Freq,
    "Other mean peak freq (Hz)" = E_mean_freq_peak,
    "Other sd peak freq (Hz)" = E_sd_freq_peak,
    "Other mean duration (s)" = E_mean_time_duration,
    "Other sd duration (s)" = E_sd_time_duration,
    "Species Common Name" = Common
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
other_flextable
save_as_image(x = other_flextable, path = "C:/Users/dlanc/Documents/PhD/Draft Manuscripts/Chapter 1 Species Specific Fish Sounds/Figures/other_table.png")

##example code for customizing flextable
#other_flextable <- add_footer_lines(other_flextable, "Daily air quality measurements in New York, May to September 1973.")
#other_flextable <- color(other_flextable, part = "footer", color = "#666666")
#other_flextable <- set_caption(other_flextable, caption = "New York Air Quality Measurements")