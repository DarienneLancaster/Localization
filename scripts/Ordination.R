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

###############################
#calculate call sequences (link together calls from the same fish <1 second apart as a call sequence)
str(fishdata)
fishdata$Begin.File
fishdata1<-fishdata
str(fishdata1)

#select rows with duplicate selection numbers and keep only the first occurence
fishdata88<- fishdata1 %>%
  group_by(Begin.File) %>%
  filter(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))%>%
  group_by(Selection)%>%
  slice(1)%>%
  ungroup()

#create data frame with these duplicate selections removed
fishdata77<-fishdata1%>%
  group_by(Begin.File) %>%
  filter(!(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))) 

#paste selections back into main dataframe with duplicate Selection rows removed
fishdata66<-rbind(fishdata88, fishdata77)

#arrange in ascending order
fishdata2<-fishdata66%>%
  arrange(Begin.File, Begin.Time..s.)

#double check no duplicate selections (should have zero observations)
fishdata00<- fishdata2 %>%
  group_by(Begin.File) %>%
  filter(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))

#deal with the same fish crossing over multiple AMAR files
fishdata33 <- fishdata2 %>%
  group_by(fishID)%>%
  mutate(
    End = ifelse(
      Begin.File != lead(Begin.File),  # Check if Begin.File is different in next row
      End.Time..s. - 1800,  # If true, subtract 1800 from End (1800 is the number of seconds in 30 minute files)
      End.Time..s.)  # Otherwise, just copy the End value
    )

fishdata44<-fishdata33%>%
  mutate(End = ifelse(is.na(End), End.Time..s., End))# If End is NA, copy the value from End.Time..s.

#create unique identifier for call sequences (calls can be a maximum of 5 seconds apart to count towards the same call sequence)
fishdata99 <- fishdata44 %>%
  # select(Selection, Begin.Time..s., End.Time..s., fishID, Species, Begin.File, End, Selection_N, Edit)%>%
  group_by(fishID) %>%
  mutate(
    Sequence_ID = cumsum(
      Begin.Time..s. > lag(End, default = first(End)) + 5 | is.na(lag(End))
    ) + 5  # Incremental sequence for each group
  ) %>%
  ungroup()

#calculate time between each call within a call sequence (call Interval)
fishdata98 <- fishdata99 %>%
  group_by(fishID, Sequence_ID) %>%
  mutate(
    Call_Interval = ifelse(
      is.na(lag(End)),  # If lag() returns NA (i.e., first row in group)
      NA,                           # Set Call_Interval to NA
      Begin.Time..s. - lag(End)  # Otherwise, subtract Begin.Time..s. from previous row
    )
  ) %>%
  ungroup()

# #code to check if there are any overlapping selection boxes (should have 0 rows if all errors fixed)
# fishdata111<-fishdata98%>%
#   filter(Call_Interval < 0)

#calculate how many calls are repeated within a sequence
fishdata998 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  tally(name = "count")

#count number of grunts and drums in each 
fishdata001 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(g_count = sum(t == "g", na.rm = TRUE), d_count = sum(t=="d", na.rm = TRUE), e_count = sum(t=="e", na.rm = TRUE))

#join number of fish call data to main data
fishdata999<-left_join(fishdata98, fishdata998, by = c("fishID", "Sequence_ID"))%>%
  mutate(Sequence_Reps = count) %>%
  select(-count) # Remove the intermediate 'count' column, if not needed

#join number of grunts and drums to main data
fishdata002<-left_join(fishdata999, fishdata001, by = c("fishID", "Sequence_ID"))

#calculate mean and sd of call interval for each call sequence
fishdata003 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(C_Interval_mean = mean(Call_Interval, na.rm = TRUE), C_Interval_sd = sd(Call_Interval, na.rm = TRUE))

#join number of grunts and drums to main data
fishdata004<-left_join(fishdata002, fishdata003, by = c("fishID", "Sequence_ID"))

#keep only the first row
fishdata000<-fishdata004%>%
  #filter(Species == "caurinus")%>%
  group_by(fishID, Sequence_ID)%>%
  slice(1)

#histogram of calling interval (time between linked calls) by species 
ggplot(fishdata000, aes(x = C_Interval_mean)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Interval by Species", x = "Call Interval", y = "Count") +
  coord_cartesian(ylim = c(0, 20)) +  # Fix y-axis range from 0 to 8
  theme_classic()

#histogram of calling repetition (within a sequence) by species 
ggplot(fishdata000, aes(x = Sequence_Reps)) +
  geom_histogram(binwidth = 1, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Repetition by Species", x = "Call Repetition Rate", y = "Count") +
  coord_cartesian(ylim = c(0, 10)) +  # Fix y-axis range from 0 to 8
  theme_classic()
  
#need to add back in the rest of the data (only a few column included right now)
#play with different time increments between calls for linking call sequences
#Examine grunts vs. knocks dynamics and behaviour dynamics (are calls repeated more when they're associated with fear/aggression?)

#change missing cells in t to e (for unknown sound)
fishdata<-fishdata%>%
  mutate(t = ifelse(is.na(t) & s == "e", "e", t))%>%
  mutate(t = ifelse(t == "" & s == "e", "e", t))
 

fishdataT<-fishdata%>%
  filter(Species == "maliger", mean_length >200)


# 
# %>%
  # filter(fishID == "0005_20220912")


##################################################################
###look at length vs sound characteristics by fish species############
lp("dplyr")
length<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030)

# Get the unique species from the 'Species' column of the fishdata00 dataframe
species_list <- unique(length$Species)

# Print the species list
print(species_list)

# Create an empty list to store the plots
plot_list <- list()

# Loop through each species and create a plot
for(species_name in species_list) {
  # Subset the data for the current species
  species_data <- length %>%
    filter(Species == species_name)
  
  # Create the scatter plot
  p <- ggplot(species_data, aes(x = mean_length, y =freq_peak )) +
    geom_point() +  # Scatter plot with points colored by 'Species'
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_jitter()+
    labs(title = paste(species_name),
         x = "Mean Length",
         y = "Center Frequency") + 
    theme_bw()
  
  # Add the plot to the plot list
  plot_list[[species_name]] <- p
}

# Use grid.arrange() to print all the plots together in one window
grid.arrange(grobs = plot_list, ncol = 2)  # ncol specifies the number of columns for arrangement

length$cen

#################################################################
####try PCA########################


#bs == "g"
# t == "d",

###################################
#all knocks in PCA with ID confidence of 1

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops|miniatus|vacca") ) #selection 3030 is a major outlier 

# fishdata0<-fishdata%>%
#   filter(t == "d", ID_confidence != 3,  Selection != 3030) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")
# 

##create simplified dataframe to experiment with PCoA and NMDS
# fishdata1<-fishdata0%>%
#   dplyr::select(Species, fishID, Peak.Freq..Hz., Dur.90...s., BW.90...Hz., Low.Freq..Hz., High.Freq..Hz., Inband.Power..dB.FS.,
#                 Center.Freq..Hz., Center.Time..s., Delta.Time..s., Delta.Freq..Hz., Dur.50...s., Max.Freq..Hz., PFC.Max.Freq..Hz., Sample.Length..samples.,
#                 Peak.Time..s.,t, Activity, soundsperfish, mean_length, Selection, Begin.File)

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -Common, -freq_flatness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
  mutate(across(where(is.numeric), scale))

str(fishdata2)
any(is.na(fishdata2))

pca<- rda(fishdata2)

# Extract loadings from the PCA result
loadings <- scores(pca, display = "species")  # Get loadings for the variables

# Print the loadings for PC1 and PC2
loadings_pc1 <- loadings[, 1]  # Loadings for PC1
loadings_pc2 <- loadings[, 2]  # Loadings for PC2

# Sort and find the most important variables for PC1 and PC2
important_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)  # Sort by absolute values
important_pc2 <- sort(abs(loadings_pc2), decreasing = TRUE)

# Get the names of the most important variables
top_variables_pc1 <- names(important_pc1)[1:2]  # Top 2 important variables for PC1
top_variables_pc2 <- names(important_pc2)[1:2]  # Top 2 important variables for PC2

# Create a dataframe with the loadings for the most important variables
variable_loadings <- data.frame(
  Variable = c(top_variables_pc1, top_variables_pc2),
  PC1_loading = c(loadings[top_variables_pc1, 1], loadings[top_variables_pc2, 1]),
  PC2_loading = c(loadings[top_variables_pc1, 2], loadings[top_variables_pc2, 2])
)

# Print the most important variables
cat("Most important variables for PC1:\n")
print(names(important_pc1)[1:2])  # Print top 5 important variables for PC1

cat("\nMost important variables for PC2:\n")
print(names(important_pc2)[1:2])  # Print top 5 important variables for PC2

# Get the PCA scores (coordinates of the samples in the principal component space)
pca1 <- as.data.frame(scores(pca, display = "sites"))
pca1$PCA_ID <- rownames(pca1)
pca1 <- pca1 %>%
  mutate(PCA_ID = str_replace(PCA_ID, "sit", ""))

fishdata1$PCA_ID<-rownames(fishdata1)

pca_all<-left_join(fishdata1, pca1, by = "PCA_ID")


# Plot PCA using ggplot2 and add the most important variables
knocks_IDconf1_pca <- ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Common)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common))+
  geom_segment(data = variable_loadings, 
               aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 1) +  # Add arrows for the most important variables
  geom_text(data = variable_loadings, 
            aes(x = PC1_loading, y = PC2_loading, label = Variable),
            color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  labs(title = "PCA Fish knocks - ID Confidence = 1",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "right")+  # Show legend on the right
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) 
knocks_IDconf1_pca


ggsave("figures/knocks_IDconf1_pca.png", plot = knocks_IDconf1_pca, width = 25, height = 25, units = "cm")

##########################################################
#####all knocks with ID confidence 1 and 2 ###############

#all knocks in PCA with ID confidence of 1

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops|miniatus|vacca|decagrammus|elongatus") ) #selection 3030 is a major outlier 

# fishdata0<-fishdata%>%
#   filter(t == "d", ID_confidence != 3,  Selection != 3030) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")
# 

##create simplified dataframe to experiment with PCoA and NMDS
# fishdata1<-fishdata0%>%
#   dplyr::select(Species, fishID, Peak.Freq..Hz., Dur.90...s., BW.90...Hz., Low.Freq..Hz., High.Freq..Hz., Inband.Power..dB.FS.,
#                 Center.Freq..Hz., Center.Time..s., Delta.Time..s., Delta.Freq..Hz., Dur.50...s., Max.Freq..Hz., PFC.Max.Freq..Hz., Sample.Length..samples.,
#                 Peak.Time..s.,t, Activity, soundsperfish, mean_length, Selection, Begin.File)

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -Common, -freq_flatness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
  mutate(across(where(is.numeric), scale))

str(fishdata2)
any(is.na(fishdata2))

pca<- rda(fishdata2)

# Extract loadings from the PCA result
loadings <- scores(pca, display = "species")  # Get loadings for the variables

# Print the loadings for PC1 and PC2
loadings_pc1 <- loadings[, 1]  # Loadings for PC1
loadings_pc2 <- loadings[, 2]  # Loadings for PC2

# Sort and find the most important variables for PC1 and PC2
important_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)  # Sort by absolute values
important_pc2 <- sort(abs(loadings_pc2), decreasing = TRUE)

# Get the names of the most important variables
top_variables_pc1 <- names(important_pc1)[1:2]  # Top 2 important variables for PC1
top_variables_pc2 <- names(important_pc2)[1:2]  # Top 2 important variables for PC2

# Create a dataframe with the loadings for the most important variables
variable_loadings <- data.frame(
  Variable = c(top_variables_pc1, top_variables_pc2),
  PC1_loading = c(loadings[top_variables_pc1, 1], loadings[top_variables_pc2, 1]),
  PC2_loading = c(loadings[top_variables_pc1, 2], loadings[top_variables_pc2, 2])
)

# Print the most important variables
cat("Most important variables for PC1:\n")
print(names(important_pc1)[1:2])  # Print top 5 important variables for PC1

cat("\nMost important variables for PC2:\n")
print(names(important_pc2)[1:2])  # Print top 5 important variables for PC2

# Get the PCA scores (coordinates of the samples in the principal component space)
pca1 <- as.data.frame(scores(pca, display = "sites"))
pca1$PCA_ID <- rownames(pca1)
pca1 <- pca1 %>%
  mutate(PCA_ID = str_replace(PCA_ID, "sit", ""))

fishdata1$PCA_ID<-rownames(fishdata1)

pca_all<-left_join(fishdata1, pca1, by = "PCA_ID")


# Plot PCA using ggplot2 and add the most important variables
knocks_IDconf1and2_pca <- ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Common)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common))+
  geom_segment(data = variable_loadings, 
               aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 1) +  # Add arrows for the most important variables
  geom_text(data = variable_loadings, 
            aes(x = PC1_loading, y = PC2_loading, label = Variable),
            color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  labs(title = "PCA Fish knocks - ID Confidence = 1",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "right")+  # Show legend on the right
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) 
knocks_IDconf1and2_pca


ggsave("figures/knocks_IDconf1and2_pca.png", plot = knocks_IDconf1and2_pca, width = 25, height = 25, units = "cm")

##########################################################
#####copper and quillback knocks ###############

#all Copper and Quillback knocks in PCA with ID confidence of 1

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|maliger") ) #selection 3030 is a major outlier 

# fishdata0<-fishdata%>%
#   filter(t == "d", ID_confidence != 3,  Selection != 3030) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")
# 

##create simplified dataframe to experiment with PCoA and NMDS
# fishdata1<-fishdata0%>%
#   dplyr::select(Species, fishID, Peak.Freq..Hz., Dur.90...s., BW.90...Hz., Low.Freq..Hz., High.Freq..Hz., Inband.Power..dB.FS.,
#                 Center.Freq..Hz., Center.Time..s., Delta.Time..s., Delta.Freq..Hz., Dur.50...s., Max.Freq..Hz., PFC.Max.Freq..Hz., Sample.Length..samples.,
#                 Peak.Time..s.,t, Activity, soundsperfish, mean_length, Selection, Begin.File)

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -Common, -freq_flatness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
  mutate(across(where(is.numeric), scale))

str(fishdata2)
any(is.na(fishdata2))

pca<- rda(fishdata2)

# Extract loadings from the PCA result
loadings <- scores(pca, display = "species")  # Get loadings for the variables

# Print the loadings for PC1 and PC2
loadings_pc1 <- loadings[, 1]  # Loadings for PC1
loadings_pc2 <- loadings[, 2]  # Loadings for PC2

# Sort and find the most important variables for PC1 and PC2
important_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)  # Sort by absolute values
important_pc2 <- sort(abs(loadings_pc2), decreasing = TRUE)

# Get the names of the most important variables
top_variables_pc1 <- names(important_pc1)[1:2]  # Top 2 important variables for PC1
top_variables_pc2 <- names(important_pc2)[1:2]  # Top 2 important variables for PC2

# Create a dataframe with the loadings for the most important variables
variable_loadings <- data.frame(
  Variable = c(top_variables_pc1, top_variables_pc2),
  PC1_loading = c(loadings[top_variables_pc1, 1], loadings[top_variables_pc2, 1]),
  PC2_loading = c(loadings[top_variables_pc1, 2], loadings[top_variables_pc2, 2])
)

# Print the most important variables
cat("Most important variables for PC1:\n")
print(names(important_pc1)[1:2])  # Print top 5 important variables for PC1

cat("\nMost important variables for PC2:\n")
print(names(important_pc2)[1:2])  # Print top 5 important variables for PC2

# Get the PCA scores (coordinates of the samples in the principal component space)
pca1 <- as.data.frame(scores(pca, display = "sites"))
pca1$PCA_ID <- rownames(pca1)
pca1 <- pca1 %>%
  mutate(PCA_ID = str_replace(PCA_ID, "sit", ""))

fishdata1$PCA_ID<-rownames(fishdata1)

pca_all<-left_join(fishdata1, pca1, by = "PCA_ID")


# Plot PCA using ggplot2 and add the most important variables
knocks_CopandQuill_pca <- ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Common)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common))+
  geom_segment(data = variable_loadings, 
               aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 1) +  # Add arrows for the most important variables
  geom_text(data = variable_loadings, 
            aes(x = PC1_loading, y = PC2_loading, label = Variable),
            color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  labs(title = "PCA Fish knocks - ID Confidence = 1",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "right")+  # Show legend on the right
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) 
knocks_CopandQuill_pca


ggsave("figures/knocks_CopandQuill_pca.png", plot = knocks_CopandQuill_pca, width = 25, height = 25, units = "cm")



##############################################################
#all grunts with ID confidence of 1

fishdata0<-fishdata%>%
  filter(t == "g", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops") ) #selection 3030 is a major outlier 

# fishdata0<-fishdata%>%
#   filter(t == "d", ID_confidence != 3,  Selection != 3030) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")
# 

##create simplified dataframe to experiment with PCoA and NMDS
# fishdata1<-fishdata0%>%
#   dplyr::select(Species, fishID, Peak.Freq..Hz., Dur.90...s., BW.90...Hz., Low.Freq..Hz., High.Freq..Hz., Inband.Power..dB.FS.,
#                 Center.Freq..Hz., Center.Time..s., Delta.Time..s., Delta.Freq..Hz., Dur.50...s., Max.Freq..Hz., PFC.Max.Freq..Hz., Sample.Length..samples.,
#                 Peak.Time..s.,t, Activity, soundsperfish, mean_length, Selection, Begin.File)

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -Common, -freq_flatness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
  mutate(across(where(is.numeric), scale))

str(fishdata2)
any(is.na(fishdata2))

pca<- rda(fishdata2)

# Extract loadings from the PCA result
loadings <- scores(pca, display = "species")  # Get loadings for the variables

# Print the loadings for PC1 and PC2
loadings_pc1 <- loadings[, 1]  # Loadings for PC1
loadings_pc2 <- loadings[, 2]  # Loadings for PC2

# Sort and find the most important variables for PC1 and PC2
important_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)  # Sort by absolute values
important_pc2 <- sort(abs(loadings_pc2), decreasing = TRUE)

# Get the names of the most important variables
top_variables_pc1 <- names(important_pc1)[1:2]  # Top 2 important variables for PC1
top_variables_pc2 <- names(important_pc2)[1:2]  # Top 2 important variables for PC2

# Create a dataframe with the loadings for the most important variables
variable_loadings <- data.frame(
  Variable = c(top_variables_pc1, top_variables_pc2),
  PC1_loading = c(loadings[top_variables_pc1, 1], loadings[top_variables_pc2, 1]),
  PC2_loading = c(loadings[top_variables_pc1, 2], loadings[top_variables_pc2, 2])
)

# Print the most important variables
cat("Most important variables for PC1:\n")
print(names(important_pc1)[1:2])  # Print top 5 important variables for PC1

cat("\nMost important variables for PC2:\n")
print(names(important_pc2)[1:2])  # Print top 5 important variables for PC2

# Get the PCA scores (coordinates of the samples in the principal component space)
pca1 <- as.data.frame(scores(pca, display = "sites"))
pca1$PCA_ID <- rownames(pca1)
pca1 <- pca1 %>%
  mutate(PCA_ID = str_replace(PCA_ID, "sit", ""))

fishdata1$PCA_ID<-rownames(fishdata1)

pca_all<-left_join(fishdata1, pca1, by = "PCA_ID")


# Plot PCA using ggplot2 and add the most important variables
grunts_IDconf1_pca <- ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Common)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common))+
  geom_segment(data = variable_loadings, 
               aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 1) +  # Add arrows for the most important variables
  geom_text(data = variable_loadings, 
            aes(x = PC1_loading, y = PC2_loading, label = Variable),
            color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  labs(title = "PCA Fish grunts - ID Confidence = 1",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "right")+  # Show legend on the right
  scale_color_manual(values = c("blue","red",  "orange","green","black", "yellow","purple",    "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("blue","red", "orange",  "green", "black","yellow", "purple",  "grey", "pink")) 
grunts_IDconf1_pca


ggsave("figures/grunts_IDconf1_pca.png", plot = grunts_IDconf1_pca, width = 25, height = 25, units = "cm")

##################################################################

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops|miniatus|vacca") ) #selection 3030 is a major outlier 

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#remove outlier canary point (fish ID 0001_20220910)
fishdata1<-fishdata1%>%
  filter(fishID != "0001_20220910")

fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -Common, -freq_flatness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
 mutate(across(where(is.numeric), scale))

# Step 1: Calculate the Manhattan distance matrix
#dist_matrix <- vegdist(fishdata2, method = "manhattan")
dist_matrix <- vegdist(fishdata2, method = "euclidean")
#help(vegdist)

# Step 2: Perform NMDS using the distance matrix
nmds_result <- metaMDS(dist_matrix, k = 2)  # k = 2 for 2D plot

# Step 3: Extract NMDS coordinates for plotting
nmds_coordinates <- as.data.frame(scores(nmds_result))


#calculate how much variables are contriubting to axes (nmds is results of test above, nmds5 is just pulling in your predictor variables)
en = envfit(nmds_result, fishdata2 , permutations = 999, na.rm = TRUE)
en #see r2 values of each variable in model
#get coordinates of each point in the nmds
data.scores = as.data.frame(scores(nmds_result))

#create dataframe with species names only to add to nmds results
Common<- fishdata1%>%
  #drop_na()%>%
  select(Common)

#create dataframe with species included with coordinates for plotting
data.scores1<-cbind(Common, data.scores)

#get loadings of each variable
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)

#keep only the variables with r2 >.9 (can check this by running en)
en_coord_cont_filtered <- en_coord_cont[rownames(en_coord_cont) %in% c( "freq_pct75","freq_pct95",
                                                                       "time_centroid"), ]

#join back to original data

nmds_all<-cbind(fishdata1, nmds_coordinates)


# Step 4: Create the NMDS plot using ggplot2

nmds <- ggplot(nmds_all, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Common)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common))+
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont_filtered, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont_filtered, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont_filtered)) + 
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "NMDS1", y = "NMDS2") +
  # geom_segment(data = variable_loadings, 
  #              aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
  #              arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
  #              color = "black", size = 1) +  # Add arrows for the most important variables
  # geom_text(data = variable_loadings, 
  #           aes(x = PC1_loading, y = PC2_loading, label = Variable),
  #           color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  # labs(title = "NMDS Fish grunts - ID Confidence = 1",
  #      x = paste("NMDS (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
  #      y = paste("NMDS (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  #geom_text(aes(label = rownames(nmds_all1)), vjust = -1, hjust = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "right")+  # Show legend on the right
  scale_color_manual(values = c("blue","red",  "orange","green","black", "yellow","purple",    "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("blue","red", "orange",  "green", "black","yellow", "purple",  "grey", "pink")) 
nmds

ggsave("figures/NMDS_gower_unscaled_knocks.png", plot = grunts_IDconf1_pca, width = 25, height = 25, units = "cm")

#check values for outliers
#nmds_all[215, c("fishID","NMDS1", "NMDS2")]

####################################
#try Linear Discriminant Analysis

#FISH KNOCKS - ID CONFIDENCE 1 - colinear variables removed

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops|miniatus|vacca") ) #selection 3030 is a major outlier 

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

# ##### create pair plot with correlation coefficients for all variables###################################################################
# 
#remove response variable
install.packages("GGally")
library(GGally)

#check colinearity (can only run 15 variables at a time in ggpairs plot)
fishdataCOL<-fishdata1%>%
  dplyr::select(freq_asymmetry, freq_kurtosis, freq_skewness,
                freq_centroid,  freq_entropy, freq_upsweep_mean, snr,
                time_iqr, time_asymmetry, time_skewness, time_entropy, time_flatness, time_centroid, time_pct50)

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot
ggpairs(fishdataCOL,
        lower = list(continuous = "smooth"))

#remove outlier canary point (fish ID 0001_20220910)
fishdata1<-fishdata1%>%
  filter(fishID != "0001_20220910")

# fishdata2<-fishdata1%>%
#   dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
#   mutate(Common = as.factor(Common))%>%
#   mutate(across(where(is.numeric), scale))

#try with only important variables from PCA (removed freq_kurtosis and time_iqr because not normally distributed)
fishdata2<-fishdata1%>%
  dplyr::select(Common, freq_asymmetry, freq_skewness,
                freq_centroid,  freq_entropy, freq_upsweep_mean, snr,
                 time_asymmetry, time_skewness, time_entropy, time_flatness, time_centroid, time_pct50)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  mutate(Common = as.factor(Common))%>%
  mutate(across(where(is.numeric), scale))

####histograms- looking at distribution of variables ####
lp("tidyverse")
lp("lubridate")
lp("ggplot2")
lp("patchwork")

#remove site name column as it is non numeric
fishdataHIST<-fishdata2%>%
  dplyr::select(c(-Common))

all_vars <- names(fishdataHIST) #create list of all variable names
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(fishdataHIST, aes(x = !!sym(var),)) +
    geom_histogram() +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

# lp("MASS")
# install.packages(caret)
# library("caret")
# lp("pROC")
# lp("irr")

set.seed(123)
# Split the data into training and testing sets
train_index <- createDataPartition(fishdata2$Common, p = 0.8, list = FALSE)
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

# Fit the LDA model
model <- lda(Common ~ ., data = train)
# Print the model
print(model)

# ######################
# #Test model performance (NEED TO REMOVE EVERYTHING BUT CANARY AND COPPER FROM DATASET FOR MODEL TESTING - NOT ENOUGH SAMPLES)
# 
# # Make predictions on the test set
# lda_pred <- predict(model, newdata = test)
# 
# # The predicted class labels are in lda_pred$class
# predicted_classes <- lda_pred$class
# print(predicted_classes)
# 
# # The true labels from the test dataset are in test$Common
# true_classes <- test$Common
# 
# # Check the distribution of classes in the true values and predicted values
# table(true_classes)
# table(predicted_classes)
# 
# # Create a confusion matrix
# conf_matrix <- confusionMatrix(predicted_classes, true_classes) #positive specified as Canary rockfish because no instances in predicted
# 
# # Print the confusion matrix
# print(conf_matrix)
# 
# # Accuracy
# accuracy <- conf_matrix$overall['Accuracy']
# print(paste("Accuracy:", accuracy))
# 
# # Sensitivity (Recall for each class)
# sensitivity <- conf_matrix$byClass['Sensitivity']
# print(paste("Sensitivity:", sensitivity))
# 
# # Specificity
# specificity <- conf_matrix$byClass['Specificity']
# print(paste("Specificity:", specificity))
# 
# # Precision (Positive Predictive Value for each class)
# precision <- conf_matrix$byClass['Pos Pred Value']
# print(paste("Precision:", precision))
# 
# # F1-Score
# f1_score <- conf_matrix$byClass['F1']
# print(paste("F1 Score:", f1_score))

# Use the LDA model to predict and get the discriminant values (LD1, LD2)
lda_pred <- predict(model)


# Extract the LD1 and LD2 values
lda_result <- data.frame(
  LD1 = lda_pred$x[, 1],  # First linear discriminant
  LD2 = lda_pred$x[, 2],  # Second linear discriminant
  Common = train$Common   # The group label from the original data
)

# Extract the loadings (coefficients) from the LDA model
loadings <- model$scaling  # Extract the scaling matrix for the loadings

# Create a data frame for the loadings and variable names
loadings_df <- data.frame(
  variable = rownames(loadings),  # Variable names
  LD1 = loadings[, 1],  # LD1 loadings
  LD2 = loadings[, 2]   # LD2 loadings
)

#Find most important variables for LD1

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD1)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD1)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD1:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD1:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#Find most important variables for LD2

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD2)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD2)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD2:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD2:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#keep only the variables with r2 >.9 (can check this by running en)
# loadings_df_filtered <- loadings_df[rownames(loadings_df) %in% c( "freq_centroid","High.Freq..Hz.",
#                                                                         "freq_median_mean", "freq_entropy_mean"), ]
# 
# 
# loadings_df_filtered$variable <- c("High\nfrequency", "Frequency\ncentroid", "Mean median\nfrequency", "Mean frequency\nentropy")

#keep only the variables with r2 >.9 (can check this by running en)
loadings_df_filtered <- loadings_df[rownames(loadings_df) %in% c("time_centroid", "freq_centroid", "freq_entropy","time_flatness"), ]


#loadings_df_filtered$variable <- c("High\nfrequency", "Frequency\ncentroid", "Mean median\nfrequency", "Mean frequency\nentropy")

loadings_df_filtered$variable <- c("frequency centroid","frequency entropy", "time centroid", "time flatness")

# Check the loadings_df content (just for debugging)
print(loadings_df_filtered)


# Create the plot using ggplot
LDAknocks<-ggplot(lda_result, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Common), size = 2) +  # Plot points with color by Common groups
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common), show.legend = FALSE) +  # Add 80% confidence ellipse
  geom_segment(data = loadings_df_filtered, aes(x = 0, y = 0, xend = LD1, yend = LD2), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "black") +  # Add arrows
  geom_text(data = loadings_df_filtered, aes(x = LD1, y = LD2, label = variable), 
            size = 4, vjust = -1, hjust =0.4, color = "black") +  # Label arrows with variable names
  theme_bw() +  # Use a minimal theme for the plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title = "LDA Results: Fish Knocks",
    x = "LD1 (66%)",  # Replace with actual explained variance for LD1
    y = "LD2 (25%)",   # Replace with actual explained variance for LD2
    color = "Species"  # Change the legend title to "Species"
  ) +
  scale_color_manual(values = c("black", "gold", "firebrick1", "cyan", "royalblue", "pink")) +  # Customize colors
  scale_fill_manual(values = c("grey", "gold", "firebrick1", "cyan", "royalblue", "pink"))  # Customize fills
LDAknocks

ggsave("figures/LDA_scaled_noColin_IDconf1_knocks.png", plot = LDAknocks, width = 25, height = 25, units = "cm")


####################################
#LDA

#FISH KNOCKS - ID CONFIDENCE 2

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger|melanops|miniatus|vacca") ) #selection 3030 is a major outlier 

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#remove outlier canary point (fish ID 0001_20220910)
fishdata1<-fishdata1%>%
  filter(fishID != "0001_20220910")

fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  mutate(Common = as.factor(Common))%>%
  mutate(across(where(is.numeric), scale))

# lp("MASS")
# install.packages(caret)
# library("caret")
# lp("pROC")
# lp("irr")

set.seed(123)
# Split the data into training and testing sets
train_index <- createDataPartition(fishdata2$Common, p = 0.8, list = FALSE)
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

# Fit the LDA model
model <- lda(Common ~ ., data = train)
# Print the model
print(model)


# Use the LDA model to predict and get the discriminant values (LD1, LD2)
lda_pred <- predict(model)


# Extract the LD1 and LD2 values
lda_result <- data.frame(
  LD1 = lda_pred$x[, 1],  # First linear discriminant
  LD2 = lda_pred$x[, 2],  # Second linear discriminant
  Common = train$Common   # The group label from the original data
)

# Extract the loadings (coefficients) from the LDA model
loadings <- model$scaling  # Extract the scaling matrix for the loadings

# Create a data frame for the loadings and variable names
loadings_df <- data.frame(
  variable = rownames(loadings),  # Variable names
  LD1 = loadings[, 1],  # LD1 loadings
  LD2 = loadings[, 2]   # LD2 loadings
)

#Find most important variables for LD1

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD1)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD1)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD1:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD1:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#Find most important variables for LD2

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD2)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD2)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD2:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD2:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#keep only the top two variables for LD1 and LD2
loadings_df_filtered <- loadings_df[rownames(loadings_df) %in% c( "freq_centroid",
                                                                  "freq_median_mean", "freq_entropy_mean"), ]

loadings_df_filtered$variable <- c("Frequency\ncentroid", "Mean median\nfrequency", "Mean frequency\nentropy")

# Check the loadings_df content (just for debugging)
print(loadings_df_filtered)


# Create the plot using ggplot
LDAknocks2<-ggplot(lda_result, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Common), size = 2) +  # Plot points with color by Common groups
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common), show.legend = FALSE) +  # Add 80% confidence ellipse
  geom_segment(data = loadings_df_filtered, aes(x = 0, y = 0, xend = LD1*0.75, yend = LD2*0.75), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "black") +  # Add arrows
  geom_text(data = loadings_df_filtered, aes(x = LD1*.75, y = LD2*.75, label = variable), 
            size = 4, vjust = -0.8, hjust =0.4, color = "black") +  # Label arrows with variable names
  theme_bw() +  # Use a minimal theme for the plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    #title = "LDA Results: Fish Species",
    x = "LD1 (57%)",  # Replace with actual explained variance for LD1
    y = "LD2 (26%)",   # Replace with actual explained variance for LD2
    color = "Species"  # Change the legend title to "Species"
  ) +
  scale_color_manual(values = c("black", "gold", "firebrick1", "cyan", "royalblue", "pink", "green", "purple")) +  # Customize colors
  scale_fill_manual(values = c("grey", "gold", "firebrick1", "cyan", "royalblue", "pink", "green", "purple"))  # Customize fills
LDAknocks2

ggsave("figures/LDA_scaled_IDconf1and2_knocks.png", plot = LDAknocks2, width = 25, height = 25, units = "cm")

########################################################################

#LDA - FISH GRUNTS ID CONFIDENCE 1

fishdata0<-fishdata%>%
  filter(t == "g", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|melanops|pinniger|maliger") ) #selection 3030 is a major outlier 

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#check colinearity (can only run 15 variables at a time in ggpairs plot)
fishdataCOL<-fishdata1%>%
  dplyr::select(
                freq_centroid,  freq_entropy, 
                 time_asymmetry, time_skewness, time_entropy)

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot
ggpairs(fishdataCOL,
        lower = list(continuous = "smooth"))

# fishdata2<-fishdata1%>%
#   dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
#   mutate(Common = as.factor(Common))%>%
#   mutate(across(where(is.numeric), scale))

#try with only important variables from PCA (removed fkurtosis, fupsweepmean, timecentroid,timeiqr because not normally distributed)
fishdata2<-fishdata1%>%
  dplyr::select(Common, freq_centroid,  freq_entropy, 
                time_asymmetry, time_skewness, time_entropy)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  mutate(Common = as.factor(Common))%>%
  mutate(across(where(is.numeric), scale))

#remove site name column as it is non numeric
fishdataHIST<-fishdata2%>%
  dplyr::select(c(-Common))

all_vars <- names(fishdataHIST) #create list of all variable names
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(fishdataHIST, aes(x = !!sym(var),)) +
    geom_histogram() +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

set.seed(123)
# Split the data into training and testing sets
train_index <- createDataPartition(fishdata2$Common, p = 0.8, list = FALSE)
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

# Fit the LDA model
model <- lda(Common ~ ., data = train)
# Print the model
print(model)

# ######################
# #Test model performance (NEED TO REMOVE CANARY AND QUILLBACK FROM DATASET FOR MODEL TESTING - NOT ENOUGH SAMPLES)
# 
# # Make predictions on the test set
# lda_pred <- predict(model, newdata = test)
# 
# # The predicted class labels are in lda_pred$class
# predicted_classes <- lda_pred$class
# print(predicted_classes)
# 
# # The true labels from the test dataset are in test$Common
# true_classes <- test$Common
# 
# # Check the distribution of classes in the true values and predicted values
# table(true_classes)
# table(predicted_classes)
# 
# # Create a confusion matrix
# conf_matrix <- confusionMatrix(predicted_classes, true_classes) #positive specified as Canary rockfish because no instances in predicted
# 
# # Print the confusion matrix
# print(conf_matrix)
# 
# # Accuracy
# accuracy <- conf_matrix$overall['Accuracy']
# print(paste("Accuracy:", accuracy))
# 
# # Sensitivity (Recall for each class)
# sensitivity <- conf_matrix$byClass['Sensitivity']
# print(paste("Sensitivity:", sensitivity))
# 
# # Specificity
# specificity <- conf_matrix$byClass['Specificity']
# print(paste("Specificity:", specificity))
# 
# # Precision (Positive Predictive Value for each class)
# precision <- conf_matrix$byClass['Pos Pred Value']
# print(paste("Precision:", precision))
# 
# # F1-Score
# f1_score <- conf_matrix$byClass['F1']
# print(paste("F1 Score:", f1_score))


# Use the LDA model to predict and get the discriminant values (LD1, LD2)
lda_pred <- predict(model)




# Extract the LD1 and LD2 values
lda_result <- data.frame(
  LD1 = lda_pred$x[, 1],  # First linear discriminant
  LD2 = lda_pred$x[, 2],  # Second linear discriminant
  Common = train$Common   # The group label from the original data
)

# Extract the loadings (coefficients) from the LDA model
loadings <- model$scaling  # Extract the scaling matrix for the loadings

# Create a data frame for the loadings and variable names
loadings_df <- data.frame(
  variable = rownames(loadings),  # Variable names
  LD1 = loadings[, 1],  # LD1 loadings
  LD2 = loadings[, 2]   # LD2 loadings
)

#Find most important variables for LD1

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD1)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD1)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD1:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD1:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#Find most important variables for LD2

# Find the index of the maximum positive value in LD1
max_positive_index <- which.max(loadings_df$LD2)

# Find the index of the maximum negative value in LD1
max_negative_index <- which.min(loadings_df$LD2)

# Get the value of 'variable' for the max positive and max negative values of LD1
max_positive_value <- loadings_df$variable[max_positive_index]
max_negative_value <- loadings_df$variable[max_negative_index]

# Print the results
print(paste("Max Positive Value of LD2:", loadings_df$LD1[max_positive_index], "for variable:", max_positive_value))
print(paste("Max Negative Value of LD2:", loadings_df$LD1[max_negative_index], "for variable:", max_negative_value))

#keep only the variables with r2 >.9 (can check this by running en)
loadings_df_filtered <- loadings_df[rownames(loadings_df) %in% c( "time_entropy","freq_centroid",
                                                                   "freq_entropy"), ]

loadings_df_filtered$variable <- c("freq_centroid", "freq_entropy", "time_entropy")

# Check the loadings_df content (just for debugging)
print(loadings_df_filtered)


# Create the plot using ggplot
LDAgrunts<- ggplot(lda_result, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Common), size = 2) +  # Plot points with color by Common groups
  stat_ellipse(level = 0.70, geom = "polygon", alpha = 0.2, aes(colour = Common, fill = Common), show.legend = FALSE) +  # Add 80% confidence ellipse
  geom_segment(data = loadings_df_filtered, aes(x = 0, y = 0, xend = LD1, yend = LD2), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "black") +  # Add arrows
  geom_text(data = loadings_df_filtered, aes(x = LD1, y = LD2, label = variable), 
            size = 4, vjust = 1.6, hjust =0.4, color = "black") +  # Label arrows with variable names
  theme_bw() +  # Use a minimal theme for the plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title = "LDA Results: Fish Grunts - ID conf 1 and 2",
    x = "LD1 (94%)",  # Replace with actual explained variance for LD1
    y = "LD2 (5%)",   # Replace with actual explained variance for LD2
    color = "Species"  # Change the legend title to "Species"
  ) +
  scale_color_manual(values = c("black", "gold", "firebrick1", "royalblue", "pink")) +  # Customize colors
  scale_fill_manual(values = c("grey", "gold", "firebrick1", "royalblue", "pink"))  # Customize fills
LDAgrunts

ggsave("figures/LDA_scaled_noColin_normVars_IDconf1and2_grunts.png", plot = LDAgrunts, width = 25, height = 25, units = "cm")





# Plot PCA using ggplot2
grunts_pca<-ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = )) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(title = "PCA Plot of Fish grunts - ID Confidence = 1",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
grunts_pca
ggsave("figures/fish_grunts_PCA_length_IDconf1.png", plot = grunts_pca, width = 25, height = 25, units = "cm")


pca_all$High.Freq..Hz.
# Plot main variables from gruntss pca
grunts_pca<-ggplot(pca_all, aes(x = High.Freq..Hz., y = Low.Freq..Hz.)) +
  geom_point(aes(color = Species)) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(title = "PCA Plot of Fish grunts - ID Confidence = 1",
       x = "High.Freq..Hz.",
       y = "Low.Freq..Hz.") +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
grunts_pca


# Plot main variables from knocks pca
knocks_pca<-ggplot(fishdata1, aes(x = Delta.Time..s., y = Center.Freq..Hz.)) +
  geom_point(aes(color = Species)) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(title = "PCA Plot of Fish Knocks - ID Confidence = 1",
       x = "Delta Time (seconds)",
       y = "Center Frequency (Hz)") +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
knocks_pca

##########################################################################
#try random forest on fish grunts


fishdata0<-fishdata%>%
  filter(t == "g", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|melanops|pinniger|maliger") ) #selection 3030 is a major outlier 

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#try with only important variables from PCA (removed fkurtosis, fupsweepmean, timecentroid,timeiqr because not normally distributed)
# fishdata2<-fishdata1%>%
#   dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
#   mutate(Common = as.factor(Common))%>%
#   mutate(across(where(is.numeric), scale))

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Load the necessary libraries
# install.packages("randomForest")
# library(randomForest)

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  # 80% training, 20% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 500)

# Print the model summary
print(rf_model)

# Extract the first tree from the Random Forest model
# The 'getTree' function gives us the tree as a data frame
tree_1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# Print the first tree's structure
print(tree_1)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")


# Plot the tree (this step can be modified based on how detailed you want it to be)
plot(tree_1, type = "n")  # Create an empty plot
text(tree_1, use.n = TRUE, all = TRUE, cex = 0.8)  # Add tree text to the plot

#find the number of variable selected at each split (mtry) - you select the mtry with the lowest out of bag error (OOB)
mtry <- tuneRF(train[-1],train$Common, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#build model again with best mtry
set.seed(123)
rf_model2 <- randomForest(Common ~ ., data = train, mtry=best.m, importance = TRUE, ntree = 500)

print(rf_model2)
#Evaluate variable importance
importance(rf_model2) #higher the value the more important to the model
varImpPlot(rf_model2) #higher the value the more important to the model

#####################################
#best way to visualize random forest model

# separate into training and test data
library(caret)

set.seed(123)
index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  # 80% training, 20% testing
train_data <- fishdata2[index, ]
test_data <- fishdata2[-index, ]

set.seed(123)
# Train the Random Forest model
model_rf <- caret::train(Common ~ .,
                         data = train_data,
                         method = "rf",
                         #preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))


# Print the model summary
print(model_rf)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train_data)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")



# run model
#plot decision tree

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

lp("igraph")
lp("ggraph")

tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))

#tree with smallest number of nodes
tree_func(final_model = model_rf$finalModel, tree_num)

#tree with largest number of nodes
tree_num <- which(model_rf$finalModel$forest$ndbigtree == max(model_rf$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf$finalModel, tree_num)


###run with test dataset


set.seed(123)
# Train the Random Forest model
model_rf <- caret::train(Common ~ .,
                         data = test_data,
                         method = "rf",
                         #preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))


# Print the model summary
print(model_rf)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = test_data)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")



# run model
#plot decision tree

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

lp("igraph")
lp("ggraph")

tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))

#tree with smallest number of nodes
tree_func(final_model = model_rf$finalModel, tree_num)

#tree with largest number of nodes
tree_num <- which(model_rf$finalModel$forest$ndbigtree == max(model_rf$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf$finalModel, tree_num)

###############################################################
#try random forest on fish knocks


fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|melanops|pinniger|maliger|elongatus") ) #selection 3030 is a major outlier 
#removed Pile Perch, Vermilion and Kelp Greenling as not enough occurrences to make predictions

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#try with only important variables from PCA (removed fkurtosis, fupsweepmean, timecentroid,timeiqr because not normally distributed)
# fishdata2<-fishdata1%>%
#   dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
#   mutate(Common = as.factor(Common))%>%
#   mutate(across(where(is.numeric), scale))

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Load the necessary libraries
# install.packages("randomForest")
# library(randomForest)


# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  # 80% training, 20% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 500)

# Print the model summary
print(rf_model)

#Evaluate variable importance
importance(rf_model) #higher the value the more important to the model
varImpPlot(rf_model) #higher the value the more important to the model

##test model on test dataset

# 1. Make predictions on the test data
test_predictions <- predict(rf_model, newdata = test)

# 2. Evaluate Model Performance

## a) Confusion Matrix
library(caret)
conf_matrix <- confusionMatrix(test_predictions, test$Common)
print(conf_matrix)

## b) Accuracy
accuracy <- sum(test_predictions == test$Common) / length(test$Common)
cat("Accuracy: ", accuracy, "\n")

########need to binarize data to check these metrics
## c) Additional performance metrics (Precision, Recall, F1 Score)
# These are available in the confusionMatrix output as well
precision <- posPredValue(test_predictions, test$Common, positive = "your_positive_class")  # Replace with actual positive class
recall <- sensitivity(test_predictions, test$Common, positive = "your_positive_class")  # Replace with actual positive class
f1_score <- (2 * precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")

# Extract the first tree from the Random Forest model
# The 'getTree' function gives us the tree as a data frame
tree_1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# Print the first tree's structure
print(tree_1)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")


# Plot the tree (this step can be modified based on how detailed you want it to be)
plot(tree_1, type = "n")  # Create an empty plot
text(tree_1, use.n = TRUE, all = TRUE, cex = 0.8)  # Add tree text to the plot

#find the number of variable selected at each split (mtry) - you select the mtry with the lowest out of bag error (OOB)
mtry <- tuneRF(train[-1],train$Common, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#build model again with best mtry
set.seed(123)
rf_model2 <- randomForest(Common ~ ., data = train, mtry=best.m, importance = TRUE, ntree = 500)

print(rf_model2)
#Evaluate variable importance
importance(rf_model2) #higher the value the more important to the model
varImpPlot(rf_model2) #higher the value the more important to the model

# Partial dependence plot coloured by species


lp("ranger")
# Fit a quick RF
set.seed(1143)  # for reproducibility
rfo <- ranger(Common ~ ., data = train, probability = TRUE)
print(rfo)

# Prediction wrapper that returns average prediction for each class
pfun <- function(object, newdata) {
  colMeans(predict(object, data = newdata)$predictions)
}

#Freq_centroid
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_centroid", pred.fun = pfun)
ggplot(p, aes(freq_centroid, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_median_mean
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_median_mean", pred.fun = pfun)
ggplot(p, aes(freq_median_mean, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_pct75
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct75", pred.fun = pfun)
ggplot(p, aes(freq_pct75, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_entropy
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_entropy_std", pred.fun = pfun)
ggplot(p, aes(freq_entropy_std, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_pct5
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

