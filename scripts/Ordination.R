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
#create summary table for call types by species

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
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030)

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

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence != 3,  Selection != 3030, str_detect(Species, "caurinus|maliger|pinniger") ) #selection 3030 is a major outlier 

# fishdata0<-fishdata%>%
#   filter(t == "d", ID_confidence != 3,  Selection != 3030) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")
# 

##create simplified dataframe to experiment with PCoA and NMDS
fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Peak.Freq..Hz., Dur.90...s., BW.90...Hz., Low.Freq..Hz., High.Freq..Hz., Inband.Power..dB.FS.,
                Center.Freq..Hz., Center.Time..s., Delta.Time..s., Delta.Freq..Hz., Dur.50...s., Max.Freq..Hz., PFC.Max.Freq..Hz., Sample.Length..samples.,
                Peak.Time..s.,t, Activity, soundsperfish, mean_length, Selection, Begin.File)


fishdata2<-fishdata1%>%
  dplyr::select(-Species, -fishID, -mean_length, -t, -Activity, -Selection, -Begin.File, -soundsperfish)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  #mutate(Activity = if_else(Activity == "" | is.na(Activity), "none", Activity))%>%
  drop_na()%>%
  mutate(across(where(is.numeric), scale))

str(fishdata2)

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
copper_knocks_pca <- ggplot(pca_all, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Species)) +  # Color by Species (shape = Activity)
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  stat_ellipse(aes(color = Species), 
               level = 0.95,  # Set the confidence level for the ellipse, default is 0.95
               linetype = "solid", size = 1)+
  geom_segment(data = variable_loadings, 
               aes(x = 0, y = 0, xend = PC1_loading, yend = PC2_loading),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 1) +  # Add arrows for the most important variables
  geom_text(data = variable_loadings, 
            aes(x = PC1_loading, y = PC2_loading, label = Variable),
            color = "black", size = 4, vjust = 1, hjust = 1.5) +  # Add variable names at the arrow ends
  labs(title = "PCA Fish grunts - ID Confidence = 1 and 2",
       x = paste("PC1 (", round(100 * summary(pca)$cont$importance[2, 1]), "%)", sep = ""),
       y = paste("PC2 (", round(100 * summary(pca)$cont$importance[2, 2]), "%)", sep = "")) +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
copper_knocks_pca


ggsave("figures/copQBblack_grunts_PCA_species_IDconf1n2.png", plot = copper_knocks_pca, width = 25, height = 25, units = "cm")


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
