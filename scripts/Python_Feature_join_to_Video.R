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

#load in original annotated video and sound data
FD<-read.csv("wdata/Sound_Species_Behaviour_Length_20250213.csv", header = TRUE)

#remove most raven columns except start time and file name


FD1<-FD%>%
  select(Begin.Time..s., Begin.File, 106:153)
  #filter(grepl("AMAR173.4.20220817T013710Z|AMAR173.4.20220913T150952Z", Begin.File))

#load in new feature classification data from python generated selection tables
PyData<-imp_raven(path = "wdata/Random_Forest_Feature_Extraction/measurements/ALL", all.data =  TRUE, only.spectro.view = FALSE)

#make begin.time and begin file names match across datasets
PyData1<-PyData%>%
  rename_all(~ gsub(" ", ".", gsub("\\(", ".", gsub("\\)", ".", gsub("/", ".", gsub("%", ".", gsub("-", ".", .)))))))

#join original video data to new sound feature data

FD_py<-left_join(FD1, PyData1, by= c("Begin.File", "Begin.Time..s."))

py_clean<- FD_py%>%
  select(-X, -X.1, -X.2, -a, -selec.file.x, -selec.file.y, -sdDrift, -meanDrift, -locadjust, -Cam, -vidnames1, -vidnames2, -vidnames3,
         -filenum3, -filenum1, -filenum2, -vidstarttime, -videndtime, -videotime1, -videotime2, -vidnum, -CamName, 
         -View, -Channel, -Confidence, -Class, -Sound.type, -Software, -uuid, )

#export .csv of new data
write.csv(py_clean,"wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250616.csv", row.names = FALSE)

###look at length vs sound characteristics by fish species############

#NEW PY DATA
length<-FD_py%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1,  Selection != 3030)

length$

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
    labs(title = paste(species_name),
         x = "Mean Length",
         y = "Center Frequency") + 
    theme_bw()
  
  # Add the plot to the plot list
  plot_list[[species_name]] <- p
}

# Use grid.arrange() to print all the plots together in one window
grid.arrange(grobs = plot_list, ncol = 2)  # ncol specifies the number of columns for arrangement

#OLD RAVEN DATA
length<-FD%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1,  Selection != 3030)

length$Peak.Freq..Hz.

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
  p <- ggplot(species_data, aes(x = mean_length, y =Peak.Freq..Hz. )) +
    geom_point() +  # Scatter plot with points colored by 'Species'
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste(species_name),
         x = "Mean Length",
         y = "Center Frequency") + 
    theme_bw()
  
  # Add the plot to the plot list
  plot_list[[species_name]] <- p
}

# Use grid.arrange() to print all the plots together in one window
grid.arrange(grobs = plot_list, ncol = 2)  # ncol specifies the number of columns for arrangement

#show rows with very low peak frequency values
FD_py1<-FD_py%>%
  #filter(freq_peak <20)
  filter(t=="d")

FD_py1$time_duration
# Plot all species knocks
grunts<-ggplot(FD_py1, aes(x = time_duration, y = freq_peak)) +
  geom_point(aes(color = Species)) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(
    x = "duration",
    y = "peak freq") +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
grunts  

FD_py1$Low.Freq..Hz.
# Plot all species knocks
knocks<-ggplot(FD_py1, aes(x = Low.Freq..Hz., y = High.Freq..Hz.)) +
  geom_point(aes(color = Species)) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(
       x = "Low Freq",
       y = "High Freq") +
  theme_bw() +
  theme(legend.position = "right")  # Show legend on the right
knocks  

