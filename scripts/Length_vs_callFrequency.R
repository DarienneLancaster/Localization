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

#create new column with species common names
fishdata$Common<- ifelse(fishdata$Species == "caurinus", "Copper rockfish",
                         ifelse(fishdata$Species == "maliger", "Quillback rockfish",
                                ifelse(fishdata$Species == "pinniger", "Canary rockfish",
                                       ifelse(fishdata$Species == "miniatus", "Vermillion rockfish",
                                              ifelse(fishdata$Species == "melanops", "Black rockfish",
                                                     ifelse(fishdata$Species == "elongatus", "Lingcod",
                                                            ifelse(fishdata$Species == "decagrammus", "Kelp Greenling",
                                                                   ifelse(fishdata$Species == "vacca", "Pile Perch", "other"))))))))

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
