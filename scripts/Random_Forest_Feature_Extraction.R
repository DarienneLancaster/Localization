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
#the package orca must be downloaded using miniconda to export static images from plotly
######

###########################################################################
#write loop to pull in all Raventables one by one, remove anything not marked 
#as a fish sound or unknown noise and then reexport as raven table.


# Set the path to the folder where your Raven tables are stored
folder_path <- "odata/Random_Forest_Feature_Extraction/DR_E"
out_folder_path<- "odata/Random_Forest_Feature_Extraction/DR_E_fs_only"

# Get a list of all Raven selection tables (.txt files) in the folder
file_list <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
file_list

raven_data$`Low Freq (Hz)`

# Loop through each file
for (file in file_list) {
  # Import the Raven selection table
  raven_data<-imp_raven(path = folder_path, all.data =  TRUE, only.spectro.view = FALSE, files = basename(file))

  # Filter the rows where column 's' is either "f" or "e"
  raven_data_filtered <- raven_data %>%
    dplyr::select(-"selec.file")%>% #remove duplicate column
    mutate(
      `Low Freq (Hz)` = ifelse(`Low Freq (Hz)` < 25,
                               `Low Freq (Hz)` + 25,
                               `Low Freq (Hz)`))%>%
    # mutate(
    #   `End Time (s)` = ifelse(`End Time (s)` < `Begin Time (s)` + 0.11, 
    #                           `Begin Time (s)` + 0.11, 
    #                           `End Time (s)`))%>%
    mutate(selec = Selection, sound.files = `Begin File`, start = `Begin Time (s)`, end = `End Time (s)`)%>% #needed to add these column for it to be exported correctly as a Raven table, not sure why but it works.
    filter(s %in% c("f", "e"))#filter to keep only fish sounds and  unknown sounds
  
  # Create the new file name by adding '_RF' before the file extension
  new_file_name <- gsub("(_E|\\.E)\\.txt$", "_RF.txt", basename(file)) #use for changing edited selection tables
  #new_file_name <- gsub(".txt$", "_RF.txt", basename(file))
  new_file_path <- file.path(out_folder_path)
  sound_path<- "F:/Bamfield_2022_Large_Array/DR20220908_Danger_Rocks_Danvers/AMAR/AMAR173.4.32000.M36-V35-100"
  
  # Export the filtered table back as a Raven selection table with the new name
  exp_raven(raven_data_filtered, path= new_file_path, file.name = new_file_name, single.file = TRUE, sound.file.path = sound_path)
  
  cat("Processed and saved:", new_file_name, "\n")
}

#additional attempts at making the RF code run by adjusting selection tables

#   mutate(across(5:6, ~ . + 0.0001)) %>%
#   mutate(across(25:26, ~ . + 0.0001))%>%
# mutate(across(55, ~ . + 0.0001))%>%
# mutate(across(72:73, ~ . + 0.0001))%>%
# mutate(across(78:85, ~ . + 0.0001))%>%
#dplyr::select(-"Dur 50% (s)")%>%
#mutate(across(where(is.numeric), ~ . + 0.0001))%>%
#mutate(`Delta Time (s)` = ifelse(`Delta Time (s)` < 0.12, `Delta Time (s)` + 0.01, `Delta Time (s)`))%>%  # Add 0.01 if Delta_time < 0.12
