#SPL calculations for AMAR data
#Darienne Lancaster - Jan 15, 2026

##############################################################################
# First load functions and packages ----
source("scripts/install_packages.R")

lp("tidyverse")
lp("lubridate")
lp("seewave")

source('PAM-R/PAMGuide-edited.R')
source('PAM-R/Meta-edited.R')
source('PAM-R/format_dates.R')
source('PAM-R/Viewer.R')	

##############################################################################
# AMAR device identifier (optional, not used in timestring)
amar_id <- "AMAR173"

# AMAR hydrophone calibration value (NEGATIVE)
calib_value <- -175.7   # <-- replace with your AMAR calibration

##############################################################################
# # Time interval for SPL (Welch window = seconds × 2)
 set_welch <- 120  # 1-minute resolution

# 30-minute SPL (two SPL values per file)
#set_welch <- 1800   

##############################################################################
# Frequency band (boat/noise band - as defined by Xavier Mouy's dissertation)
set_lcut <- 20
set_hcut <- 1000

##############################################################################
# Run SPL calculations on AMAR data files
#NOTE: audio files must be in water to work, if you include files from before or after deployment they will not process.

PAMMeta(
  atype = "Broadband",      # SPL
  outwrite = 1,
  calib = 1,
  envi = "Wat",
  ctype = "EE",             # END-TO-END calibration (Sysgain)
  Si = calib_value,
  lcut = set_lcut,
  hcut = set_hcut,
  welch = set_welch,
  plottype = "None",
  
  # AMAR filename format:
  # AMAR173.4.20220908T210952Z.wav
  timestring = "AMAR173.4.%Y%m%dT%H%M%SZ.wav"
)


#############################################################################
#Data output
#edit results to show data as correct timestamp and rename dataframe and column headers

#open full results table from Files tab (navigate to folder where data was output and open file - 
#name should be something like Conk_AcousticTest_Abs_Broadband_96000ptHannWindow_5000pcOlap)
#when you open file rename as SPL

# Set the directory path where your .rds files are located
directory_path <- "odata/SPL_LargeArray/SPL_DR_fulldeployment/Meta_AMAR173.4.32000.M36-V35-100_Broadband_Abs_32000ptHannWindow_50pcOlap"  # Update this with the correct path

# Get a list of all .rds files in the directory
rds_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)

# Load all .rds files into a list
loaded_files <- lapply(rds_files, readRDS)


# Check if each loaded file is a data frame and convert if necessary
# If files aren't data frames, we might need to adjust this depending on the structure of the data
loaded_files <- lapply(loaded_files, function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  return(x)
})

# Bind all data frames by columns (assuming they have the same number of rows)
SPL <- do.call(rbind, loaded_files)

###
#Taylor Islet SPL

#make SPL a dataframe and rename columns and add site name as column
Site_name <- "TI"

SPL_TI<-SPL%>%
  rename(Date = V1, SPL = V2)%>%
  mutate(Date = as.POSIXct(Date, origin = "1970-01-01")) %>%# note: numeric dates are based on origin = "1970-01-01" (this will fix the strange date column)
  filter(format(Date, "%Y") != "1969")%>%  # Remove rows where the year is 1969
  filter(SPL != "Inf")%>%
  mutate(Site_name = Site_name)

write.csv(SPL_TI, file = "odata/SPL_LargeArray/SPL_1min_fulldeployment_TI_20to1000Hz.csv", row.names = FALSE)

########
#Ohiat Island SPL

#make SPL a dataframe and rename columns and add site name as column
Site_name <- "OH"

SPL_OH<-SPL%>%
  rename(Date = V1, SPL = V2)%>%
  mutate(Date = as.POSIXct(Date, origin = "1970-01-01")) %>%# note: numeric dates are based on origin = "1970-01-01" (this will fix the strange date column)
  filter(format(Date, "%Y") != "1969")%>%  # Remove rows where the year is 1969
  filter(SPL != "Inf")%>%
  mutate(Site_name = Site_name)

write.csv(SPL_OH, file = "odata/24hrAnnotations_AllSites_Selection_Tables/SPL/SPL_OH_20to1000Hz.csv", row.names = FALSE)

######
#Danger Rocks SPL

#make SPL a dataframe and rename columns and add site name as column
Site_name <- "DR"

SPL_DR<-SPL%>%
  rename(Date = V1, SPL = V2)%>%
  mutate(Date = as.POSIXct(Date, origin = "1970-01-01")) %>%# note: numeric dates are based on origin = "1970-01-01" (this will fix the strange date column)
  filter(format(Date, "%Y") != "1969")%>%  # Remove rows where the year is 1969
  filter(SPL != "Inf")%>%
  mutate(Site_name = Site_name)

write.csv(SPL_DR, file = "odata/SPL_LargeArray/SPL_1min_fulldeployment_DR_20to1000Hz.csv", row.names = FALSE)

#slice(-1)  # This removes the first row (getting strange date column at beginning with SPL value - check with Steph and Philina)

##############################################################################
# Export the filtered data to a CSV file

#30min SPL files
#rename each file to be the exact deployment/array name from your other data
#write.csv(SPL, file = "wdata/SPL1/Array_1_Deployment_1.csv", row.names = FALSE) #You'll need to create folders for SPL1_fish and SPL1_boat


#1min SPL files
#rename each file to be the exact deployment/array name from your other data
write.csv(SPL1, file = "wdata/boat_background/Array_2_Deployment_13.csv", row.names = FALSE)


################################################################################
#load all .csv SPL files back in as one file

#30 min SPL files
#################
# Define the directory containing the CSV files
#folder_path <- "wdata/SPL30"


# List all CSV files in the directory
#file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files and combine them into a single data frame with a column for Site_name pulled from the filenames you exported above.
#SPL30_All <- file_list %>%
#  lapply(function(file) {
#    data <- read.csv(file)                                   # Read the CSV file
#    data$Site_name <- sub("\\.csv$", "", basename(file))    # Remove .csv from the file name
#    return(data)                                            # Return the modified data frame
#  }) %>%
#  bind_rows() %>%
#  distinct(Site_name, .keep_all = TRUE)  # Keep only the first row for each unique Site_name (this retains only the first 30 minute SPL as this was what was annotated)

#write.csv(SPL30_All, file = "wdata/SPL30_All.csv", row.names = FALSE)

#1min SPL files
###############
# Define the directory containing the CSV files
folder_path <- "wdata/boat_background"



# List all CSV files in the directory
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)


# Read all CSV files and combine them into a single data frame with a column for Site_name pulled from the filenames you exported above.
SPL_boat_background <- file_list %>%
  lapply(function(file) {
    data <- read.csv(file)                                   # Read the CSV file
    data$Site_name <- sub("\\.csv$", "", basename(file))    # Remove .csv from the file name
    return(data)                                            # Return the modified data frame
  }) %>%
  bind_rows() %>%
  group_by(Site_name) %>%
  slice_head(n = 30) %>%  # Keep the first 30 rows for each unique Site_name (keep values for first 30min that were annotated)
  ungroup()               # Ungroup the data frame


rms1 <- SPL_boat_background %>%
  group_by(Site_name) %>%
  summarize(
    rms_SPLB = rms(SPL))

write.csv(rms1, file = "wdata/SPL_boat_background_ALL.csv", row.names = FALSE)

