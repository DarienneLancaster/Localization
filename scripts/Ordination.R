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

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_20250205.csv", header = TRUE)

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
  select(Selection, Begin.Time..s., End.Time..s., fishID, Species, Begin.File, End, Selection_N, Edit)%>%
  group_by(fishID) %>%
  mutate(
    Sequence_ID = cumsum(
      Begin.Time..s. > lag(End, default = first(End)) + 2 | is.na(lag(End))
    ) + 2  # Incremental sequence for each group
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

fishdata999<-left_join(fishdata98, fishdata998, by = c("fishID", "Sequence_ID"))%>%
  mutate(Sequence_Reps = count) %>%
  select(-count)  # Remove the intermediate 'count' column, if not needed


#histogram of calling interval (time between linked calls) by species 
ggplot(fishdata999, aes(x = Call_Interval)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Interval by Species", x = "Call Interval", y = "Count") +
  coord_cartesian(ylim = c(0, 20)) +  # Fix y-axis range from 0 to 8
  theme_classic()

#histogram of calling repetition (within a sequence) by species 
ggplot(fishdata999, aes(x = Sequence_Reps)) +
  geom_histogram(binwidth = 1, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Repetition by Species", x = "Call Repetition Rate", y = "Count") +
  coord_cartesian(ylim = c(0, 75)) +  # Fix y-axis range from 0 to 8
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
fishdata00<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1,  Selection != 3030 )

fishdata00$Center.Freq..Hz.

# Get the unique species from the 'Species' column of the fishdata00 dataframe
species_list <- unique(fishdata00$Species)

# Print the species list
print(species_list)

# Create an empty list to store the plots
plot_list <- list()

# Loop through each species and create a plot
for(species_name in species_list) {
  # Subset the data for the current species
  species_data <- fishdata00 %>%
    filter(Species == species_name)
  
  # Create the scatter plot
  p <- ggplot(species_data, aes(x = mean_length, y =Center.Freq..Hz. )) +
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
