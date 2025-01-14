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

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_20250114.csv", header = TRUE)

fishdataT<-fishdata%>%
  filter(Selection == 4345)


##################################################################
###look at length vs sound characteristics by fish species############
fishdata00<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1,  Selection != 3030 )

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

fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence != 3,  Selection != 3030, str_detect(Species, "melanops|flavidus") ) #selection 3030 is a major outlier #str_detect(Species, "maliger|caurinus|melanops")

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


pca_all$Peak.Freq..Hz.
# Plot main variables from gruntss pca
grunts_pca<-ggplot(pca_all, aes(x = Dur.50...s., y = Peak.Freq..Hz.)) +
  geom_point(aes(color = Species)) +  # Color by Species
  #geom_text(aes(label = PCA_ID), vjust = -0.5, hjust = 0.5, size = 3) +  # Add PCA_ID labels
  labs(title = "PCA Plot of Fish grunts - ID Confidence = 1",
       x = "Duration (seconds)",
       y = "Center Frequency (Hz)") +
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
