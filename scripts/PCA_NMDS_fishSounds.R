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