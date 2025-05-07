# NMDS Code
#original code by Hailey Davies

####Load Packages######
source("scripts/install_packages.R")#bring in install_packages script
lp("vegan")
lp("ggplot2")

#### Ordination ####

## Method - NMDS ##
# PCA most useful when species linearly related, which make it unsuitable for most ecological datasets
# PCA descriptors should be quantitative, multinormal, not too many zeros
# NMDS is more robust to ecological data, plots dissimilar objects far apart & similar objects close

nmds1<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250221.csv", header = TRUE)

#filter dataframe

#shortened dataframe with important idenitfying info included
nmds2<-nmds1%>%
  select(-Begin.Time..s., -a, -selec.file.x, -selec.file.y, -Begin.Date.y, -(Cam:CamName),
         -(Date:Genus), -(Code:fishnum), -Comments, -tottime, -(Selection:Channel), -(Begin.Path:uuid))

#filter data for species, confidence interval, call type, etc...

nmds3<- nmds2%>%
  filter(ID_confidence == 1, t == "d")

#nmds dataframe with only useable variables

# nmds<- nmds3%>%
#   select(Species, t, (Low.Freq..Hz.:time_centroid))

nmds4<- nmds3%>%
  select(Species, (Low.Freq..Hz.:freq_entropy), (freq_roughness:time_centroid))%>%
  drop_na()

#Do not need to scale for nmds (this can be detrimental to analysis)
nmds5<- nmds4%>%
  select((Low.Freq..Hz.:freq_entropy), (freq_roughness:time_centroid))%>%
  drop_na()

m_nmds<-as.matrix(nmds5)


########################
#bray-curtis nmds for fish knocks with 1 ID_confidence
# copying code from here https://jkzorz.github.io/2020/04/04/NMDS-extras.html

#run NMDS using bray curtist distance metric (need to play with this since I have negative values - try euclidean)
set.seed(123)
nmds<-metaMDS(m_nmds, distance = "bray")
nmds #prints results of nmds (ideally want stress to be below 0.1)

#calculate how much variables are contriubting to axes (nmds is results of test above, nmds5 is just pulling in your predictor variables)
en = envfit(nmds, nmds5 , permutations = 999, na.rm = TRUE)
en #see r2 values of each variable in model
#get coordinates of each point in the nmds
data.scores = as.data.frame(scores(nmds))

#create dataframe with species names only to add to nmds results
Species<-nmds4%>%
  #drop_na()%>%
  select(Species)

#create dataframe with species included with coordinates for plotting
data.scores1<-cbind(Species, data.scores)

#get loadings of each variable
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)

#keep only the variables with r2 >.9 (can check this by running en)
en_coord_cont_filtered <- en_coord_cont[rownames(en_coord_cont) %in% c("freq_bandwidth90", "freq_pct5", "freq_pct25",
                                                                       "freq_pct50","freq_pct75","freq_pct95","freq_entropy",
                                                                       "freq_centroid", "freq_entropy_mean"), ]


# plot nmds with 80% elipses, loading arrows, and coloured by species
gg <- ggplot(data.scores1, aes(x = NMDS1, y = NMDS2)) +
  # Add ellipses with shading inside
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Species, fill = Species)) +  # alpha controls transparency of the shading
  geom_point(aes(colour = Species), size = 1, alpha = 0.5) +  # Smaller points
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont_filtered, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont_filtered, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont_filtered)) + 
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "MDS1", y = "MDS2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink"))  # Fill colors match species
gg

########################
#euclidean nmds for fish knocks with 1 ID_confidence
# copying code from here https://jkzorz.github.io/2020/04/04/NMDS-extras.html

#run NMDS using euclidean distance metric 
set.seed(123)
nmds<-metaMDS(m_nmds, distance = "euclidean")
nmds #prints results of nmds (ideally want stress to be below 0.1)

#calculate how much variables are contriubting to axes (nmds is results of test above, nmds5 is just pulling in your predictor variables)
en = envfit(nmds, nmds5 , permutations = 999, na.rm = TRUE)
en #see r2 values of each variable in model
#get coordinates of each point in the nmds
data.scores = as.data.frame(scores(nmds))

#create dataframe with species names only to add to nmds results
Species<-nmds4%>%
  #drop_na()%>%
  select(Species)

#create dataframe with species included with coordinates for plotting
data.scores1<-cbind(Species, data.scores)

#get loadings of each variable
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)

#keep only the variables with r2 >.9 (can check this by running en)
en_coord_cont_filtered <- en_coord_cont[rownames(en_coord_cont) %in% c("High.Freq..Hz.","freq_bandwidth90", 
                                                                       "freq_pct50","freq_pct75","freq_pct95",
                                                                       "freq_centroid"), ]


# plot nmds with 80% elipses, loading arrows, and coloured by species
gg <- ggplot(data.scores1, aes(x = NMDS1, y = NMDS2)) +
  # Add ellipses with shading inside
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2, aes(colour = Species, fill = Species)) +  # alpha controls transparency of the shading
  geom_point(aes(colour = Species), size = 1, alpha = 0.5) +  # Smaller points
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont_filtered, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont_filtered, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont_filtered)) + 
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "MDS1", y = "MDS2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink"))  # Fill colors match species
gg

################################

#calculate euclidean distance

# Assuming 'nmds' is your data frame
# Step 2: Compute Euclidean distance matrix
euclidean_dist <- dist(nmds, method = "euclidean")

Species<-nmds4%>%
  #drop_na()%>%
  select(Species)

# Step 3: Run NMDS ordination using the Euclidean distance matrix
#nmds_result <- metaMDS(nmds, distance = "euclidean")


# Step 3: Run NMDS ordination using the Euclidean distance matrix
nmds_result <- metaMDS(euclidean_dist, distance = "euclidean")



###############################

# Step 4: View the NMDS result
print(nmds_result)

# Get the NMDS coordinates (scores)
nmds_scores <- as.data.frame(scores(nmds_result))

ED_Species<-cbind(Species, nmds_scores)


# Step 1: Create a ggplot of the NMDS scores
ggplot(ED_Species, aes(x = NMDS1, y = NMDS2, color = Species, fill = Species)) +
  # Add ellipses with shading inside
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2) +  # alpha controls transparency of the shading
  geom_point(size = 1.5) +  # Smaller points
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "MDS1", y = "MDS2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "grey", "pink"))  # Fill colors match species

### add loadings

# Assuming 'ED_Species' contains your NMDS results and 'nmds_result' is your NMDS object

# Extract the NMDS loadings (variable scores) for the axes (MDS1, MDS2)
nmds_loadings <- scores(nmds_scores, display = "species")

# Convert the loadings into a data frame
loadings_df <- as.data.frame(nmds_loadings)

# Create a ggplot of the NMDS scores
ggplot(ED_Species, aes(x = NMDS1, y = NMDS2, color = Species, fill = Species)) +
  # Add ellipses with shading inside
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2) +  # alpha controls transparency of the shading
  geom_point(size = 1.5) +  # Smaller points
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "MDS1", y = "MDS2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black")) +  # Fill colors match species
  
  # Add variable loading arrows (arrows indicating how variables load on the ordination)
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
               color = "black", size = 0.5) +
  
  # Add labels for the variables (loadings)
  geom_text(data = loadings_df, aes(x = NMDS1, y = NMDS2, label = rownames(loadings_df)), 
            color = "black", size = 3, vjust = 1, hjust = 1)


#########################################################
#calculate Bray-Curtis distance

#might need to scale variables

bray_curtis_dist <- vegdist(nmds, method = "bray")

Species<-nmds4%>%
  #drop_na()%>%
  select(Species)

# Step 3: Run NMDS ordination using the Euclidean distance matrix
nmds_result <- metaMDS(bray_curtis_dist, distance = "bray")

# Step 4: View the NMDS result
print(nmds_result)

# Step 5: Plot the NMDS result
plot(nmds_result)

# Optional: Extract and view NMDS coordinates for each sample
nmds_coordinates <- as.data.frame(scores(nmds_result))
print(nmds_coordinates)

# Load necessary libraries
library(vegan)
library(ggplot2)

# Assuming you've already run metaMDS and have 'nmds_result' object from metaMDS
# Get the NMDS coordinates (scores)
nmds_scores <- as.data.frame(scores(nmds_result))

# If you have a 'species' or group column in your original 'nmds' dataframe, e.g.:
# Let's assume the column 'species' exists in the original 'nmds' dataframe
nmds_scores$species <- nmds$species  # Replace 'species' with your actual column name
ED_Species<-cbind(Species, nmds_scores)

# Step 1: Create a ggplot of the NMDS scores
ggplot(ED_Species, aes(x = NMDS1, y = NMDS2, color = Species, fill = Species)) +
  # Add ellipses with shading inside
  stat_ellipse(level = 0.80, geom = "polygon", alpha = 0.2) +  # alpha controls transparency of the shading
  geom_point(size = 1.5) +  # Smaller points
  labs(title = "NMDS Ordination with 80% Confidence Ellipses and Shading", x = "MDS1", y = "MDS2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "pink")) +  # Customize colors
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "orange", "black", "pink"))  # Fill colors match species


####################################
#original PCA code
#it works but doesn't look as good as nmds plot

################

pca<- rda(nmds)

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


pca_all<-cbind(Species, pca1)

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
















##########################################################################################################
# Set a seed
set.seed(14) # number doesn't actually matter

# # Dataset with just barotrauma variables
# baro <- nmds3[, c("TA", "BM", "ME", "PE", "OE", "EE")]

# Try NMDS analysis & check result
NMDS1 <- metaMDS(nmds, k = 2, trymax = 100, trace = FALSE)
NMDS1
# ?metaMDS # in vegan package

# Stress plot
stressplot(NMDS1)
# good non-metric fit between observed dissimilarities & the distances in ordination space
# stress of our final result low (0.05) = good fit

# Plot results
plot(NMDS1)
plot(NMDS1, type = "t")

# Dataset with just species variables
sp <- nmds3[, c("Species", "Depth.capture..ft.", "TL..mm.")]

# envfit adds the species variables as vectors to the plot
ef <- envfit(NMDS1, sp, permu = 999)
ef
# ?envfit # in vegan package

# Depth & TL are both of interest so we can plot & interpret
plot(NMDS1, type = "t", display = "sites")
plot(ef, p.max = 0.05)

# Rename dataframe
df <- data.frame(NMDS1$points)


## NMDS with ellipses ##

## Species

# Subset species with enough data
# More species were tried but the NMDS would not run
nmds_ellipse <- nmds3 %>% filter(Species %in% c("copper", "quillback", "yelloweye"))

# Dataset with just species variables
sp <- nmds_ellipse[, c("Species", "Depth.capture..ft.", "TL..mm.")]

# Dataset with just barotrauma variables
baro <- nmds_ellipse[, c("TA", "BM", "ME", "PE", "OE", "EE")]

# NMDS analysis & check result
NMDS_sub <- metaMDS(baro, k = 2, trymax = 100, trace = FALSE)

# Make new dataframe
df <- data.frame(NMDS_sub$points)

df$Species <- sp$Species
df$Depth <- sp$Depth.capture..ft.
df$Length <- sp$TL..mm.

# Subset by species
cp <- df[df$Species == "copper",][chull(df[df$Species == "copper", c("MDS1", "MDS2")]),]
qb <- df[df$Species == "quillback",][chull(df[df$Species == "quillback", c("MDS1", "MDS2")]),]
ye <- df[df$Species == "yelloweye",][chull(df[df$Species == "yelloweye", c("MDS1", "MDS2")]),]

# Add barotrauma symptom labels
baro_symp <- as.data.frame(scores(NMDS_sub, display = "species"))
baro_symp$baro_symp <- c("TA", "BM", "ME", "PE", "OE", "EE")

baro_symp$MDS1 <- baro_symp$NMDS1
baro_symp$MDS2 <- baro_symp$NMDS2

# Make sure species are factors
df$Species <- as.factor(df$Species)

# Create ellipses
hull_cyl <- df %>%
  group_by(Species) %>%
  slice(chull(MDS1, MDS2))

veganCovEllipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
plot.new()

ord <- ordiellipse(NMDS_sub, df$Species, kind = "se", conf = 0.95, label = TRUE)

df_ell <- data.frame()
head(df_ell)

for(g in levels(df$Species)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(df[df$Species == g,], 
                                                   veganCovEllipse(ord[[g]]$cov, ord[[g]]$center,                                                                    ord[[g]]$scale))), group = g))
}

# envfit adds the species variables as vectors to the plot
ef2 <- envfit(NMDS_sub, sp, permu = 999)
ef2

# Vector arrows for depth & length
arrows <- as.data.frame(scores(ef2, display = "vectors"))
arrows <- cbind(arrows, env.variables = rownames(arrows))

# Report version
my_colors = c("#FE40DB", "#FF8017", "#12B3F5")
new_labels <- c("TA", "BM", "ME", "PE", "OE", "EE")
new_labels2 <- c("Depth", "Length")

ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Species, shape = Species, fill = Species), 
              width = 0.08, height = 0.08, alpha = 0.6, size = 4) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), 
               alpha = 0.5, lwd = 1) +
  scale_fill_manual(name = "Species", values = my_colors, 
                    labels = c("Copper", "Quillback", "Yelloweye")) +
  scale_color_manual(name = "Species", values = my_colors, 
                     labels = c("Copper", "Quillback", "Yelloweye")) +
  scale_shape_manual(name = "Species", values = 15:17, 
                     labels = c("Copper", "Quillback", "Yelloweye"),
                     guide = guide_legend(override.aes = list(color = my_colors, fill = my_colors))) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) +
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), 
                  size = 7, color = "black") +
  geom_segment(data = arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, 
               color = "black") + # depth & length arrows
  geom_text_repel(data = arrows, aes(x = NMDS1, y = NMDS2, label = new_labels2), 
                  fontface = "bold", nudge_x = -0.25, nudge_y = 0.05, size = 7, 
                  segment.color = "transparent", color = "black") + 
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) +
  labs(x = "NMDS1", y = "NMDS2")
# ggsave("nmds_species.png", width = 25, height = 16, units = "cm", dpi = 600, bg = "white")


# Presentation version (just dots)
ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Species), 
              width = 0.08, height = 0.08, alpha = 0.6, size = 3) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), alpha = 0.5, lwd = 0.75) +
  scale_fill_manual(name = "Species", values = my_colors, labels = c("Copper", "Quillback", "Yelloweye")) +
  scale_color_manual(name = "Species", values = my_colors, labels = c("Copper", "Quillback", "Yelloweye")) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) +
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), size = 4, color = "white") +
  geom_segment(data = arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, color = "white") + # depth & length arrows
  geom_text_repel(data = arrows, aes(x = NMDS1, y = NMDS2, label = new_labels2), fontface = "bold",
                  nudge_x = -0.2, nudge_y = 0.05, size = 4, segment.color = "transparent", color = "white") + 
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  theme(axis.line = element_line(colour = "white"), 
        axis.title = element_text(colour = "white", size = 22),
        axis.text = element_text(colour = "white", size = 20), 
        legend.title = element_text(colour = "white", size = 20),
        legend.text = element_text(colour = "white", size = 20), 
        axis.ticks = element_line(colour = "white"))
# ggsave("nmds_species_pres.png", width = 25, height = 15, units = "cm", dpi = 300, bg = "transparent")

# Presentation version (shapes)
ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Species, 
                             shape = Species, fill = Species), 
              width = 0.08, height = 0.08, alpha = 0.6, size = 4) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), 
               alpha = 0.5, lwd = 0.75) +
  scale_fill_manual(name = "Species", values = my_colors, 
                    labels = c("Copper", "Quillback", "Yelloweye")) +
  scale_color_manual(name = "Species", values = my_colors, 
                     labels = c("Copper", "Quillback", "Yelloweye")) +
  scale_shape_manual(name = "Species", values = 15:17,
                     labels = c("Copper", "Quillback", "Yelloweye"),
                     guide = guide_legend(override.aes = list(color = my_colors, fill = my_colors))) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) +
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), 
                  size = 4, color = "white") +
  geom_segment(data = arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, 
               color = "white") + # depth & length arrows
  geom_text_repel(data = arrows, aes(x = NMDS1, y = NMDS2, label = new_labels2), 
                  fontface = "bold", nudge_x = -0.2, nudge_y = 0.05, size = 4, 
                  segment.color = "transparent", color = "white") + 
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  theme(axis.line = element_line(colour = "white"), 
        axis.title = element_text(colour = "white", size = 22),
        axis.text = element_text(colour = "white", size = 20), 
        legend.title = element_text(colour = "white", size = 20),
        legend.text = element_text(colour = "white", size = 20), 
        axis.ticks = element_line(colour = "white"))
# ggsave("nmds_species_pres.PNG", width = 25, height = 15, units = "cm", dpi = 300, bg = "transparent")


## Depth

# Create depth bins
nmds3$Depth_bin <- ""
nmds3$Depth_bin[nmds3$Depth.capture..ft. >= 0 & nmds3$Depth.capture..ft. <=  50] <- "0-50"
nmds3$Depth_bin[nmds3$Depth.capture..ft. > 50 & nmds3$Depth.capture..ft. <= 100 ] <- "50-100"
nmds3$Depth_bin[nmds3$Depth.capture..ft. > 100 & nmds3$Depth.capture..ft. <= 150 ] <- "100-150"
nmds3$Depth_bin[nmds3$Depth.capture..ft. > 150 & nmds3$Depth.capture..ft. <= 200 ] <- "150-200"
nmds3$Depth_bin[nmds3$Depth.capture..ft. > 200 & nmds3$Depth.capture..ft. <= 260 ] <- "200-260"

# New dataframe with all species included
nmds_ellipse <- nmds3

# Dataset with just barotrauma variables
baro <- nmds_ellipse[, c("TA", "BM", "ME", "PE", "OE", "EE")]

# NMDS analysis & check result
NMDS_depth <- metaMDS(baro, k = 2, trymax = 100, trace = FALSE)

# Make new dataframe
df <- data.frame(NMDS_depth$points)

# Dataset with just species variables
sp <- nmds_ellipse[, c("Species", "Depth.capture..ft.", "TL..mm.", "Depth_bin")]
df$Depth_bin <- sp$Depth_bin

# Add barotrauma symptom labels
baro_symp <- as.data.frame(scores(NMDS_depth, display = "species"))
baro_symp$baro_symp <- c("TA", "BM", "ME", "PE", "OE", "EE")

baro_symp$MDS1 <- baro_symp$NMDS1
baro_symp$MDS2 <- baro_symp$NMDS2

# Make sure depth bins are factors
df$Depth_bin <- as.factor(df$Depth_bin)

# Create ellipses
hull_cyl <- df %>%
  group_by(Depth_bin) %>%
  slice(chull(MDS1, MDS2))

veganCovEllipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
plot.new()

ord <- ordiellipse(NMDS_depth, df$Depth_bin, kind = "se", conf = 0.95, label = TRUE)

df_ell <- data.frame()
head(df_ell)

for(g in levels(df$Depth_bin)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(df[df$Species == g,],
                                                   veganCovEllipse(ord[[g]]$cov, ord[[g]]$center,                                                                    ord[[g]]$scale))), group = g))
}

# envfit adds the species variables as vectors to the plot
ef2 <- envfit(NMDS_depth, sp, permu = 999)
ef2

# Vector arrows for depth & length
arrows <- as.data.frame(scores(ef2, display = "vectors"))
arrows <- cbind(arrows, env.variables = rownames(arrows))

arrows_length <- arrows %>% filter(env.variables == "TL..mm.")

# Convert Depth_bin to a factor with specified order
df$Depth_bin <- factor(df$Depth_bin, levels = c("0-50", "50-100", "100-150", "150-200", "200-260"))
df_ell$group <- factor(df_ell$group, levels = c("0-50", "50-100", "100-150", "150-200", "200-260"))

# Report version
my_colors = c("#FE40DB", "#FC5D62", "#FF8017", "#889FD2", "#12B3F5")
new_labels <- c("TA", "BM", "ME", "PE", "OE", "EE")
new_labels3 <- c("Length")

ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Depth_bin,shape = Depth_bin, fill = Depth_bin),
              width = 0.08, height = 0.08, alpha = 0.6, size = 4) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), 
               alpha = 0.5, lwd = 1) +
  scale_fill_manual(name = "Depth bin", values = my_colors,
                    labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  scale_color_manual(name = "Depth bin", values = my_colors,
                     labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  scale_shape_manual(name = "Depth bin", values = c(15, 16, 17, 18, 8),
                     labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft"),
                     guide = guide_legend(override.aes = list(color = my_colors, fill = my_colors))) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) + # barotrauma labels
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), 
                  size = 7, color = "black") +
  geom_segment(data = arrows_length, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, 
               color = "black") + # depth & length arrows
  geom_text_repel(data = arrows_length, aes(x = NMDS1, y = NMDS2, label = new_labels3), 
                  fontface = "bold", nudge_x = -0.25, nudge_y = 0.05, size = 7, 
                  segment.color = "transparent", color = "black") +
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) +
  labs(x = "NMDS1", y = "NMDS2")
# ggsave("nmds_depth.PNG", width = 25, height = 16, units = "cm", dpi = 600, bg = "white")


# Presentation version (just circles)
ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Depth_bin),
              width = 0.08, height = 0.08, alpha = 0.6, size = 4) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), alpha = 0.5, lwd = 0.75) +
  scale_fill_manual(name = "Depth bin", values = my_colors,
                    labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  scale_color_manual(name = "Depth bin", values = my_colors,
                     labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) + # barotrauma labels
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), size = 4, color = "white") +
  geom_segment(data = arrows_length, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, color = "white") + # depth & length arrows
  geom_text_repel(data = arrows_length, aes(x = NMDS1, y = NMDS2, label = new_labels3), fontface = "bold",
                  nudge_x = -0.2, nudge_y = 0.05, size = 4, segment.color = "transparent", color = "white") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  theme(axis.line = element_line(colour = "white"),
        axis.title = element_text(colour = "white", size = 23),
        axis.text = element_text(colour = "white", size = 20),
        legend.title = element_text(colour = "white", size = 20),
        legend.text = element_text(colour = "white", size = 20),
        axis.ticks = element_line(colour = "white"))
# ggsave("nmds_depth_pres.PNG", width = 25, height = 15, units = "cm", dpi = 600, bg = "transparent")

# Presentation version (shapes)
ggplot() +
  geom_jitter(data = df, aes(x = MDS1, y = MDS2, color = Depth_bin,
                             shape = Depth_bin, fill = Depth_bin),
              width = 0.08, height = 0.08, alpha = 0.6, size = 4) +
  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, fill = group, color = group), 
               alpha = 0.5, lwd = 0.75) +
  scale_fill_manual(name = "Depth bin", values = my_colors,
                    labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  scale_color_manual(name = "Depth bin", values = my_colors,
                     labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft")) +
  scale_shape_manual(name = "Depth bin", values = c(15, 16, 17, 18, 8),
                     labels = c("0-50 ft", "50-100 ft", "100-150 ft", "150-200 ft", "> 200 ft"),
                     guide = guide_legend(override.aes = list(color = my_colors, fill = my_colors))) +
  geom_point(data = baro_symp, aes(x = MDS1, y = MDS2), shape = NA) + # barotrauma labels
  geom_text_repel(data = baro_symp, aes(x = MDS1, y = MDS2, label = new_labels), 
                  size = 4, color = "white") +
  geom_segment(data = arrows_length, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), size = 0.75, 
               color = "white") + # depth & length arrows
  geom_text_repel(data = arrows_length, aes(x = NMDS1, y = NMDS2, label = new_labels3), 
                  fontface = "bold", nudge_x = -0.2, nudge_y = 0.05, size = 4, 
                  segment.color = "transparent", color = "white") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  theme(axis.line = element_line(colour = "white"),
        axis.title = element_text(colour = "white", size = 23),
        axis.text = element_text(colour = "white", size = 20),
        legend.title = element_text(colour = "white", size = 20),
        legend.text = element_text(colour = "white", size = 20),
        axis.ticks = element_line(colour = "white"))
# ggsave("nmds_depth_pres.PNG", width = 25, height = 15, units = "cm", dpi = 300, bg = "transparent")


## NMDS with polygons ##

## Doesn't work well with these data, leaving code in case useful ##

# Subset
br <- df[df$Species == "brown",][chull(df[df$Species == "brown", c("MDS1", "MDS2")]),]
cn <- df[df$Species == "canary",][chull(df[df$Species == "canary", c("MDS1", "MDS2")]),]
cp <- df[df$Species == "copper",][chull(df[df$Species == "copper", c("MDS1", "MDS2")]),]
gs <- df[df$Species == "greenstriped",][chull(df[df$Species == "greenstriped", c("MDS1", "MDS2")]),]
qb <- df[df$Species == "quillback",][chull(df[df$Species == "quillback", c("MDS1", "MDS2")]),]
tg <- df[df$Species == "tiger",][chull(df[df$Species == "tiger", c("MDS1", "MDS2")]),]
vm <- df[df$Species == "vermilion",][chull(df[df$Species == "vermilion", c("MDS1", "MDS2")]),]
ye <- df[df$Species == "yelloweye",][chull(df[df$Species == "yelloweye", c("MDS1", "MDS2")]),]
yt <- df[df$Species == "yellowtail",][chull(df[df$Species == "yellowtail", c("MDS1", "MDS2")]),]
hb <- df[df$Species == "hybrid",][chull(df[df$Species == "hybrid", c("MDS1", "MDS2")]),]

# Polygons
hull.data <- rbind(cp, qb, ye)
# hull.data <- rbind(br, cn, cp, gs, qb, tg, vm, ye, yt, hb) # all species

# Add barotrauma symptom labels
baro_symp <- as.data.frame(scores(NMDS1, display = "species"))
baro_symp$baro_symp <- c("TA", "BM", "ME", "PE", "OE", "EE")

baro_symp$MDS1 <- baro_symp$NMDS1
baro_symp$MDS2 <- baro_symp$NMDS2

# Plot (adapted from Micah's code)
ggplot(data = df, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = hull.data,aes(x = MDS1, y = MDS2, fill = Species, group = Species), alpha = 0.25) +
  theme(axis.title = element_text(size = 12, face = "bold", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "black"), 
        legend.text = element_text(size = 10, colour = "black")) + 
  labs(colour = "Species") + 
  geom_text_repel(data =  baro_symp, aes(x = MDS1, y = MDS2, label =  baro_symp), size = 4)

