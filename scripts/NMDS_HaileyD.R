# NMDS Code

#### Ordination ####

# Goal = visualize relationships between rockfish species, barotrauma symptoms, & other variables of interest
# Initial variables of interest:
# species, barotrauma (TA, BM, ME, PE, OE, EE), capture depth, fish length 

# Possible other variables: subgenus, surface time, surface temp

## Method - NMDS ##
# PCA most useful when species linearly related, which make it unsuitable for most ecological datasets
# PCA descriptors should be quantitative, multinormal, not too many zeros
# NMDS is more robust to ecological data, plots dissimilar objects far apart & similar objects close

# New dataframe
nmds <- subset(just_rockfish, select = c("Species", "Depth.capture..ft.", "TL..mm.", 
                                         "TA", "BM", "ME", "PE", "OE", "EE"))
write.csv(nmds, "nmds.csv") # removed 1 row w missing barotrauma data from csv :(
nmds2 <- read.csv("nmds2.csv")

# Change barotrauma values to "0" & "1"
nmds2 <- nmds2 %>%
  dplyr::mutate_at(vars("TA", "BM", "ME", "PE", "OE", "EE"), ~ ifelse(. == "y", 1, ifelse(. == "n", 0, NA)))

# Remove barotrauma data with only zeros
nmds3 <- nmds2 %>% 
  filter(rowSums(. == 1) > 0)

nrow(nmds2)
nrow(nmds3) # 31 rockfish removed (+1 missing already removed above)

# Set a seed
set.seed(14) # number doesn't actually matter

# Dataset with just barotrauma variables
baro <- nmds3[, c("TA", "BM", "ME", "PE", "OE", "EE")]

# Try NMDS analysis & check result
NMDS1 <- metaMDS(baro, k = 2, trymax = 100, trace = FALSE)
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

