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

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250616.csv", header = TRUE)

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

length_knocks_all<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger|vacca|elongatus|melanops|pinniger"))

length_knocks_CP<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))

length_knocks_QB<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))

length_knocks_CA<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "pinniger"))

length_knocks_BL<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "melanops"))

length_knocks_L<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "elongatus"))

length_knocks_PP<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "vacca"))



#copper
CK<-lm(freq_pct50~mean_length, data = length_knocks_CP)
summary(CK)

#quillback
QBK<-lm(freq_pct50~mean_length, data = length_knocks_QB)
summary(QBK)

#canary
CAK<-lm(freq_pct50~mean_length, data = length_knocks_CA)
summary(CAK)

#black
BLK<-lm(freq_pct50~mean_length, data = length_knocks_BL)
summary(BLK)

#ling
LK<-lm(freq_pct50~mean_length, data = length_knocks_L)
summary(LK)

#pile perch
PPK<-lm(freq_pct50~mean_length, data = length_knocks_PP)
summary(PPK)

# Define custom colors for species
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)


##
lp('purrr')
lp('broom')

#Linear regression Frequency 50% vs all species (KNOCKS)

# Determine significance and assign linetypes
linetype_map <- length_knocks_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct50 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_knocks_all_linetype <- left_join(length_knocks_all, linetype_map, by = "Common")

knocks_all_50 <- ggplot(length_knocks_all_linetype, aes(x = mean_length, y = freq_pct50, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Frequency 50% (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

knocks_all_50

#Linear regression Peak Frequency  vs all species (KNOCKS)

# Determine significance and assign linetypes
linetype_map <- length_knocks_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_peak ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.07, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_knocks_all_linetype <- left_join(length_knocks_all, linetype_map, by = "Common")

knocks_all_peak <- ggplot(length_knocks_all_linetype, aes(x = mean_length, y = freq_peak, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Peak frequency (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

knocks_all_peak

#Linear regression Frequency 25 vs all species (KNOCKS)

# Determine significance and assign linetypes
linetype_map <- length_knocks_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct25 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.07, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_knocks_all_linetype <- left_join(length_knocks_all, linetype_map, by = "Common")

knocks_all_25 <- ggplot(length_knocks_all_linetype, aes(x = mean_length, y = freq_pct25, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Frequency 25% (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

knocks_all_25

#Linear regression Frequency 75 vs all species (KNOCKS)

# Determine significance and assign linetypes
linetype_map <- length_knocks_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct75 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.07, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_knocks_all_linetype <- left_join(length_knocks_all, linetype_map, by = "Common")

knocks_all_75 <- ggplot(length_knocks_all_linetype, aes(x = mean_length, y = freq_pct75, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Frequency 75% (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

knocks_all_75


lp("patchwork")
lp("cowplot")

# Combine plots
K_length_combined <- (knocks_all_50 |knocks_all_peak ) /
  (knocks_all_25| knocks_all_75) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

K_length_final <- ggdraw() +
  # Title at the top
  draw_label("Knocks", fontface = "bold", x = 0.1, y = 0.98, size = 16, hjust = 0.5) +
  # Shared y-axis label (rotated)
  draw_label("", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Mean length (mm)", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(K_length_combined, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

K_length_final
ggsave("figures/CH2/Length_Freq_KnocksLMplot.png", plot = K_length_final, width = 10, height = 6, dpi = 300)

############################
#Length vs grunts all species

##################################################################
###look at length vs sound characteristics by fish species############
lp("dplyr")

length_grunts_all<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1,  Selection != 3030, str_detect(Species, "caurinus|maliger|melanops"))


##
lp('purrr')
lp('broom')

#Linear regression Frequency 50% vs all species (GRUNTS)

# Define custom colors for species
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

# Determine significance and assign linetypes
linetype_map <- length_grunts_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct50 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_grunts_all_linetype <- left_join(length_grunts_all, linetype_map, by = "Common")

grunts_all_50 <- ggplot(length_grunts_all_linetype, aes(x = mean_length, y = freq_pct50, color = Common)) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Common, linetype = linetype), method = "lm", se = FALSE) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  labs(
    x = "",
    y = "Frequency 50% (Hz)"
  ) +
  theme_classic()

grunts_all_50

#Linear regression Peak Frequency  vs all species (grunts)

# Determine significance and assign linetypes
linetype_map <- length_grunts_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_peak ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_grunts_all_linetype <- left_join(length_grunts_all, linetype_map, by = "Common")

grunts_all_peak <- ggplot(length_grunts_all_linetype, aes(x = mean_length, y = freq_peak, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.3) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Peak frequency (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

grunts_all_peak

#Linear regression Frequency 25 vs all species (grunts)

# Determine significance and assign linetypes
linetype_map <- length_grunts_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct25 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_grunts_all_linetype <- left_join(length_grunts_all, linetype_map, by = "Common")

grunts_all_25 <- ggplot(length_grunts_all_linetype, aes(x = mean_length, y = freq_pct25, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.3) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Frequency 25% (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

grunts_all_25

#Linear regression Frequency 75 vs all species (grunts)

# Determine significance and assign linetypes
linetype_map <- length_grunts_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(freq_pct75 ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_grunts_all_linetype <- left_join(length_grunts_all, linetype_map, by = "Common")

grunts_all_75 <- ggplot(length_grunts_all_linetype, aes(x = mean_length, y = freq_pct75, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.3) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "",
    y = "Frequency 75% (Hz)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()

grunts_all_75


lp("patchwork")
lp("cowplot")

# Combine plots
G_length_combined <- (grunts_all_50 |grunts_all_peak ) /
  (grunts_all_25| grunts_all_75) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

G_length_final <- ggdraw() +
  # Title at the top
  draw_label("Grunts", fontface = "bold", x = 0.1, y = 0.98, size = 16, hjust = 0.5) +
  # Shared y-axis label (rotated)
  draw_label("", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Mean length (mm)", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(G_length_combined, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

G_length_final
ggsave("figures/CH2/Length_Freq_gruntsLMplot.png", plot = G_length_final, width = 10, height = 6, dpi = 300)


##


#try UMAP data visualization
###########


#NOTES to self

#add in repetition rate, call interval as predictors based on behaviour
#change behaviour names to be more descriptive


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))%>%
  dplyr::select(Common, Activity, Site, mean_length, freq_peak:time_centroid, High.Freq..Hz., Low.Freq..Hz.)%>%
  mutate(Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity))%>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Flight"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  dplyr::select(-freq_flatness)


  

# Extract feature matrix (excluding response)
X_test <- Behav_UMAP[, !(names(Behav_UMAP) %in% c("Common", "Activity", "Site", "mean_length"))]
X_test
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
#umap_df$True <- test$Common
# umap_df$Site<-test_wExtra$Site
# umap_df$fishID<- test_wExtra$fishID
levels(as.factor(umap_df$Activity))

# Define custom colors for species
custom_colors <- c(
  "Chase" = "#003399",   
  "Flight" = "#FF6600", 
  "No activity" = "#33CC99",
  "Approach" = "#33CCFF",
  "Feeding" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

par(mfrow = c(1, 2))
###########

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_QB <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Quillback Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_QB

###Seeing if two groups are length related between Flight and 
# #Predicted values plot (shows how the Random Forest classified fish sounds)
# Behaviour_QB <- ggplot(umap_df, aes(V1, V2, color = mean_length, fill =mean_length, shape = Site)) +
#   geom_point(alpha = 0.8, size = 2) +
#   stat_ellipse(aes(group = mean_length), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
#   stat_ellipse(aes(group = mean_length), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
#   #scale_color_manual(values = custom_colors) +
#   #scale_fill_manual(values = custom_colors) +
#   labs(title = "Quillback Grunts",
#        x = "UMAP 1", y = "UMAP 2", fill = "Behaviour") +
#   theme_classic()+
#   theme(legend.position = "right")
# Behaviour_QB

ggsave("figures/CH2/UMAP_Behaviour_QuillbackKnock.png", plot = Behaviour_QB, width = 10, height = 6, dpi = 300)

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_C <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_C

ggsave("figures/CH2/UMAP_Behaviour_CopperKnock.png", plot = Behaviour_C, width = 10, height = 6, dpi = 300)

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_B <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_B

ggsave("figures/CH2/UMAP_Behaviour_Black.png", plot = Behaviour_B, width = 10, height = 6, dpi = 300)


#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_L <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_L

ggsave("figures/CH2/UMAP_Behaviour_LingcodKnock.png", plot = Behaviour_L, width = 10, height = 6, dpi = 300)

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_PP <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_PP

ggsave("figures/CH2/UMAP_Behaviour_PilePerchKnock.png", plot = Behaviour_PP, width = 10, height = 6, dpi = 300)

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_Can <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Can

ggsave("figures/CH2/UMAP_Behaviour_CanKnock.png", plot = Behaviour_Can, width = 10, height = 6, dpi = 300)










# Create summary table of linear model results per 'Common' level
lm_summary_table <- length_knocks_all %>%
  group_by(Common) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(freq_pct50 ~ mean_length, data = .x)),
    summary = map(model, tidy)
  ) %>%
  unnest(summary) %>%
  select(Common, term, estimate, std.error, statistic, p.value)

# View results
print(lm_summary_table)

CK<-lm(freq_pct50~mean_length, data = length_knocks_all)
summary(CK)

knocks_all <- ggplot(length_knocks_all, aes(x = mean_length, y = freq_peak)) +
  geom_point(aes(color = Common)) +
  geom_jitter(aes(color = Common), width = 0.2, height = 0) +
  geom_smooth(aes(group = Common, color = Common), method = "lm", se = FALSE) +
  labs(
    title = paste(species_name),
    x = "Mean Length",
    y = "Center Frequency"
  ) +
  theme_bw()
knocks_all

length_knocks_CQB<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger"),)

knocks_CQB <- ggplot(length_knocks_CQB, aes(x = mean_length, y = freq_pct50)) +
  geom_point(aes(color = Common)) +
  geom_jitter(aes(color = Common), width = 0.2, height = 0) +
  geom_smooth(aes(group = Common, color = Common), method = "lm", se = FALSE) +
  labs(
    title = paste(species_name),
    x = "Mean Length",
    y = "Center Frequency"
  ) +
  theme_bw()
knocks_CQB

length_grunts_CQB<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger"),)

grunts_CQB <- ggplot(length_grunts_CQB, aes(x = mean_length, y = freq_pct50)) +
  geom_point(aes(color = Common)) +
  geom_jitter(aes(color = Common), width = 0.2, height = 0) +
  geom_smooth(aes(group = Common, color = Common), method = "lm", se = FALSE) +
  labs(
    x = "Mean Length",
    y = "Center Frequency"
  ) +
  theme_bw()
grunts_CQB

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
