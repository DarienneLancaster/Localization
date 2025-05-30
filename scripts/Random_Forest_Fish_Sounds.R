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
lp("randomForest")
lp("caret")
lp("lattice")
lp("gridExtra")

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

##########################################################################
#try random forest on fish grunts 

#(MULTICLASS)

############################
#ORIGINAL DATA - UNBALANCED

#keep only ID confidence 1 for copper and black, ID confidence 1 and 2 for quillbacks to increase sample size. 
fishdata0 <- fishdata %>%
  filter(
    t == "g",
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|maliger"),
    (
      # Keep ID_confidence == 1 generally
      ID_confidence == 1 |
        # Exception: quillback can be 1 or 2
        (str_detect(Species, "maliger") & ID_confidence %in% c(1, 2))
    ))

numSpecies<-fishdata0 %>%
  count(Species) %>%
  arrange(desc(n))  # Optional: sort by count
numSpecies

fishdata1<-fishdata0%>%
  dplyr::select(Species, Site, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common, Site, fishID, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train_wExtra <- fishdata2[train_index, ] #includes Site and fishID #
test_wExtra <- fishdata2[-train_index, ] #includes Site and fishID #

train<- train_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)
test<- test_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)

###
set.seed(123)  # For reproducibility
# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$'Actual Class' <- rownames(conf_df)
conf_df<-conf_df%>%
  rename(Error=class.error)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_grunt_UNBALANCED_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot
#############

# Extract and format importance data
imp <- randomForest::importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_UNBALANCED_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
#############
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats


# 3. Add row names as a new column for clarity
class_stats$Class <- rownames(class_stats)
rownames(class_stats) <- NULL  # optional: reset row names
class_stats<-class_stats%>%
  select(-Sensitivity, -Specificity, -`Pos Pred Value`, -`Neg Pred Value`)%>%
  mutate(Class = str_replace(Class, "^Class:", "")) %>%
  rename('F Score' = F1, 'Grunt Class' = Class)


# 4. Reorder columns to show actual class first
class_stats <- class_stats[, c(ncol(class_stats), 1:(ncol(class_stats)-1))]
print(class_stats)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

class_stats_flextable <- flextable(class_stats)
class_stats_flextable <- colformat_double(
  x = class_stats_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

class_stats_flextable  <- line_spacing(class_stats_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
class_stats_flextable  <- set_table_properties(class_stats_flextable, align = "right", layout = "autofit")
class_stats_flextable <- theme_vanilla(class_stats_flextable)
class_stats_flextable
save_as_image(x = class_stats_flextable, path = "figures/rf_grunt_TEST_RESULTS_confusion_matrix.png")


###
################################################################################
#try UMAP data visualization
#############

lp("umap")

# Predict class labels on test set
rf_preds <- predict(rf_model, newdata = test, type = "response")

# Extract feature matrix (excluding response)
X_test <- test[, !(names(test) %in% c("Common"))]
X_test

set.seed(55)

# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Predicted <- rf_preds
umap_df$True <- test$Common
umap_df$Site<-test_wExtra$Site
umap_df$fishID<- test_wExtra$fishID

par(mfrow = c(1, 2))
###########
#NOTE- change so all species have same colour in both plots
# Define custom colors for each class
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99"  
)

#Predicted values plot (shows how the Random Forest classified fish sounds)
pred<-ggplot(umap_df, aes(V1, V2, color = Predicted, fill = Predicted, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA) +
  stat_ellipse(level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Random Forest Grunt Predictions",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "none")
pred

#True values plot (shows how well the groups align with true species classifications)
true<-ggplot(umap_df, aes(V1, V2, color = True, fill = True, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA) +
  stat_ellipse(level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "True Species Grunt Classifications",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()

UMAP_grunts_unbalanced<- pred+true
UMAP_grunts_unbalanced
ggsave("figures/UMAP_Grunt_UNBALANCED_IDconf1_1n2QB_bySite.png", plot = UMAP_grunts_unbalanced, width = 10, height = 6, dpi = 300)
###
################
# Partial dependence plot coloured by species
#################
set.seed(123)
rfo <- randomForest(Common ~ ., data = train, ntree = 1000, importance = TRUE)
rfo
# Prediction wrapper that returns class probabilities
pfun <- function(object, newdata) {
  colMeans(predict(object, newdata = newdata, type = "prob"))
}

##################
#PDP all species together - TOP 6 GINI sound features
###############

#Freq_pct25
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct25", pred.fun = pfun)

custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99"  
)

G_freq25<-ggplot(p, aes(x = freq_pct25, y = yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 25% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_freq25

#Frequency  5

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
G_freq5<-ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 5% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_freq5
#Low Frequency

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "Low.Freq..Hz.", pred.fun = pfun)
G_Low<-ggplot(p, aes(Low.Freq..Hz., yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Low Frequency (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_Low

#Frequency  50

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct50", pred.fun = pfun)
G_freq50<-ggplot(p, aes(freq_pct50, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 50% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_freq50
  
#Frequency  Peak

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_peak", pred.fun = pfun)
G_peak<-ggplot(p, aes(freq_peak, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Peak Frequency (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_peak

#Frequency  Centroid

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_centroid", pred.fun = pfun)
G_centroid<-ggplot(p, aes(freq_centroid, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency Centroid (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
G_centroid

lp("patchwork")
lp("cowplot")

# Combine plots
G_PDP_combined <- (G_freq25 | G_Low | G_freq5) /
  (G_freq50 | G_peak | G_centroid) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

G_PDP_final <- ggdraw() +
  # Shared y-axis label (rotated)
  draw_label("Probability", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Sound Feature", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(G_PDP_combined, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

G_PDP_final
ggsave("figures/PDP_Grunt_Unbalanced_Top6GINI.png", plot = G_PDP_final, width = 10, height = 6, dpi = 300)

###############
#species by Species PDP
##################
# 
# 
# # Filter to just Quillback rockfish
# p_quill <- p %>% filter(yhat.id == "Quillback rockfish")
# 
# # Plot
# quill<-ggplot(p_quill, aes(freq_pct25, yhat)) +
#   geom_line(color = "#FF6600", size = 0.8) +
#   theme_classic() +
#   labs(title = "Quillback rockfish",y = "Probability", x = "")+
#   coord_cartesian(ylim = c(0, 0.5))
# quill
# 
# # Filter to just Copper rockfish
# p_copper <- p %>% filter(yhat.id == "Copper rockfish")
# 
# # Plot
# copper<-ggplot(p_copper, aes(freq_pct25, yhat)) +
#   geom_line(color = "#33CC99", size = 0.8) +
#   theme_classic() +
#   labs(title = "Copper rockfish",y = "", x = "freq_pct25")+
#   coord_cartesian(ylim = c(0, 0.5))
# copper
# 
# # Filter to just Black rockfish
# p_black <- p %>% filter(yhat.id == "Black rockfish")
# 
# # Plot
# black<-ggplot(p_black, aes(freq_pct25, yhat)) +
#   geom_line(color = "#003399", size = 0.8) +
#   theme_classic() +
#   labs(title = "Black rockfish",y = "", x = "")+
#   coord_cartesian(ylim = c(0, 0.5))
# black
# PDP<-grid.arrange(quill,copper,black, nrow = 1)
# PDP

#Low Frequency
# 
# # Partial dependence of probability for each class on petal width
# p <- partial(rfo, pred.var = "Low.Freq..Hz.", pred.fun = pfun)
# ggplot(p, aes(Low.Freq..Hz., yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
# 
# # Filter to just Quillback rockfish
# p_quill <- p %>% filter(yhat.id == "Quillback rockfish")
# 
# # Plot
# quill2<-ggplot(p_quill, aes(Low.Freq..Hz., yhat)) +
#   geom_line(color = "firebrick1", size = 0.8) +
#   theme_classic() +
#   labs(y = "Probability", x = "")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# quill2
# 
# # Filter to just Copper rockfish
# p_copper <- p %>% filter(yhat.id == "Copper rockfish")
# 
# # Plot
# copper2<-ggplot(p_copper, aes(Low.Freq..Hz., yhat)) +
#   geom_line(color = "seagreen3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = "Low.Freq..Hz.")+
#   coord_cartesian(ylim =c(0.25, 0.4))
# copper2
# 
# # Filter to just Black rockfish
# p_black <- p %>% filter(yhat.id == "Black rockfish")
# 
# # Plot
# black2<-ggplot(p_black, aes(Low.Freq..Hz., yhat)) +
#   geom_line(color = "dodgerblue3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = ".")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# black2
# PDP2<-grid.arrange(quill2,copper2,black2, nrow =1)
# PDP2
# 
# #Freq_pct5
# # Partial dependence of probability for each class on petal width
# p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
# ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
# 
# # Filter to just Quillback rockfish
# p_quill <- p %>% filter(yhat.id == "Quillback rockfish")
# 
# # Plot
# quill3<-ggplot(p_quill, aes(freq_pct5, yhat)) +
#   geom_line(color = "firebrick1", size = 0.8) +
#   theme_classic() +
#   labs(y = "Probability", x = "")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# quill3
# 
# # Filter to just Copper rockfish
# p_copper <- p %>% filter(yhat.id == "Copper rockfish")
# 
# # Plot
# copper3<-ggplot(p_copper, aes(freq_pct5, yhat)) +
#   geom_line(color = "seagreen3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = "freq_pct5")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# copper3
# 
# # Filter to just Black rockfish
# p_black <- p %>% filter(yhat.id == "Black rockfish")
# 
# # Plot
# black3<-ggplot(p_black, aes(freq_pct5, yhat)) +
#   geom_line(color = "dodgerblue3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = "")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# black3
# PDP3<-grid.arrange(quill3,copper3,black3, nrow = 1)
# PDP3
# 
# 
# #Time roughness
# 
# # Partial dependence of probability for each class on petal width
# p <- partial(rfo, pred.var = "time_roughness", pred.fun = pfun)
# ggplot(p, aes(time_roughness, yhat, color = yhat.id)) +
#   geom_line() +
#   theme(legend.title = element_blank())
# 
# # Filter to just Quillback rockfish
# p_quill <- p %>% filter(yhat.id == "Quillback rockfish")
# 
# # Plot
# quill4<-ggplot(p_quill, aes(time_roughness, yhat)) +
#   geom_line(color = "firebrick1", size = 0.8) +
#   theme_classic() +
#   labs(y = "Probability", x = "")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# quill4
# 
# # Filter to just Copper rockfish
# p_copper <- p %>% filter(yhat.id == "Copper rockfish")
# 
# # Plot
# copper4<-ggplot(p_copper, aes(time_roughness, yhat)) +
#   geom_line(color = "seagreen3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = "time_roughness")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# copper4
# 
# # Filter to just Black rockfish
# p_black <- p %>% filter(yhat.id == "Black rockfish")
# 
# # Plot
# black4<-ggplot(p_black, aes(time_roughness, yhat)) +
#   geom_line(color = "dodgerblue3", size = 0.8) +
#   theme_classic() +
#   labs(y = "", x = "")+
#   coord_cartesian(ylim = c(0.25, 0.4))
# black4
# PDP4<-grid.arrange(quill4, copper4, black4, nrow=1)
# PDP4
# 
# 
# 
# PDP_Grunts_Balanced <- grid.arrange(PDP, PDP2, PDP3, PDP4, ncol = 1)
# 
# ggsave("figures/PDP_Grunt_BALANCED_GiniTop4bySpecies.png", plot = PDP_Grunts_Balanced, width = 10, height = 6, dpi = 300)
# 

#####################################
#BALANCED DATA - upsampled with replacement

##############################################################################
#keep only ID confidence 1 for pinniger (all other pinniger grunts are actually blacks)
fishdata0<-fishdata%>%
  dplyr::filter(t == "g", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|melanops|maliger") )
###

numSpecies<-fishdata0 %>%
  count(Species) %>%
  arrange(desc(n))  # Optional: sort by count
numSpecies

fishdata1<-fishdata0%>%
  dplyr::select(Species, Site, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

# Get max sample size across groups
max_n <- fishdata1 %>%
  count(Species) %>%
  summarise(max_n = max(n)) %>%
  pull(max_n)

# Sample with replacement to equalize group sizes
balanced_data <- fishdata1 %>%
  group_by(Species) %>%
  slice_sample(n = 200, replace = TRUE) %>% #upsample all data so there are 200 datapoints per species
  ungroup()

#random forest version
fishdata2<-balanced_data%>%
  dplyr::select(Common, Site, fishID, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train_wExtra <- fishdata2[train_index, ] #includes Site and fishID #
test_wExtra <- fishdata2[-train_index, ] #includes Site and fishID #

train<- train_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)
test<- test_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_grunt_BALANCED_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot
################

# Extract and format importance data
imp <- randomForest::importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_BALANCED_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
###############
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

################################################################################
#try UMAP data visualization
#######################

lp("umap")

# Predict class labels on test set
rf_preds <- predict(rf_model, newdata = test, type = "response")

# Extract feature matrix (excluding response)
X_test <- test[, !(names(test) %in% c("Common"))]
X_test
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Predicted <- rf_preds
umap_df$True <- test$Common
umap_df$Site<-test_wExtra$Site
umap_df$fishID<- test_wExtra$fishID

par(mfrow = c(1, 2))
###########
#NOTE- change so all species have same colour in both plots

#Predicted values plot (shows how the Random Forest classified fish sounds)
pred<-ggplot(umap_df, aes(V1, V2, color = Predicted, fill = Predicted, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA) +
  stat_ellipse(level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Random Forest Predictions",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()

#True values plot (shows how well the groups align with true species classifications)
true<-ggplot(umap_df, aes(V1, V2, color = True, fill = True, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA) +
  stat_ellipse(level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "True Species Classifications",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()

UMAP_grunts_balanced<- pred+true
UMAP_grunts_balanced
ggsave("figures/UMAP_Grunt_BALANCED_IDconf1and2_bySite.png", plot = UMAP_grunts_unbalanced, width = 10, height = 6, dpi = 300)

##########################################################################

# Partial dependence plot coloured by species
#################


lp("ranger")
# Fit a quick RF
set.seed(1143)  # for reproducibility
rfo <- ranger(Common ~ ., data = train, probability = TRUE)
print(rfo)

# Prediction wrapper that returns average prediction for each class
pfun <- function(object, newdata) {
  colMeans(predict(object, data = newdata)$predictions)
}

#Freq_pct25
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct25", pred.fun = pfun)
ggplot(p, aes(freq_pct25, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill<-ggplot(p_quill, aes(freq_pct25, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(title = "Quillback rockfish",y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
quill

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper<-ggplot(p_copper, aes(freq_pct25, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(title = "Copper rockfish",y = "", x = "freq_pct25")+
  coord_cartesian(ylim = c(0.25, 0.4))
copper

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black<-ggplot(p_black, aes(freq_pct25, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(title = "Black rockfish",y = "", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
black
PDP<-grid.arrange(quill,copper,black, nrow = 1)
PDP

#Low Frequency

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "Low.Freq..Hz.", pred.fun = pfun)
ggplot(p, aes(Low.Freq..Hz., yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill2<-ggplot(p_quill, aes(Low.Freq..Hz., yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
quill2

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper2<-ggplot(p_copper, aes(Low.Freq..Hz., yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "Low.Freq..Hz.")+
  coord_cartesian(ylim =c(0.25, 0.4))
copper2

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black2<-ggplot(p_black, aes(Low.Freq..Hz., yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = ".")+
  coord_cartesian(ylim = c(0.25, 0.4))
black2
PDP2<-grid.arrange(quill2,copper2,black2, nrow =1)
PDP2

#Freq_pct5
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill3<-ggplot(p_quill, aes(freq_pct5, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
quill3

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper3<-ggplot(p_copper, aes(freq_pct5, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "freq_pct5")+
  coord_cartesian(ylim = c(0.25, 0.4))
copper3

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black3<-ggplot(p_black, aes(freq_pct5, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
black3
PDP3<-grid.arrange(quill3,copper3,black3, nrow = 1)
PDP3


#Time roughness

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "time_roughness", pred.fun = pfun)
ggplot(p, aes(time_roughness, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill4<-ggplot(p_quill, aes(time_roughness, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
quill4

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper4<-ggplot(p_copper, aes(time_roughness, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "time_roughness")+
  coord_cartesian(ylim = c(0.25, 0.4))
copper4

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black4<-ggplot(p_black, aes(time_roughness, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.25, 0.4))
black4
PDP4<-grid.arrange(quill4, copper4, black4, nrow=1)
PDP4



PDP_Grunts_Balanced <- grid.arrange(PDP, PDP2, PDP3, PDP4, ncol = 1)

ggsave("figures/PDP_Grunt_BALANCED_GiniTop4bySpecies.png", plot = PDP_Grunts_Balanced, width = 10, height = 6, dpi = 300)


##############################################################################
#One vs. All

###########################
#Copper vs All - GRUNTS
###########################

fishdata0 <- fishdata %>%
  filter(
    t == "g",
    ID_confidence %in% c(1),
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|pinniger|maliger")
  ) %>%
  mutate(
    Common = ifelse(Common == "Copper rockfish", "Copper rockfish", "Other")
  )

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_COPPER_grunt_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot

# Extract and format importance data
imp <- importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_COPPER_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

#################################
#Plot ROC curve

# Predict probabilities on the test set
rf_probs <- predict(rf_model, newdata = test, type = "prob")

# Create ROC curve (e.g., for "Positive" class)
library(pROC)
roc_obj <- roc(response = test$Common, predictor = rf_probs[, "Copper rockfish"])

# Plot ROC curve
plot(roc_obj, main = "")

# Save base R ROC plot as PNG
png("figures/roc_COPPER_grunt.png", width = 800, height = 800, res = 150)
plot(smooth(roc_obj), main = "")
dev.off()

auc(roc_obj)  # AUC score


###########################
#Quillback vs All - GRUNTS
##############################

fishdata0 <- fishdata %>%
  filter(
    t == "g",
    ID_confidence %in% c(1),
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|pinniger|maliger")
  ) %>%
  mutate(
    Common = ifelse(Common == "Quillback rockfish", "Quillback rockfish", "Other")
  )

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_QUILLBACK_grunt_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot

# Extract and format importance data
imp <- importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_QUILLBACK_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

#################################
#Plot ROC curve

# Predict probabilities on the test set
rf_probs <- predict(rf_model, newdata = test, type = "prob")

# Create ROC curve (e.g., for "Positive" class)
library(pROC)
roc_obj <- roc(response = test$Common, predictor = rf_probs[, "Quillback rockfish"])

# Plot ROC curve
plot(roc_obj, main = "")

# Save base R ROC plot as PNG
png("figures/roc_QUILLBACK_grunt.png", width = 800, height = 800, res = 150)
plot(smooth(roc_obj), main = "")
dev.off()

auc(roc_obj)  # AUC score

###########################
#Canary vs All - Knocks
#########################

fishdata0 <- fishdata %>%
  filter(
    t == "g",
    ID_confidence %in% c(1, 2),
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|pinniger|maliger")
  ) %>%
  mutate(
    Common = ifelse(Common == "Canary rockfish", "Canary rockfish", "Other")
  )

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_CANARY_grunt_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot

# Extract and format importance data
imp <- importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_CANARY_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

#################################
#Plot ROC curve

# Predict probabilities on the test set
rf_probs <- predict(rf_model, newdata = test, type = "prob")

# Create ROC curve (e.g., for "Positive" class)
library(pROC)
roc_obj <- roc(response = test$Common, predictor = rf_probs[, "Canary rockfish"])

# Plot ROC curve
plot(smooth(roc_obj), main = "")

# Save base R ROC plot as PNG
png("figures/roc_CANARY_grunt.png", width = 800, height = 800, res = 150)
plot(smooth(roc_obj), main = "")
dev.off()

auc(roc_obj)  # AUC score

####################################
#Black vs All - Knocks
#######################################

fishdata0 <- fishdata %>%
  filter(
    t == "g",
    ID_confidence %in% c(1, 2),
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|pinniger|maliger")
  ) %>%
  mutate(
    Common = ifelse(Common == "Black rockfish", "Black rockfish", "Other")
  )

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_BLACK_grunt_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot

# Extract and format importance data
imp <- importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Grunt_BLACK_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

#################################
#Plot ROC curve

# Predict probabilities on the test set
rf_probs <- predict(rf_model, newdata = test, type = "prob")

# Create ROC curve (e.g., for "Positive" class)
library(pROC)
roc_obj <- roc(response = test$Common, predictor = rf_probs[, "Black rockfish"])

# Plot ROC curve
plot(smooth(roc_obj), main = "")

# Save base R ROC plot as PNG
png("figures/roc_BLACK_grunt.png", width = 800, height = 800, res = 150)
plot(smooth(roc_obj), main = "")
dev.off()

auc(roc_obj)  # AUC score

#######################################################################################

#KNOCKS

##########################################################################
#try random forest on fish knocks

#(MULTICLASS)

############################
#ORIGINAL DATA - UNBALANCED
#don't include Kelp Greenling because all IDs are just too uncertain
fishdata0 <- fishdata %>%
  filter(
    t == "d",
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|maliger|elongatus|pinniger|vacca"),
    (
      # Keep ID_confidence == 1 generally
      ID_confidence == 1 |
        # Exception: Lingcod and Kelp Greenling can be 1 or 2
        (str_detect(Species, "elongatus") & ID_confidence %in% c(1, 2))
    ))

numSpecies<-fishdata0 %>%
  count(Species) %>%
  arrange(desc(n))  # Optional: sort by count
numSpecies

fishdata1<-fishdata0%>%
  dplyr::select(Species, Site, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common, Site, fishID, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train_wExtra <- fishdata2[train_index, ] #includes Site and fishID #
test_wExtra <- fishdata2[-train_index, ] #includes Site and fishID #

train<- train_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)
test<- test_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)

###
set.seed(123)  # For reproducibility
# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)
# Print the model summary
print(rf_model)


# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

##

# 3. Add row names as a new column for clarity
conf_df$'Actual Class' <- rownames(conf_df)
conf_df<-conf_df%>%
  rename(Error=class.error)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
K_conf_df_flextable <- theme_vanilla(conf_df_flextable)
K_conf_df_flextable
save_as_image(x = K_conf_df_flextable, path = "figures/rf_knock_UNBALANCED_TRAINING_confusion_matrix.png")

##

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot
########

# Extract and format importance data
imp <- randomForest::importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/Knock_UNBALANCED_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
########
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)
conf_mat_TEST

##

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats


# 3. Add row names as a new column for clarity
class_stats$Class <- rownames(class_stats)
rownames(class_stats) <- NULL  # optional: reset row names
class_stats<-class_stats%>%
  select(-Sensitivity, -Specificity, -`Pos Pred Value`, -`Neg Pred Value`)%>%
  mutate(Class = str_replace(Class, "^Class:", "")) %>%
  rename('F Score' = F1, 'Knock Class' = Class)


# 4. Reorder columns to show actual class first
class_stats <- class_stats[, c(ncol(class_stats), 1:(ncol(class_stats)-1))]
print(class_stats)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

class_stats_flextable <- flextable(class_stats)
class_stats_flextable <- colformat_double(
  x = class_stats_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

class_stats_flextable  <- line_spacing(class_stats_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
class_stats_flextable  <- set_table_properties(class_stats_flextable, align = "right", layout = "autofit")
K_class_stats_flextable <- theme_vanilla(class_stats_flextable)
K_class_stats_flextable
save_as_image(x = K_class_stats_flextable, path = "figures/rf_knock_TEST_RESULTS_confusion_matrix.png")

#######
#Plot one v. All ROC curves
###############
rf_predsROC <- as.data.frame(predict(rf_model, newdata = test, type = "prob"))
rf_classes <- predict(rf_model, newdata = test)  # predicted classes (optional)
y_true <- test$Common  # true class labels

lp("pROC")
lp("ggplot2")
lp("reshape2")

# Convert actual labels to factor to match column names
y_true <- as.factor(y_true)
classes <- levels(y_true)

# Initialize list to store ROC curves
roc_list <- list()
auc_list <- c()

# For each class: compute One-vs-All ROC
for (cls in classes) {
  true_binary <- as.numeric(y_true == cls)
  prob <- rf_predsROC[[cls]]
  
  roc_obj <- roc(true_binary, prob)
  roc_list[[cls]] <- roc_obj
  auc_list[cls] <- auc(roc_obj)
}

# Plot using base R
plot(roc_list[[1]], col = 1, main = "One-vs-Rest ROC Curves", lwd = 2, legacy.axes = TRUE)
for (i in 2:length(roc_list)) {
  plot(roc_list[[i]], add = TRUE, col = i, lwd = 2)
}
legend("bottomright", legend = paste(classes, "(AUC =", round(auc_list, 2), ")"),
       col = 1:length(classes), lwd = 2)

################

##############
# # View full summary
# print(conf_mat_TEST)
# table(test$Common)
# 
# ##Get F score for all classes
# 
# # Extract precision and recall per class
# by_class <- conf_mat_TEST$byClass
# precision <- by_class[, "Pos Pred Value"]
# recall <- by_class[, "Sensitivity"]
# 
# # Compute F1 score per class
# f1_score <- 2 * (precision * recall) / (precision + recall)
# 
# # Combine in a table
# f1_table <- data.frame(
#   Class = rownames(by_class),
#   Precision = precision,
#   Recall = recall,
#   F1_Score = f1_score
# )
# 
# print(f1_table)
# 
# #may need to make tables later of model validation results for paper
# overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
# overall_stats
# class_stats <- as.data.frame(conf_mat_TEST$byClass)
# class_stats

################################################################################
#try UMAP data visualization
###########

lp("umap")

# Predict class labels on test set
rf_preds <- predict(rf_model, newdata = test, type = "response")

# Extract feature matrix (excluding response)
X_test <- test[, !(names(test) %in% c("Common"))]
X_test
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Predicted <- rf_preds
umap_df$True <- test$Common
umap_df$Site<-test_wExtra$Site
umap_df$fishID<- test_wExtra$fishID

par(mfrow = c(1, 2))
###########
#NOTE- change so all species have same colour in both plots
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

#Predicted values plot (shows how the Random Forest classified fish sounds)
pred <- ggplot(umap_df, aes(V1, V2, color = Predicted, fill = Predicted, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Predicted), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Predicted), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Random Forest Predictions",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "none")

#True values plot (shows how well the groups align with true species classifications)
true<-ggplot(umap_df, aes(V1, V2, color = True, fill = True, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = True), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = True), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "True Species Classifications",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()

UMAP_knocks_unbalanced<- pred+true
UMAP_knocks_unbalanced
ggsave("figures/UMAP_Knock_UNBALANCED_SiteID.png", plot = UMAP_knocks_unbalanced, width = 10, height = 6, dpi = 300)

####################
#PDP plots - Unbalanced Knocks
##########
##
# Set seed and fit random forest
set.seed(123)
rfo <- randomForest(Common ~ ., data = train, ntree = 1000, importance = TRUE)
rfo
# Prediction wrapper that returns class probabilities
pfun <- function(object, newdata) {
  colMeans(predict(object, newdata = newdata, type = "prob"))
}

# Define custom colors for species
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

p <- partial(rfo, pred.var = "freq_centroid", pred.fun = pfun)

# Plot partial dependence lines
K_centroid <- ggplot(p, aes(freq_centroid, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency Centroid (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))

# Show the plot
K_centroid
# ##

#Frequency  Median Mean

p <- partial(rfo, pred.var = "freq_median_mean", pred.fun = pfun)
K_median<-ggplot(p, aes(freq_median_mean, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Mean Median Frequency (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
K_median

#Frequency  50

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct50", pred.fun = pfun)
K_freq50<-ggplot(p, aes(freq_pct50, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 50% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
K_freq50

#Freq 25
p <- partial(rfo, pred.var = "freq_pct25", pred.fun = pfun)
K_freq25<-ggplot(p, aes(x = freq_pct25, y = yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 25% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
K_freq25

#Frequency  75

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct75", pred.fun = pfun)
K_freq75<-ggplot(p, aes(freq_pct75, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 75% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
K_freq75

#Frequency  5

# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
K_freq5<-ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_classic() +
  labs(
    title = "",
    y = "", 
    x = "Frequency 5% (Hz)", 
    color = "Species"
  ) +
  coord_cartesian(ylim = c(0, 0.5))
K_freq5

lp("patchwork")
lp("cowplot")

# Combine plots
K_PDP_combined <- (K_freq50 | K_centroid| K_median ) /
  (K_freq25| K_freq75 | K_freq5) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

K_PDP_final <- ggdraw() +
  # Shared y-axis label (rotated)
  draw_label("Probability", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Sound Feature", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(K_PDP_combined, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

K_PDP_final
ggsave("figures/PDP_Knock_Unbalanced_Top6GINI.png", plot = K_PDP_final, width = 10, height = 6, dpi = 300)


#####################################

#BALANCED DATA - upsampled with replacement
##############################################################################
#keep only ID confidence 1 for pinniger (all other pinniger grunts are actually blacks)
# fishdata0<-fishdata%>%
#   dplyr::filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|melanops|maliger|elongatus|miniatus|vacca|decagrammus|pinniger") )


fishdata0 <- fishdata %>%
  filter(
    t == "d",
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|maliger|elongatus|pinniger|vacca|decagrammus"),
    (
      Species %in% c("elongatus", "decagrammus") & ID_confidence %in% c(1, 2)
    ) |
      (
        !Species %in% c("elongatus", "decagrammus") & ID_confidence == 1
      )
  )


numSpecies<-fishdata0 %>%
  count(Species) %>%
  arrange(desc(n))  # Optional: sort by count
numSpecies

fishdata1<-fishdata0%>%
  dplyr::select(Species, Site, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

# Get max sample size across groups
max_n <- fishdata1 %>%
  count(Species) %>%
  summarise(max_n = max(n)) %>%
  pull(max_n)

# Sample with replacement to equalize group sizes
balanced_data <- fishdata1 %>%
  group_by(Species) %>%
  slice_sample(n = 300, replace = TRUE) %>% #upsample all data so there are 200 datapoints per species
  ungroup()

#random forest version
fishdata2<-balanced_data%>%
  dplyr::select(Common, Site, fishID, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train_wExtra <- fishdata2[train_index, ] #includes Site and fishID #
test_wExtra <- fishdata2[-train_index, ] #includes Site and fishID #

train<- train_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)
test<- test_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 2000, importance=TRUE)

# Print the model summary
print(rf_model)

# 1. View confusion matrix
conf_matrix <- rf_model$confusion

# 2. Convert to data frame
conf_df <- as.data.frame(conf_matrix)

# 3. Add row names as a new column for clarity
conf_df$Actual_Class <- rownames(conf_df)
rownames(conf_df) <- NULL  # optional: reset row names

# 4. Reorder columns to show actual class first
conf_df <- conf_df[, c(ncol(conf_df), 1:(ncol(conf_df)-1))]
print(conf_df)


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

conf_df_flextable <- flextable(conf_df)
conf_df_flextable <- colformat_double(
  x = conf_df_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)

conf_df_flextable  <- line_spacing(conf_df_flextable , space = 1.5, part = "all")
# countFS_table_flextable <- add_header_row(countFS_table_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable
save_as_image(x = conf_df_flextable, path = "figures/rf_knock_BALANCED_TRAINING_confusion_matrix.png")

# Set up side-by-side plotting area
par(mfrow = c(1, 2))

# Plot 1: Accuracy importance
varImpPlot(rf_model, type = 1, main = "")

# Plot 2: Gini importance
varImpPlot(rf_model, type = 2, main = "")

# Reset plotting layout
par(mfrow = c(1, 1))

#######################################################
#create variable importance plots in ggplot

# Extract and format importance data
imp <- randomForest::importance(rf_model)
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)

# Plot using ggplot2 with spaced/rotated labels
p1<-ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "Sound Features", y = "Mean Decrease Gini") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Mean Decrease Accuracy
p2 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "deepskyblue4") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and display side by side
combined_plot <- p1 + p2  # patchwork combines them
print(combined_plot)

ggsave("figures/knock_BALANCED_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

##############################################################################################################
# Test the Random Forest model
rf_preds <- predict(rf_model, newdata = test)
# Get confusion matrix
conf_mat_TEST <- confusionMatrix(rf_preds, test$Common)

# View full summary
print(conf_mat_TEST)

#may need to make tables later of model validation results for paper
overall_stats <- as.data.frame(t(conf_mat_TEST$overall))
overall_stats
class_stats <- as.data.frame(conf_mat_TEST$byClass)
class_stats

##########################################################################

# Partial dependence plot coloured by species
############
lp("ranger")
# Fit a quick RF
set.seed(1143)  # for reproducibility
rfo <- ranger(Common ~ ., data = train, probability = TRUE)
print(rfo)

# Prediction wrapper that returns average prediction for each class
pfun <- function(object, newdata) {
  colMeans(predict(object, data = newdata)$predictions)
}
fishdata$freq_std
#freq_std
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_std", pred.fun = pfun)
ggplot(p, aes(freq_std, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill<-ggplot(p_quill, aes(freq_std, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(title = "Quillback rockfish",y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.1, 0.20))
quill

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper<-ggplot(p_copper, aes(freq_std, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(title = "Copper rockfish",y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
copper

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black<-ggplot(p_black, aes(freq_std, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(title = "Black rockfish",y = "", x = "freq_std")+
  coord_cartesian(ylim = c(0.1, 0.2))
black

# Filter to just Black rockfish
p_canary <- p %>% filter(yhat.id == "Canary rockfish")
# Plot
canary<-ggplot(p_canary, aes(freq_std, yhat)) +
  geom_line(color = "orange", size = 0.8) +
  theme_classic() +
  labs(title = "Canary rockfish",y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
canary

# Filter to just Black rockfish
p_ling <- p %>% filter(yhat.id == "Lingcod")
# Plot
ling<-ggplot(p_ling, aes(freq_std, yhat)) +
  geom_line(color = "purple", size = 0.8) +
  theme_classic() +
  labs(title = "Lingcod",y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
ling

# Filter to just Black rockfish
p_pile <- p %>% filter(yhat.id == "Pile Perch")
# Plot
pile<-ggplot(p_pile, aes(freq_std, yhat)) +
  geom_line(color = "yellow3", size = 0.8) +
  theme_classic() +
  labs(title = "Pile Perch",y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
pile



PDP<-grid.arrange(quill,copper,black, canary, ling, pile, nrow = 1)
PDP
#################################
#freq_freq_median_mean
fishdata$freq_median_mean
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_median_mean", pred.fun = pfun)
ggplot(p, aes(freq_median_mean, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill2<-ggplot(p_quill, aes(freq_median_mean, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
quill2

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper2<-ggplot(p_copper, aes(freq_median_mean, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
copper2

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black2<-ggplot(p_black, aes(freq_median_mean, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "freq_median_mean")+
  coord_cartesian(ylim = c(0.1, 0.2))
black2

# Filter to just Black rockfish
p_canary <- p %>% filter(yhat.id == "Canary rockfish")
# Plot
canary2<-ggplot(p_canary, aes(freq_median_mean, yhat)) +
  geom_line(color = "orange", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
canary2

# Filter to just Black rockfish
p_ling <- p %>% filter(yhat.id == "Lingcod")
# Plot
ling2<-ggplot(p_ling, aes(freq_median_mean, yhat)) +
  geom_line(color = "purple", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
ling2

# Filter to just Black rockfish
p_pile <- p %>% filter(yhat.id == "Pile Perch")
# Plot
pile2<-ggplot(p_pile, aes(freq_median_mean, yhat)) +
  geom_line(color = "yellow3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
pile2

PDP2<-grid.arrange(quill2,copper2,black2,canary2,ling2,pile2, nrow =1)
PDP2
##########################
#freq_pct25
fishdata$freq_pct25
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct25", pred.fun = pfun)
ggplot(p, aes(freq_pct25, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill3<-ggplot(p_quill, aes(freq_pct25, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
quill3

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper3<-ggplot(p_copper, aes(freq_pct25, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
copper3

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black3<-ggplot(p_black, aes(freq_pct25, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "freq_pct25")+
  coord_cartesian(ylim = c(0.1, 0.2))
black3

# Filter to just Black rockfish
p_canary <- p %>% filter(yhat.id == "Canary rockfish")
# Plot
canary3<-ggplot(p_canary, aes(freq_pct25, yhat)) +
  geom_line(color = "orange", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
canary3

# Filter to just Black rockfish
p_ling <- p %>% filter(yhat.id == "Lingcod")
# Plot
ling3<-ggplot(p_ling, aes(freq_pct25, yhat)) +
  geom_line(color = "purple", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
ling3

# Filter to just Black rockfish
p_pile <- p %>% filter(yhat.id == "Pile Perch")
# Plot
pile3<-ggplot(p_pile, aes(freq_pct25, yhat)) +
  geom_line(color = "yellow3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
pile3

PDP3<-grid.arrange(quill3,copper3,black3,canary3, ling3, pile3, nrow = 1)
PDP3

######################################
#snr
fishdata$snr
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "snr", pred.fun = pfun)
ggplot(p, aes(snr, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

# Filter to just Quillback rockfish
p_quill <- p %>% filter(yhat.id == "Quillback rockfish")

# Plot
quill4<-ggplot(p_quill, aes(snr, yhat)) +
  geom_line(color = "firebrick1", size = 0.8) +
  theme_classic() +
  labs(y = "Probability", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
quill4

# Filter to just Copper rockfish
p_copper <- p %>% filter(yhat.id == "Copper rockfish")

# Plot
copper4<-ggplot(p_copper, aes(snr, yhat)) +
  geom_line(color = "seagreen3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
copper4

# Filter to just Black rockfish
p_black <- p %>% filter(yhat.id == "Black rockfish")

# Plot
black4<-ggplot(p_black, aes(snr, yhat)) +
  geom_line(color = "dodgerblue3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "snr")+
  coord_cartesian(ylim = c(0.1, 0.2))
black4

# Filter to just Black rockfish
p_canary <- p %>% filter(yhat.id == "Canary rockfish")
# Plot
canary4<-ggplot(p_canary, aes(snr, yhat)) +
  geom_line(color = "orange", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
canary4

# Filter to just Black rockfish
p_ling <- p %>% filter(yhat.id == "Lingcod")
# Plot
ling4<-ggplot(p_ling, aes(snr, yhat)) +
  geom_line(color = "purple", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
ling4

# Filter to just Black rockfish
p_pile <- p %>% filter(yhat.id == "Pile Perch")
# Plot
pile4<-ggplot(p_pile, aes(snr, yhat)) +
  geom_line(color = "yellow3", size = 0.8) +
  theme_classic() +
  labs(y = "", x = "")+
  coord_cartesian(ylim = c(0.1, 0.2))
pile4


PDP4<-grid.arrange(quill4, copper4, black4,canary4,ling4, pile4, nrow=1)
PDP4



PDP_Knocks_Balanced <- grid.arrange(PDP, PDP2, PDP3, PDP4, ncol = 1)

ggsave("figures/PDP_Knock_BALANCED_GiniTop4bySpecies.png", plot = PDP_Knocks_Balanced, width = 15, height = 10, dpi = 300)

################################################################################
#try UMAP data visualization

lp("umap")

# Predict class labels on test set
rf_preds <- predict(rf_model, newdata = test, type = "response")

# Extract feature matrix (excluding response)
X_test <- test[, !(names(test) %in% c("Common"))]

# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Predicted <- rf_preds
umap_df$True <- test$Common

ggplot(umap_df, aes(V1, V2, color = True, fill = True)) +
  geom_point(alpha = 0.8, size = 2) +
  # Shaded ellipse (fill only)
  stat_ellipse(level = 0.80, type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
  # Solid outline ellipse
  stat_ellipse(level = 0.80, type = "norm", geom = "path", size = 1) +
  labs(title = "Random Forest Predictions (UMAP projection)",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()








# Extract the first tree from the Random Forest model
# The 'getTree' function gives us the tree as a data frame
tree_1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# Print the first tree's structure
print(tree_1)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")


# Plot the tree (this step can be modified based on how detailed you want it to be)
plot(tree_1, type = "n")  # Create an empty plot
text(tree_1, use.n = TRUE, all = TRUE, cex = 0.8)  # Add tree text to the plot

#find the number of variable selected at each split (mtry) - you select the mtry with the lowest out of bag error (OOB)
mtry <- tuneRF(train[-1],train$Common, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#build model again with best mtry
set.seed(123)
rf_model2 <- randomForest(Common ~ ., data = train, mtry=best.m, importance = TRUE, ntree = 500)

print(rf_model2)
#Evaluate variable importance
importance(rf_model2) #higher the value the more important to the model
varImpPlot(rf_model2) #higher the value the more important to the model

#####################################
#best way to visualize random forest model

# separate into training and test data
library(caret)

set.seed(123)
index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  # 80% training, 20% testing
train_data <- fishdata2[index, ]
test_data <- fishdata2[-index, ]

set.seed(123)
# Train the Random Forest model
model_rf <- caret::train(Common ~ .,
                         data = train_data,
                         method = "rf",
                         #preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))


# Print the model summary
print(model_rf)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train_data)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")



# run model
#plot decision tree

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

lp("igraph")
lp("ggraph")

tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))

#tree with smallest number of nodes
tree_func(final_model = model_rf$finalModel, tree_num)

#tree with largest number of nodes
tree_num <- which(model_rf$finalModel$forest$ndbigtree == max(model_rf$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf$finalModel, tree_num)


###run with test dataset


set.seed(123)
# Train the Random Forest model
model_rf <- caret::train(Common ~ .,
                         data = test_data,
                         method = "rf",
                         #preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))


# Print the model summary
print(model_rf)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = test_data)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")



# run model
#plot decision tree

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

lp("igraph")
lp("ggraph")

tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))

#tree with smallest number of nodes
tree_func(final_model = model_rf$finalModel, tree_num)

#tree with largest number of nodes
tree_num <- which(model_rf$finalModel$forest$ndbigtree == max(model_rf$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf$finalModel, tree_num)

###############################################################
#try random forest on fish knocks


fishdata0<-fishdata%>%
  filter(t == "d", ID_confidence ==1,  Selection != 3030, str_detect(Species, "caurinus|melanops|pinniger|maliger|elongatus") ) #selection 3030 is a major outlier 
#removed Pile Perch, Vermilion and Kelp Greenling as not enough occurrences to make predictions

fishdata1<-fishdata0%>%
  dplyr::select(Species, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)

#try with only important variables from PCA (removed fkurtosis, fupsweepmean, timecentroid,timeiqr because not normally distributed)
# fishdata2<-fishdata1%>%
#   dplyr::select(-Species, -fishID, -freq_flatness, -freq_entropy, -freq_skewness, -freq_roughness)%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
#   mutate(Common = as.factor(Common))%>%
#   mutate(across(where(is.numeric), scale))

#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common,  High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Load the necessary libraries
# install.packages("randomForest")
# library(randomForest)


# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  # 80% training, 20% testing
train <- fishdata2[train_index, ]
test <- fishdata2[-train_index, ]

sum(is.na(train))

# Train the Random Forest model
rf_model <- randomForest(Common ~ ., data = train, ntree = 500)

# Print the model summary
print(rf_model)

#Evaluate variable importance
importance(rf_model) #higher the value the more important to the model
varImpPlot(rf_model) #higher the value the more important to the model

##test model on test dataset

# 1. Make predictions on the test data
test_predictions <- predict(rf_model, newdata = test)

# 2. Evaluate Model Performance

## a) Confusion Matrix
library(caret)
conf_matrix <- confusionMatrix(test_predictions, test$Common)
print(conf_matrix)

## b) Accuracy
accuracy <- sum(test_predictions == test$Common) / length(test$Common)
cat("Accuracy: ", accuracy, "\n")

########need to binarize data to check these metrics
## c) Additional performance metrics (Precision, Recall, F1 Score)
# These are available in the confusionMatrix output as well
precision <- posPredValue(test_predictions, test$Common, positive = "your_positive_class")  # Replace with actual positive class
recall <- sensitivity(test_predictions, test$Common, positive = "your_positive_class")  # Replace with actual positive class
f1_score <- (2 * precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")

# Extract the first tree from the Random Forest model
# The 'getTree' function gives us the tree as a data frame
tree_1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# Print the first tree's structure
print(tree_1)

# Train a single decision tree using rpart (for better visualization)
library(rpart)
tree_model <- rpart(Common ~ ., data = train)

# Plot the decision tree using rpart.plot
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree from rpart")


# Plot the tree (this step can be modified based on how detailed you want it to be)
plot(tree_1, type = "n")  # Create an empty plot
text(tree_1, use.n = TRUE, all = TRUE, cex = 0.8)  # Add tree text to the plot

#find the number of variable selected at each split (mtry) - you select the mtry with the lowest out of bag error (OOB)
mtry <- tuneRF(train[-1],train$Common, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#build model again with best mtry
set.seed(123)
rf_model2 <- randomForest(Common ~ ., data = train, mtry=best.m, importance = TRUE, ntree = 500)

print(rf_model2)
#Evaluate variable importance
importance(rf_model2) #higher the value the more important to the model
varImpPlot(rf_model2) #higher the value the more important to the model

# Partial dependence plot coloured by species


lp("ranger")
# Fit a quick RF
set.seed(1143)  # for reproducibility
rfo <- ranger(Common ~ ., data = train, probability = TRUE)
print(rfo)

# Prediction wrapper that returns average prediction for each class
pfun <- function(object, newdata) {
  colMeans(predict(object, data = newdata)$predictions)
}

#Freq_centroid
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_centroid", pred.fun = pfun)
ggplot(p, aes(freq_centroid, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_median_mean
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_median_mean", pred.fun = pfun)
ggplot(p, aes(freq_median_mean, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_pct75
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct75", pred.fun = pfun)
ggplot(p, aes(freq_pct75, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_entropy
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_entropy_std", pred.fun = pfun)
ggplot(p, aes(freq_entropy_std, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

#Freq_pct5
# Partial dependence of probability for each class on petal width
p <- partial(rfo, pred.var = "freq_pct5", pred.fun = pfun)
ggplot(p, aes(freq_pct5, yhat, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

############################
#TEST CLASS WEIGHTING WITH RANGER
#####
#ORIGINAL DATA - UNBALANCED
#don't include Kelp Greenling because all IDs are just too uncertain
fishdata0 <- fishdata %>%
  filter(
    t == "d",
    Selection != 3030,
    str_detect(Species, "caurinus|melanops|maliger|elongatus|pinniger|vacca"),
    (
      # Keep ID_confidence == 1 generally
      ID_confidence == 1 |
        # Exception: Lingcod and Kelp Greenling can be 1 or 2
        (str_detect(Species, "elongatus") & ID_confidence %in% c(1, 2))
    ))

numSpecies<-fishdata0 %>%
  count(Species) %>%
  arrange(desc(n))  # Optional: sort by count
numSpecies

fishdata1<-fishdata0%>%
  dplyr::select(Species, Site, fishID, Common, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid)


#random forest version
fishdata2<-fishdata1%>%
  dplyr::select(Common, Site, fishID, High.Freq..Hz., Low.Freq..Hz., freq_peak:time_centroid )%>% #removing length because too many NAs and can't have NA in bray curtis matrix (rerun later with only data with length available)
  dplyr::select(-freq_flatness)%>%
  mutate(Common = as.factor(Common))%>%
  drop_na()

# Split the data into training and test sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(fishdata2$Common, p = 0.7, list = FALSE)  #70% training, 30% testing
train_wExtra <- fishdata2[train_index, ] #includes Site and fishID #
test_wExtra <- fishdata2[-train_index, ] #includes Site and fishID #

train<- train_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)
test<- test_wExtra%>%
  dplyr::select(!Site)%>%
  dplyr::select(!fishID)


# First, get class distribution to compute weights (inverse frequency example)
class_counts <- table(train$Common)
class_counts
class_weights <- 1 / class_counts
class_weights
class_weights <- class_weights / sum(class_weights)  # normalize to sum to 1

# Fit the ranger model
model <- ranger(
  formula = Common ~ .,
  data = train,
  num.trees = 2000,
  max.depth = 6,  # to limit tree depth (not split count directly)
  class.weights = class_weights,
  probability = FALSE,  # Set to TRUE if you want class probabilities
  importance = 'impurity'
)

# Print model summary
print(model)

##
# Assuming you already have a trained model
predictions_test <- predict(model, data = test)

# For classification
predicted_classes_test <- predictions_test$predictions

# For binary classification with probabilities
# probs_test <- predictions_test$predictions[, 2]  # if probability = TRUE was used

# Evaluate
conf_matrix_test <- confusionMatrix(factor(predicted_classes_test), factor(test$Common))
print(conf_matrix_test)
##

# Make predictions on training data
predictions <- predict(model, data = train)
predictions
# Extract predicted classes
predicted_classes <- predictions$predictions

# Actual classes
actual_classes <- train$Common

# Load necessary library for evaluation
library(caret)

# Confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_classes))
print(conf_matrix)

# Accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Training Accuracy:", round(accuracy, 4)))


#################################

