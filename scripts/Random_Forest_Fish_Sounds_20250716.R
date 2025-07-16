#Script runs random forest multiclass models for grunts and knocks by species
#Script also creates variable importance plots, UMAP plots, and partial dependence probability plots for each random forest model

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
lp("smplot2") 
lp("vegan")
lp("gridExtra")
lp("stringr")
lp("flextable")
lp("randomForest")
lp("caret")
lp("lattice")
lp("gridExtra")

#pull .csv from wdata (working data) folder - if .csv is not saved in wdata folder remove wdata from file path
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

##########################################################################
#Random forest - fish GRUNTS - multiclass model


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

#View confusion matrix
conf_matrix <- rf_model$confusion

#Convert to data frame
conf_df <- as.data.frame(conf_matrix)

#Add row names as a new column for clarity
conf_df$'Actual Class' <- rownames(conf_df)
conf_df<-conf_df%>%
  rename(Error=class.error)
rownames(conf_df) <- NULL  # optional: reset row names

#Reorder columns to show actual class first
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
conf_df_flextable  <- set_table_properties(conf_df_flextable, align = "right", layout = "autofit")
conf_df_flextable <- theme_vanilla(conf_df_flextable)
conf_df_flextable

#save figure to folder named figures in working directory
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

class_counts <- test %>%
  count(Common) %>%
  rename(`Knock Class` = Common, n = n)

class_stats$n <- class_counts$n 

# 3. Add row names as a new column for clarity
class_stats$Class <- rownames(class_stats)
rownames(class_stats) <- NULL  # optional: reset row names
class_stats<-class_stats%>%
  dplyr::select(-Sensitivity, -Specificity, -`Pos Pred Value`, -`Neg Pred Value`)%>%
  mutate(Class = str_replace(Class, "^Class:", "")) %>%
  rename('F1 Score' = F1, 'Grunt Class' = Class)


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
#UMAP data visualization to look for site based clustering
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
  labs(title = "Random forest grunt predictions",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "none")
pred

#True values plot (shows how well the groups align with manual video anlaysis species classifications)
true<-ggplot(umap_df, aes(V1, V2, color = True, fill = True, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA) +
  stat_ellipse(level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Manual classification via video review",
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
lp("pdp")
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

################################################################################################################
#KNOCKS

##########################################################################
#Random forest - fish knocks - (MULTICLASS)

#don't include Kelp Greenling because all IDs are low confidence
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

class_counts <- test %>%
  count(Common) %>%
  rename(`Knock Class` = Common, n = n)

class_stats$n <- class_counts$n 

# 3. Add row names as a new column for clarity
class_stats$Class <- rownames(class_stats)
rownames(class_stats) <- NULL  # optional: reset row names
class_stats<-class_stats%>%
  dplyr::select(-Sensitivity, -Specificity, -`Pos Pred Value`, -`Neg Pred Value`)%>%
  mutate(Class = str_replace(Class, "^Class:", "")) %>%
  rename('F1 Score' = F1, 'Knock Class' = Class)


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
class_stats_flextable  <- set_table_properties(class_stats_flextable, align = "right", layout = "autofit")
K_class_stats_flextable <- theme_vanilla(class_stats_flextable)
K_class_stats_flextable
save_as_image(x = K_class_stats_flextable, path = "figures/rf_knock_TEST_RESULTS_confusion_matrix.png")

################################################################################
#UMAP data visualization
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
  labs(title = "Random forest knock predictions",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "none")

#True values plot (shows how well the groups align with manual video analysis species classifications)
true<-ggplot(umap_df, aes(V1, V2, color = True, fill = True, shape = Site)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = True), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = True), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Manual classification via video review",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()

UMAP_knocks_unbalanced<- pred+true
UMAP_knocks_unbalanced
ggsave("figures/UMAP_Knock_UNBALANCED_SiteID.png", plot = UMAP_knocks_unbalanced, width = 10, height = 6, dpi = 300)

####################
#PDP plots - Knocks
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
#########################
