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

fishdata0<-fishdata%>%
  dplyr::filter(t == "g", ID_confidence ==1|2,  Selection != 3030, str_detect(Species, "caurinus|melanops|pinniger|maliger") ) #selection 3030 is a major outlier 

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
save_as_image(x = conf_df_flextable, path = "figures/rf_grunt_TRAINING_confusion_matrix.png")

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

ggsave("figures/Grunt_Train_Variable_Importance.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

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

##############################################################################
#One vs. All

###########################
#Copper vs All - GRUNTS
###########################

fishdata0 <- fishdata %>%
  filter(
    t == "g",
    ID_confidence %in% c(1, 2),
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
plot(smooth(roc_obj), main = "")

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
    ID_confidence %in% c(1, 2),
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
plot(smooth(roc_obj), main = "")

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