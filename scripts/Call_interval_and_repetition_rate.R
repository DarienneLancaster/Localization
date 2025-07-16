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
  # select(Selection, Begin.Time..s., End.Time..s., fishID, Species, Begin.File, End, Selection_N, Edit)%>%
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

#################################################
#NOTE - how to determing call sequence time cut off (e.g. 1 sec, 5 sec)
#calculate mean (sd) time between all calls made by unique fish (also calculate by species)
#calculate time between each call within a call sequence (call Interval)
SeqThresh <- fishdata99 %>%
  group_by(fishID) %>%
  mutate(
    Seq_thresh = ifelse(
      is.na(lag(End)),  # If lag() returns NA (i.e., first row in group)
      NA,                          
      Begin.Time..s. - lag(End)  # Otherwise, subtract Begin.Time..s. from previous row
    )
  ) %>%
  ungroup()

Thresh <- SeqThresh %>%
  group_by(Common, fishID) %>%
  summarise(
    mean_thresh = mean(Seq_thresh, na.rm = TRUE),
    sd_thresh = sd(Seq_thresh, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(Common != "Kelp Greenling")%>%
  filter(Common != "other")

Seq_Thresh_outliers<-ggplot(Thresh, aes(x = Common, y = mean_thresh, fill = Common)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +  # Or use your custom colors
  labs(
    title = "Time between calls for individual fish by species",
    x = "Species",
    y = "Time between calls (s)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
Seq_Thresh_outliers

ggsave("figures/CH2/Call_Interval_Threshold_Selection_withoutliersBOXPLOT.png", plot = Seq_Thresh_outliers, width = 10, height = 10, dpi = 300)

Thresh <- SeqThresh %>%
  group_by(Common, fishID) %>%
  summarise(
    mean_thresh = mean(Seq_thresh, na.rm = TRUE),
    sd_thresh = sd(Seq_thresh, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(mean_thresh < 20)%>%
  filter(Common != "Kelp Greenling")%>%
  filter(Common != "other")
  

Seq_Thresh_NOoutliers<-ggplot(Thresh, aes(x = Common, y = mean_thresh, fill = Common)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +  # Or use your custom colors
  labs(
    title = "Time between calls for individual fish by species (<20 seconds)",
    x = "Species",
    y = "Time between calls (s)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
Seq_Thresh_NOoutliers

ggsave("figures/CH2/Call_Interval_Threshold_Selection_NOoutliersBOXPLOT_lessthan20s.png", plot = Seq_Thresh_NOoutliers, width = 10, height = 10, dpi = 300)


MeanInt <- Thresh %>%
  filter(mean_thresh <20)%>%
  group_by(Common)%>%
  summarise(
    mean_interval = mean(mean_thresh, na.rm = TRUE),
    sd_interval = sd(sd_thresh, na.rm = TRUE),
    .groups = "drop"
  )

MeanInt_flex<-flextable(MeanInt)
MeanInt_flex
save_as_image(x = MeanInt_flex, path = "figures/CH2/Mean_call_interval_outliersabove20sremoved.png")

#######
#assign number to each call sequentially within a sequence

Call_Num<- fishdata98%>%
  filter(ID_confidence != 3)%>%
  group_by(fishID, Sequence_ID)%>%
  mutate(call_num = row_number()) %>%
  ungroup()

###
#calculate mean and sd of call intervals by species for each call within a sequence (e.g. call 1, call 2, etc...)
#calculate mean and sd of call interval for each call sequence
Call_Num_Sum <- Call_Num %>%
  filter(ID_confidence != 3)%>%
  group_by(Common, call_num) %>%
  summarize(C_Interval_mean = mean(Call_Interval, na.rm = TRUE), C_Interval_sd = sd(Call_Interval, na.rm = TRUE))

Call_Num1<- Call_Num%>%
  filter(!(call_num > 5))

Call_Num1$call_num<-as.factor(Call_Num1$call_num)



##
library(ggplot2)
library(ggpubr) #allows you to do wilcoxon pairwise comparisons in the plot

#################
#plot of call interval vs. call number within sequence for each species
DD <- ggplot(Call_Num1, aes(x = call_num, y = Call_Interval, fill = call_num)) +
  geom_boxplot(varwidth = TRUE, color = "black", outlier.shape = NA, alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", 
                     comparisons = combn(levels(Call_Num1$call_num), 2, simplify = FALSE),
                     label = "p.signif") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  facet_wrap(~ Common)

DD

ggsave("figures/CH2/CallNumbervsCallIntervalbySpecies_Boxplots.png", plot = DD, width = 10, height = 10, dpi = 300)

##

#count how many grunts and knocks per call number
percent_t <- Call_Num %>%
  group_by(Common, call_num, t) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total = sum(count), .by = c(Common, call_num)) %>%
  mutate(percent = (count / total) * 100)%>%
  filter(!(call_num >10))

GK<-ggplot(percent_t, aes(x = factor(call_num), y = percent, fill = t)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ Common) +
  theme_classic() +
  labs(
    x = "Call Number",
    y = "Percentage",
    fill = "t",
    title = "Distribution of call types per Call Number by species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
GK
ggsave("figures/CH2/CallNumbervsCallTypebySpecies_PercentBar.png", plot = GK, width = 10, height = 10, dpi = 300)

Call_Num_GK <- Call_Num %>%
  group_by(Common, call_num, t) %>%
  count(name = "occurrences") %>%
  ungroup()

CoppG<- Call_Num_GK%>%
  filter(Common == "Copper rockfish")



#calculate how many calls are repeated within a sequence
fishdata998 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  tally(name = "count")

#count number of grunts and drums in each 
fishdata001 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(g_count = sum(t == "g", na.rm = TRUE), d_count = sum(t=="d", na.rm = TRUE), e_count = sum(t=="e", na.rm = TRUE))

#join number of fish call data to main data
fishdata999<-left_join(fishdata98, fishdata998, by = c("fishID", "Sequence_ID"))%>%
  mutate(Sequence_Reps = count) %>%
  dplyr::select(-count) # Remove the intermediate 'count' column, if not needed

#join number of grunts and drums to main data
fishdata002<-left_join(fishdata999, fishdata001, by = c("fishID", "Sequence_ID"))

#calculate mean and sd of call interval for each call sequence
fishdata003 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(C_Interval_mean = mean(Call_Interval, na.rm = TRUE), C_Interval_sd = sd(Call_Interval, na.rm = TRUE))

#join number of grunts and drums to main data
fishdata004<-left_join(fishdata002, fishdata003, by = c("fishID", "Sequence_ID"))

#keep only the first row
fishdata000<-fishdata004%>%
  #filter(Species == "caurinus")%>%
  group_by(fishID, Sequence_ID)%>%
  slice(1)

#histogram of calling interval (time between linked calls) by species 
ggplot(fishdata000, aes(x = C_Interval_mean)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "#FFCC00") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Interval by Species", x = "Call Interval", y = "Count") +
  coord_cartesian(ylim = c(0, 20)) +  # Fix y-axis range from 0 to 8
  theme_classic()

#histogram of calling repetition (within a sequence) by species 
ggplot(fishdata000, aes(x = Sequence_Reps)) +
  geom_histogram(binwidth = 1, color = "black", fill = "#FFCC00") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Repetition by Species", x = "Call Repetition Rate", y = "Count") +
  coord_cartesian(ylim = c(0, 10)) +  # Fix y-axis range from 0 to 8
  theme_classic()


#############################################################################
#Summary table of Call interval, repetition rate, # grunts, # knocks, time in FOV

##

lp("purrr")
lp("stringr")
lp("lubridate")
lp("hms")

#convert tottime to time and then change to seconds
#there is a negative time value in one KG entry which will cause time conversion issues if you remove the KG filter
CallDeets <- fishdata000 %>%
  filter(Species != "decagrammus")%>%
  filter(Common != "other")%>%
  mutate(tottime = as.numeric(as_hms(tottime)))

# Define columns to summarize
summary_cols <- c("Sequence_Reps","d_count","g_count",  "e_count",  "C_Interval_mean", "C_Interval_sd", "tottime")


summary_table <- CallDeets %>%
  group_by(Common) %>%
  summarise(
    n_fish = n_distinct(fishID),  # 🔢 Count unique fishID per Common
    across(all_of(summary_cols),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  # Combine each mean and sd into a single string column
  mutate(across(ends_with("_mean"), 
                ~ {
                  col_base <- str_remove(cur_column(), "_mean")
                  sd_col <- paste0(col_base, "_sd")
                  mean_val <- round(.x, 2)
                  sd_val <- round(get(sd_col), 2)
                  paste0(mean_val, " ± ", sd_val)
                },
                .names = "{str_remove(.col, '_mean')}")) %>%
  # Select Common, unique fish count, and the formatted ± columns
  dplyr::select(Common, n_fish, all_of(summary_cols))

# View result
print(summary_table)


##########################
#create summary flex table for call details

calldeets_table<- summary_table%>%
  rename(
    "Call repetition" = Sequence_Reps,
    "Knock count" = d_count,
    "Grunt count" = g_count,
    "Other count" = e_count,
    "Call interval (mean)" = C_Interval_mean,
    "Call interval (standard deviation)" = C_Interval_sd,
    "Species" = Common,
    "n" = "n_fish",
    "Time on camera (s)" = "tottime" 
  )

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 3,
  background.color = "white")

calldeets_flextable <- flextable(calldeets_table)
calldeets_flextable <- colformat_double(
  x = calldeets_flextable,
  big.mark = ",", digits = 2, na_str = "N/A"
)
calldeets_flextable  <- line_spacing(calldeets_flextable , space = 1.5, part = "all")
# knock_flextable <- add_header_row(knock_flextable,
#                      colwidths = c(1, 8),
#                      values = c("", "Sound Features")
# )
calldeets_flextable  <- set_table_properties(calldeets_flextable , align = "right", layout = "autofit")
# Add a title row: "Knocks"
calldeets_flextable <- theme_vanilla(calldeets_flextable)
calldeets_flextable <- width(calldeets_flextable, width = 1.2)
calldeets_flextable
save_as_image(x = calldeets_flextable, path = "figures/CH2/Call_Details_Table2.png")

##########################
#create box plots of call details

# Define custom colors for species
custom_colors <- c(
  "Black rockfish" = "#003399",   
  "Quillback rockfish" = "#FF6600", 
  "Copper rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

#Call interval
str(CallDeets)

CI<- ggplot(CallDeets, aes(x = Common, y = C_Interval_mean, fill = Common)) +
  geom_boxplot(varwidth = TRUE, color = "black", outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Common),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call interval (s)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
CI

#All species Call Interval
pairwise.wilcox.test(CallDeets$C_Interval_mean, CallDeets$Common, p.adjust.method = "BH")
# nothing significant


#call repetition
CR<- ggplot(CallDeets, aes(x = Common, y = Sequence_Reps, fill = Common)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Common),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call repetition (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
CR
#All species Call Interval
pairwise.wilcox.test(CallDeets$Sequence_Reps, CallDeets$Common, p.adjust.method = "BH")
# nothing significant

# number knocks
Knum<- ggplot(CallDeets, aes(x = Common, y = d_count, fill = Common)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Common),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Knock (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
Knum

#All species knocks
pairwise.wilcox.test(CallDeets$d_count, CallDeets$Common, p.adjust.method = "BH")
# Lingcod and Quillback rockfish have significantly more knock sounds than Black rockfish

#number grunts
Gnum<- ggplot(CallDeets, aes(x = Common, y = g_count, fill = Common)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Common),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Grunt (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
Gnum

#All species grunts
pairwise.wilcox.test(CallDeets$g_count, CallDeets$Common, p.adjust.method = "BH")
# Black rockfish have significantly more grunts than than all species,
#Copper rockfish have significantly more grunts than Canary and Pile perch

lp("patchwork")
lp("cowplot")

# Combine plots
All_calldeets <- (CI |CR) /
  (Knum| Gnum) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "")

All_calldeetsfinal <- ggdraw() +
  # Title at the top
  draw_label("", fontface = "bold", x = 0.1, y = 0.98, size = 16, hjust = 0.5) +
  # Shared y-axis label (rotated)
  draw_label("", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Species", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(All_calldeets, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

All_calldeetsfinal
ggsave("figures/CH2/CallDetails_Boxplots.png", plot = All_calldeetsfinal, width = 10, height = 10, dpi = 300)

#look at call rate (before this can be included we need a better estimates of how often fish show
#up on camera and are silent)

CallDeets$CallRate<-(CallDeets$soundsperfish/CallDeets$tottime)

#Call rate
CallR<- ggplot(CallDeets, aes(x = Common, y = CallRate, fill = Common)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Common),
    position = position_jitter(width = 0.1),
    size = 2,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +  # color points same as fill
  labs(
    title = "Call Rate",
    x = "Species",
    y = "Call rate (calls per second)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
CallR
  

###################################################################################
#GLM poisson for number of Grunts vs. behaviour

#Quillback


levels(as.factor(CallDeets$Activity))

CallDeets_qb <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "A No activity"),
    Activity = str_replace(Activity, "No activity", "A No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  filter(Species == "maliger")

#load MASS package to get glm.nb (negative binomial function)
lp("MASS")

M1<- glm.nb(g_count~Activity, data = CallDeets_qb)
summary(M1)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
explaineddeviance

Mnull<- glm.nb(g_count~1, data = CallDeets_qb)
summary(Mnull)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Mnull)$null.deviance-(Mnull)$deviance)/(Mnull)$null.deviance)
explaineddeviance


lp("AICcmodavg")
packageVersion("AICcmodavg")

lp("flextable")
###put all models in a list to compare AIC weights
models <- list(Mnull, M1)
model.names <- c('Mnull', 'M1')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)



###model validation plots

par(mfrow = c(2, 2))
plot(M1)
#validation plots aren't great but no major issues considering what a small sample size it is

# Residuals vs Fitted plot - hoping for random scattering of points around line.  
# QQ plot - hoping for points to follow line closely (dipping below and above line near the ends (called tailedness) can indicate overdistribution)
#         - points mostly above or mostly below line can indicate skewedness to right or left
#         - small sample sizes like ours are more prone to variability so it's not uncommon for it not to perfectly fit the line
# Scale-Location plot - plots fitted values against square root of standardized residuals (looking again for even scattering around a straight middle line 
# like the Residual vs. fitted plot - again outliers and small datasets can have issues with this)
# Residuals vs Leverage - helps identify influential outliers (in this example point 7 is almost worrisome but it's not past the dashed lines so probably okay to include)


#check model fit with DHARMa tests
r <- simulateResiduals(M1, n = 1000, plot = TRUE)  #If there are issues with resid vs pred quantile plot they will show up in red on this plot

####################################
#Copper

levels(as.factor(CallDeets$Activity))

CallDeets_cop <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "A No activity"),
    Activity = str_replace(Activity, "No activity", "A No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  filter(Species == "caurinus")

#load MASS package to get glm.nb (negative binomial function)
lp("MASS")

M1<- glm.nb(g_count~Activity, data = CallDeets_cop)
summary(M1)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
explaineddeviance

Mnull<- glm.nb(g_count~1, data = CallDeets_cop)
summary(Mnull)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Mnull)$null.deviance-(Mnull)$deviance)/(Mnull)$null.deviance)
explaineddeviance


lp("AICcmodavg")
packageVersion("AICcmodavg")

lp("flextable")
###put all models in a list to compare AIC weights
models <- list(Mnull, M1)
model.names <- c('Mnull', 'M1')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

###model validation plots

par(mfrow = c(2, 2))
plot(M1)
#validation plots aren't great but no major issues considering what a small sample size it is

# Residuals vs Fitted plot - hoping for random scattering of points around line.  
# QQ plot - hoping for points to follow line closely (dipping below and above line near the ends (called tailedness) can indicate overdistribution)
#         - points mostly above or mostly below line can indicate skewedness to right or left
#         - small sample sizes like ours are more prone to variability so it's not uncommon for it not to perfectly fit the line
# Scale-Location plot - plots fitted values against square root of standardized residuals (looking again for even scattering around a straight middle line 
# like the Residual vs. fitted plot - again outliers and small datasets can have issues with this)
# Residuals vs Leverage - helps identify influential outliers (in this example point 7 is almost worrisome but it's not past the dashed lines so probably okay to include)

#check model fit with DHARMa tests
r <- simulateResiduals(M1, n = 1000, plot = TRUE)  #If there are issues with resid vs pred quantile plot they will show up in red on this plot


##############################################
#Behaviour box plots

CallDeets2 <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  filter(Species == "caurinus")

# Define custom colors for species
custom_colors_BEHAV <- c(
  "Chase" = "#003399",   
  "Fleeing" = "#FF6600", 
  "No activity" = "#33CC99",
  "Approach" = "#33CCFF",
  "Feeding" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

####
#definite trend of more grunts when in Fleeing (being chased for Coppers and Quillbacks, doesn't really show up for other species)

#number grunts
G_BEHA_C<- ggplot(CallDeets2, aes(x = Activity, y = g_count, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "Copper",
    x = "",
    y = "Grunt (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
G_BEHA_C

#test if there are significant differences in number of grunts across behaviours

#Copper Grunts 
pairwise.wilcox.test(CallDeets2$g_count, CallDeets2$Activity, p.adjust.method = "BH")
# significantly more grunts during Fleeing than during No Activity and Feeding
#significantly more grunts during chasing than during no activity

#number grunts
K_BEHA_C<- ggplot(CallDeets2, aes(x = Activity, y = d_count, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Knock (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
K_BEHA_C

#Copper Knocks
pairwise.wilcox.test(CallDeets2$d_count, CallDeets2$Activity, p.adjust.method = "BH")
# significantly more knocks during Fleeing and No activity than during chasing 

#number call reps
Rep_BEHA_C<- ggplot(CallDeets2, aes(x = Activity, y = Sequence_Reps, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call repetition (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
Rep_BEHA_C

#Copper call reps
pairwise.wilcox.test(CallDeets2$Sequence_Reps, CallDeets2$Activity, p.adjust.method = "BH")
# significantly more call reps during Fleeing than during feeding

#number grunts
INT_BEHA_C<- ggplot(CallDeets2, aes(x = Activity, y = C_Interval_mean, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call interval (s)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
INT_BEHA_C

#Copper call reps

#remove any activity groups with less than 2 observations
CallDeets2_filtered <- CallDeets2 %>%
  group_by(Activity) %>%
  filter(n() >= 2) %>%
  ungroup()

pairwise.wilcox.test(CallDeets2_filtered$C_Interval_mean, 
                     CallDeets2_filtered$Activity, 
                     p.adjust.method = "BH")
#no significant differences

####################
#behaviour vs call patterns for QB

CallDeets3 <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  filter(Species == "maliger")

# Define custom colors for species
custom_colors_BEHAV <- c(
  "Chase" = "#003399",   
  "Fleeing" = "#FF6600", 
  "No activity" = "#33CC99",
  "Approach" = "#33CCFF",
  "Feeding" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

####
#definite trend of more grunts when in Fleeing (being chased for Coppers and Quillbacks, doesn't really show up for other species)

#number grunts
G_BEHA_Q<- ggplot(CallDeets3, aes(x = Activity, y = g_count, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "Quillback",
    x = "",
    y = "Grunt (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
G_BEHA_Q

#Quillback grunts
pairwise.wilcox.test(CallDeets3$g_count, CallDeets3$Activity, p.adjust.method = "BH")
# significantly more grunts during Fleeing and feeding than no activity


#number knocks
K_BEHA_Q<- ggplot(CallDeets3, aes(x = Activity, y = d_count, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Knock (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
K_BEHA_Q

#Quillback knocks
pairwise.wilcox.test(CallDeets3$d_count, CallDeets3$Activity, p.adjust.method = "BH")
# nothing significant

#number reps
Rep_BEHA_Q<- ggplot(CallDeets3, aes(x = Activity, y = Sequence_Reps, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call repetition (count)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
Rep_BEHA_Q

#Quillback call reps
pairwise.wilcox.test(CallDeets3$Sequence_Reps, CallDeets3$Activity, p.adjust.method = "BH")
# nothing significant


#call interval
INT_BEHA_Q<- ggplot(CallDeets3, aes(x = Activity, y = C_Interval_mean, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "",
    x = "",
    y = "Call interval (s)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
INT_BEHA_Q

#Quillback interval
#remove any activity groups with less than 2 observations
CallDeets3_filtered <- CallDeets3 %>%
  group_by(Activity) %>%
  filter(n() >= 2) %>%
  ungroup()

pairwise.wilcox.test(CallDeets3_filtered$C_Interval_mean, 
                     CallDeets3_filtered$Activity, 
                     p.adjust.method = "BH")
#nothing significant 

lp("patchwork")
lp("cowplot")

# Combine plots
CallPat_Beha_CQB <- ( G_BEHA_Q|K_BEHA_Q|Rep_BEHA_Q|INT_BEHA_Q) /
  ( G_BEHA_C|K_BEHA_C|Rep_BEHA_C|INT_BEHA_C) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "")

CallPat_Beha_CQBfinal <- ggdraw() +
  # Title at the top
  draw_label("", fontface = "bold", x = 0.1, y = 0.98, size = 16, hjust = 0.5) +
  # Shared y-axis label (rotated)
  draw_label("", angle = 90, x = 0.03, y = 0.5, vjust = 0.5) +
  # Shared x-axis label (centered at bottom)
  draw_label("Behaviour", angle = 0, x = 0.48, y = 0.02, vjust = 0.5) +
  # Combined plot
  draw_plot(CallPat_Beha_CQB, x = 0.05, y = 0.05, width = 0.9, height = 0.9)

CallPat_Beha_CQBfinal
ggsave("figures/CH2/CallPatterns_Behaviour_QB_C_Boxplots.png", plot = CallPat_Beha_CQBfinal, width = 10, height = 6, dpi = 300)


#################
#check other species relationship to call interval

CallDeets4 <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  filter(Species == "pinniger")

#call interval
INT_BEHA_O<- ggplot(CallDeets4, aes(x = Activity, y = C_Interval_mean, fill = Activity)) +
  geom_boxplot(varwidth = TRUE, color = "black",  outlier.shape = NA, alpha = 0.7) +
  geom_point(
    aes(color = Activity),
    position = position_jitter(width = 0.1),
    size = 1,
    alpha = 0.3,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  scale_color_manual(values = custom_colors_BEHAV) +  # color points same as fill
  labs(
    title = "call interval",
    x = "",
    y = "Call interval (s)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
INT_BEHA_O


###################################
#Percent behaviour type by species

CallDeets_all <- CallDeets %>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))


# Step 1: Prepare data by calculating percentages
  activity_summary <- CallDeets_all %>%
  group_by(Common, Activity) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Common) %>%
  mutate(percentage = count / sum(count) * 100)

custom_colors_BEHAV <- c(
  "Chase" = "#003399",   
  "Fleeing" = "#FF6600", 
  "No activity" = "#33CC99",
  "Approach" = "#33CCFF",
  "Feeding" = "#FFCC00"
)

# Step 2: Plot stacked bar chart with custom colors
Beha_Bar <- ggplot(activity_summary, aes(x = Common, y = percentage, fill = Activity)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha =0.7) +
  scale_fill_manual(values = custom_colors_BEHAV) +
  labs(
    title = "",
    x = "Species",
    y = "Behavoiur (%)",
    fill = "Behaviour"  # <-- This renames the legend title
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Beha_Bar
ggsave("figures/CH2/Percentage_Behaviour_barplot.png", plot = Beha_Bar, width = 10, height = 6, dpi = 300)
  
# if you want sample size for each bar use this code. 

# Beha_Bar <- ggplot(activity_summary, aes(x = Common, y = percentage, fill = Activity)) +
#   geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.5) +
#   # Add count labels inside each bar segment
#   geom_text(aes(label = count),
#             position = position_stack(vjust = 0.5),
#             size = 3, color = "black") +
#   scale_fill_manual(values = custom_colors_BEHAV) +
#   labs(
#     title = "",
#     x = "Species",
#     y = "Behaviour (%)",
#     fill = "Behaviour"
#   ) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# Beha_Bar


####################################
#how many Fleeing instances are conspecific vs other

Fleeing<-CallDeets%>%
  mutate(
    Activity = if_else(Activity == "" | is.na(Activity), "No activity", Activity),
    Activity = str_replace(Activity, "Chase other", "Chase"),
    Activity = str_replace(Activity, "Chase conspecific", "Chase"),
    Activity = str_replace(Activity, "Guarding bait", "Fleeing"),
    Activity = str_replace(Activity, "Passing", "No activity"),
    Activity = str_replace(Activity, "No activity", "No activity"),
    Activity = str_replace(Activity, "Attracted", "Approach"))%>%
  #filter(Species == "pinniger")
  filter(Activity == "Fleeing")
