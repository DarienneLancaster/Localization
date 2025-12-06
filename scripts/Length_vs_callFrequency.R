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
fishdata$Common<- ifelse(fishdata$Species == "caurinus", "Copper Rockfish",
                         ifelse(fishdata$Species == "maliger", "Quillback Rockfish",
                                ifelse(fishdata$Species == "pinniger", "Canary Rockfish",
                                       ifelse(fishdata$Species == "miniatus", "Vermillion Rockfish",
                                              ifelse(fishdata$Species == "melanops", "Black Rockfish",
                                                     ifelse(fishdata$Species == "elongatus", "Lingcod",
                                                            ifelse(fishdata$Species == "decagrammus", "Kelp Greenling",
                                                                   ifelse(fishdata$Species == "vacca", "Pile Perch", "other"))))))))

##################################################################
info<-fishdata%>%
  filter(Activity == "Guarding bait")%>%
  distinct(fishID, .keep_all = TRUE)%>%
  filter(fishID == "0014_20220823")

info2<-fishdata%>%
  filter(Common == "Copper Rockfish")%>%
  filter(ID_confidence == 1|2)%>%
  #distinct(fishID, .keep_all = TRUE)%>%
  filter(mean_length == min(mean_length, na.rm = TRUE))

###############
#check for significant difference between quillback and copper frequency features

#truncate all coppers to be less than 229mm (QB max length)

#knocks ttest

C_QB<-fishdata%>%
  filter(
    (Common == "Quillback Rockfish" & mean_length < 229) |
      (Common == "Copper Rockfish" & mean_length < 229)
  ) %>%
  filter(ID_confidence %in% c(1, 2))%>%
  filter(t == "d")


# Unpaired (independent) t-test
t.test(freq_pct25 ~ Common, data = C_QB, subset = Common %in% c("Quillback Rockfish", "Copper Rockfish"))

custom_colors <- c(
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99"
)

ttest_knocks <- ggplot(C_QB[C_QB$Common %in% c("Quillback Rockfish", "Copper Rockfish"), ], 
                       aes(x = Common, y = freq_pct25, fill = Common)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = Common), width = 0.2, alpha = 0.5) +
  labs(
    title = "Welch t test - knocks",
    x = "",
    y = "Frequency 25%"
  ) +
  theme_classic() +
  theme(legend.position = "none") +  #  Removes the legend
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors)
ttest_knocks
ggsave("figures/CH2/ttest_CQB_knocks.png", plot = ttest_knocks, width = 10, height = 6, dpi = 300)

#################


#grunts ttest
C_QB<-fishdata%>%
  filter(
    (Common == "Quillback Rockfish" & mean_length < 229) |
      (Common == "Copper Rockfish" & mean_length < 229)
  ) %>%
  filter(ID_confidence %in% c(1, 2))%>%
  filter(t == "g")




# Unpaired (independent) t-test
t.test(freq_pct25 ~ Common, data = C_QB, subset = Common %in% c("Quillback Rockfish", "Copper Rockfish"))

custom_colors <- c(
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99"
)

ttest_grunts <- ggplot(C_QB[C_QB$Common %in% c("Quillback Rockfish", "Copper Rockfish"), ], 
                       aes(x = Common, y = freq_pct25, fill = Common)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = Common), width = 0.2, alpha = 0.5) +
  labs(
    title = "Welch t test - grunts",
    x = "",
    y = "Frequency 25%"
  ) +
  theme_classic() +
  theme(legend.position = "none") +  #  Removes the legend
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors)
ttest_grunts 
ggsave("figures/CH2/ttest_CQB_grunts.png", plot = ttest_grunts, width = 10, height = 6, dpi = 300)



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


###############################
#run loop for all four knock frequency variables 

#---------------------------------------------------
# 1. Define species datasets and variable names
#---------------------------------------------------

species_data <- list(
  "Copper Rockfish"    = length_knocks_CP,
  "Quillback Rockfish" = length_knocks_QB,
  "Canary Rockfish"    = length_knocks_CA,
  "Black Rockfish"     = length_knocks_BL,
  "Lingcod"            = length_knocks_L,
  "Pile Perch"         = length_knocks_PP
)

response_vars <- c("freq_pct50", "freq_pct25", "freq_pct75", "freq_peak")

#---------------------------------------------------
# 2. Fit all models: each species × each response variable
#---------------------------------------------------

all_models <- expand_grid(
  Species = names(species_data),
  Response = response_vars
) %>%
  mutate(
    Model = map2(
      Species, Response,
      ~ lm(formula(paste0(.y, " ~ mean_length")), data = species_data[[.x]])
    )
  )

#---------------------------------------------------
# 3. Convert all model results into a clean summary table
#---------------------------------------------------

model_summary <- all_models %>%
  mutate(
    Tidy = map(Model, ~ tidy(.x) %>% filter(term == "mean_length"))
  ) %>%
  unnest(Tidy) %>%
  select(Species, Response, estimate, std.error, statistic, p.value)

#---------------------------------------------------
# 4. Format the table (rounding + p-value rules)
#---------------------------------------------------

model_summary <- model_summary %>%
  rename(
    Estimate        = estimate,
    `Standard Error` = std.error,
    `t value`       = statistic,
    `p value`       = p.value
  ) %>%
  mutate(
    Estimate         = round(Estimate, 3),
    `Standard Error` = round(`Standard Error`, 3),
    `t value`        = round(`t value`, 3),
    `p value`        = ifelse(
      `p value` < 0.01,
      "<0.01",
      sprintf("%.3f", round(`p value`, 3))
    )
  )

#---------------------------------------------------
# 5. Build a flextable with section headers for each variable
#---------------------------------------------------

# Sort alphabetically by response variable
model_summary <- model_summary %>%
  arrange(Response, Species)

# Rename response variables
model_summary <- model_summary %>%
  mutate(Response = case_when(
    Response == "freq_pct50" ~ "Frequency 50%",
    Response == "freq_pct25" ~ "Frequency 25%",
    Response == "freq_pct75" ~ "Frequency 75%",
    Response == "freq_peak"  ~ "Peak frequency",
    TRUE ~ Response  # keep any other values unchanged
  )) %>%
  arrange(Response, Species)  # sort alphabetically


# Build flextable
ft <- flextable(model_summary)


# Build flextable
ft <- flextable(model_summary) %>%
  theme_booktabs() %>%
  autofit() %>%
  separate_header() %>%
  set_header_labels(
    Species = "Species",
    Response = "Response Variable",
    Estimate = "Estimate",
    `Standard Error` = "Standard Error",
    `t value` = "t value",
    `p value` = "p value"
  ) %>%
  merge_v(j = "Response") %>%
  valign(j = "Response", valign = "top") %>%
  bold(i = ~ !duplicated(Response), j = "Response", bold = TRUE) %>%
  # Add horizontal lines after each response variable grouping
  hline(i = ~ duplicated(Response) & !duplicated(Response, fromLast = TRUE), border = fp_border(color="black", width = 1))



ft
save_as_image(x = ft, path = "figures/CH2/LM_Results_Knocks_AllVars_Length.png")

# Create Word document
doc <- read_docx() %>%
  body_add_par("Model Summary Table", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")  # adds spacing

# Save editable Word document
print(doc, target = "figures/CH2/LM_Results_Knocks_AllVars_Length.docx")

################################


# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "c)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "d)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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

length_grunts_CP<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))

length_grunts_QB<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))


length_grunts_BL<-fishdata%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "melanops"))


##
lp('purrr')
lp('broom')


###############################
#run loop for all four grunt frequency variables 

#---------------------------------------------------
# 1. Define species datasets and variable names
#---------------------------------------------------

species_data <- list(
  "Copper Rockfish"    = length_grunts_CP,
  "Quillback Rockfish" = length_grunts_QB,
  "Black Rockfish"     = length_grunts_BL
)

response_vars <- c("freq_pct50", "freq_pct25", "freq_pct75", "freq_peak")

#---------------------------------------------------
# 2. Fit all models: each species × each response variable
#---------------------------------------------------
all_models <- expand_grid(
  Species = names(species_data),
  Response = response_vars
) %>%
  mutate(
    Model = map2(
      Species, Response,
      ~ lm(formula(paste0(.y, " ~ mean_length")), data = species_data[[.x]])
    )
  )


#---------------------------------------------------
# 3. Convert all model results into a clean summary table
#---------------------------------------------------

model_summary <- all_models %>%
  mutate(
    Tidy = map(Model, ~ tidy(.x) %>% filter(term == "mean_length"))
  ) %>%
  unnest(Tidy) %>%
  select(Species, Response, estimate, std.error, statistic, p.value)

#---------------------------------------------------
# 4. Format the table (rounding + p-value rules)
#---------------------------------------------------

model_summary <- model_summary %>%
  rename(
    Estimate        = estimate,
    `Standard Error` = std.error,
    `t value`       = statistic,
    `p value`       = p.value
  ) %>%
  mutate(
    Estimate         = round(Estimate, 3),
    `Standard Error` = round(`Standard Error`, 3),
    `t value`        = round(`t value`, 3),
    `p value`        = ifelse(
      `p value` < 0.01,
      "<0.01",
      sprintf("%.3f", round(`p value`, 3))
    )
  )

#---------------------------------------------------
# 5. Build a flextable with section headers for each variable
#---------------------------------------------------

# Sort alphabetically by response variable
model_summary <- model_summary %>%
  arrange(Response, Species)

# Rename response variables
model_summary <- model_summary %>%
  mutate(Response = case_when(
    Response == "freq_pct50" ~ "Frequency 50%",
    Response == "freq_pct25" ~ "Frequency 25%",
    Response == "freq_pct75" ~ "Frequency 75%",
    Response == "freq_peak"  ~ "Peak frequency",
    TRUE ~ Response  # keep any other values unchanged
  )) %>%
  arrange(Response, Species)  # sort alphabetically


# Build flextable
ft <- flextable(model_summary)


# Build flextable
ft <- flextable(model_summary) %>%
  theme_booktabs() %>%
  autofit() %>%
  separate_header() %>%
  set_header_labels(
    Species = "Species",
    Response = "Response Variable",
    Estimate = "Estimate",
    `Standard Error` = "Standard Error",
    `t value` = "t value",
    `p value` = "p value"
  ) %>%
  merge_v(j = "Response") %>%
  valign(j = "Response", valign = "top") %>%
  bold(i = ~ !duplicated(Response), j = "Response", bold = TRUE) %>%
  # Add horizontal lines after each response variable grouping
  hline(i = ~ duplicated(Response) & !duplicated(Response, fromLast = TRUE), border = fp_border(color="black", width = 1))



ft
save_as_image(x = ft, path = "figures/CH2/LM_Results_Grunts_AllVars_Length.png")

# Create Word document
doc <- read_docx() %>%
  body_add_par("Model Summary Table", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")  # adds spacing

# Save editable Word document
print(doc, target = "figures/CH2/LM_Results_Grunts_AllVars_Length.docx")

#Linear regression Frequency 50% vs all species (GRUNTS)

# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "c)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "d)",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

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


############
#Quillback - Grunts
#############


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))%>%
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

str(Behav_UMAP)

# Extract feature matrix (excluding response)
X_test <- Behav_UMAP[, !(names(Behav_UMAP) %in% c("Common", "Activity", "Site", "mean_length"))]
X_test

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$freq_bandwidth<-Behav_UMAP$freq_bandwidth

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_Bandwidth = case_when(
    freq_bandwidth <= 400 ~ "<400 Hz",
    freq_bandwidth <= 600 ~ "400 to <600 Hz",
    freq_bandwidth <= 800 ~ "600 to <800 Hz",
    freq_bandwidth > 800  ~ "≥800 Hz"
  ),
  Frequency_Bandwidth = factor(Frequency_Bandwidth,
                               levels = c("<400 Hz", "400 to <600 Hz", "600 to <800 Hz", "≥800 Hz")))



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

Behaviour_QB_Grunts <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Frequency_Bandwidth)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Quillback Rockfish Grunts",
       x = "UMAP 1", y = "UMAP 2",
       color = "Behaviour",    # change legend title for color
       fill = "Behaviour",     # change legend title for fill
       shape = "Frequency Bandwidth") +  # change legend title for shape
  theme_classic() +
  theme(legend.position = "right")
Behaviour_QB_Grunts
ggsave("figures/CH2/UMAP_QB_Grunts_Behaviour.png", plot = Behaviour_QB_Grunts , width = 10, height = 6, dpi = 300)
##
#IF YOU SEE A TREND IN THE UMAP PLOT USE THIS CODE TO CHECK WHICH VARIABLE IS MOST LIKELY PULLING POINTS APART

#Combine original variables with UMAP coordinates
umap_combined <- cbind(X_test, UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2])

# Correlation with UMAP dimensions
cor_UMAP1 <- sapply(X_test, function(x) cor(x, umap_result$layout[,1], method = "spearman"))
cor_UMAP2 <- sapply(X_test, function(x) cor(x, umap_result$layout[,2], method = "spearman"))

# Combine into a single data frame
cor_df <- data.frame(Variable = names(X_test),
                     UMAP1_corr = cor_UMAP1,
                     UMAP2_corr = cor_UMAP2)

# Order by strongest correlation
cor_df <- cor_df[order(abs(cor_df$UMAP1_corr) + abs(cor_df$UMAP2_corr), decreasing = TRUE), ]
print(cor_df)
#for QB Grunts largest correlation is with Frequency Bandwidth (Fleeing is associated loosely with larger bandwidth in calls)
#examined mean_length relationship to bandwidth and it's not related
#all large bandwidth (>800) fleeing sounds come from the same individual so interpret with caution


############
#Quillback - Knocks
#############


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

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$freq_bandwidth<-Behav_UMAP$freq_bandwidth

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_Bandwidth = case_when(
    freq_bandwidth <= 400 ~ "<400 Hz",
    freq_bandwidth <= 600 ~ "400 to <600 Hz",
    freq_bandwidth <= 800 ~ "600 to <800 Hz",
    freq_bandwidth > 800  ~ "≥800 Hz"
  ),
Frequency_Bandwidth = factor(Frequency_Bandwidth,
                             levels = c("<400 Hz", "400 to <600 Hz", "600 to <800 Hz", "≥800 Hz")))



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

Behaviour_QB_Knocks <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity, shape = Frequency_Bandwidth)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Quillback Rockfish Knocks",
       x = "UMAP 1", y = "UMAP 2",
       color = "Behaviour",    # change legend title for color
       fill = "Behaviour",     # change legend title for fill
       shape = "Frequency Bandwidth") +  # change legend title for shape
  theme_classic() +
  theme(legend.position = "right")

Behaviour_QB_Knocks

ggsave("figures/CH2/UMAP_QB_Knocks_Behaviour.png", plot = Behaviour_QB_Knocks , width = 10, height = 6, dpi = 300)
##
#IF YOU SEE A TREND IN THE UMAP PLOT USE THIS CODE TO CHECK WHICH VARIABLE IS MOST LIKELY PULLING POINTS APART

#Combine original variables with UMAP coordinates
umap_combined <- cbind(X_test, UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2])

# Correlation with UMAP dimensions
cor_UMAP1 <- sapply(X_test, function(x) cor(x, umap_result$layout[,1], method = "spearman"))
cor_UMAP2 <- sapply(X_test, function(x) cor(x, umap_result$layout[,2], method = "spearman"))

# Combine into a single data frame
cor_df <- data.frame(Variable = names(X_test),
                     UMAP1_corr = cor_UMAP1,
                     UMAP2_corr = cor_UMAP2)

# Order by strongest correlation
cor_df <- cor_df[order(abs(cor_df$UMAP1_corr) + abs(cor_df$UMAP2_corr), decreasing = TRUE), ]
print(cor_df)
#for QB Grunts largest correlation is with Frequency Bandwidth (Feeding never seems to have very low or very high frequency bandwidths - but lots of overlap)

#combine QB plots into one

lp("patchwork")
lp("cowplot")

# Combine plots
QB_UMAP_combined <- (Behaviour_QB_Grunts|Behaviour_QB_Knocks ) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")
QB_UMAP_combined

ggsave("figures/CH2/QB_UMAP_combined_Behaviour.png", plot = QB_UMAP_combined, width = 10, height = 6, dpi = 300)



############
#Copper - Knocks
#############


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))%>%
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

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$freq_bandwidth<-Behav_UMAP$freq_bandwidth

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_Bandwidth = case_when(
    freq_bandwidth <= 400 ~ "400 Hz or less",
    freq_bandwidth <= 600 ~ "600 Hz or less",
    freq_bandwidth <= 800 ~ "800 Hz or less",
    freq_bandwidth > 800  ~ "800 Hz or more"
  ))



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
Behaviour_Cop_Knocks <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Knocks",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Cop_Knocks
ggsave("figures/CH2/UMAP_Cop_Knocks_Behaviour.png", plot = Behaviour_Cop_Knocks , width = 10, height = 6, dpi = 300)
##
#NO TRENDS- TOO MUCH OVERLAP - Add to Supp data

############
#Copper - Grunts
#############


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))%>%
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

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$freq_bandwidth<-Behav_UMAP$freq_bandwidth

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_Bandwidth = case_when(
    freq_bandwidth <= 400 ~ "400 Hz or less",
    freq_bandwidth <= 600 ~ "600 Hz or less",
    freq_bandwidth <= 800 ~ "800 Hz or less",
    freq_bandwidth > 800  ~ "800 Hz or more"
  ))



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
Behaviour_Cop_Grunts  <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Copper Grunts",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "none")
Behaviour_Cop_Grunts 
ggsave("figures/CH2/UMAP_Cop_Grunts_Behaviour.png", plot = Behaviour_Cop_Grunts , width = 10, height = 6, dpi = 300)
##
#NO TRENDS- TOO MUCH OVERLAP - Add to Supp data

#combine Copper plots into one

lp("patchwork")
lp("cowplot")

# Combine plots
Cop_UMAP_combined <- (Behaviour_Cop_Grunts|Behaviour_Cop_Knocks ) +
  theme(legend.position = "right")
Cop_UMAP_combined

ggsave("figures/CH2/Copper_UMAP_combined_Behaviour.png", plot = Cop_UMAP_combined, width = 10, height = 6, dpi = 300)


############
#Canary - Knocks
#############


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "pinniger"))%>%
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

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$freq_bandwidth<-Behav_UMAP$freq_bandwidth

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_Bandwidth = case_when(
    freq_bandwidth <= 400 ~ "400 Hz or less",
    freq_bandwidth <= 600 ~ "600 Hz or less",
    freq_bandwidth <= 800 ~ "800 Hz or less",
    freq_bandwidth > 800  ~ "800 Hz or more"
  ))



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
Behaviour_Can_Knocks <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Canary Knocks",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Can_Knocks
ggsave("figures/CH2/UMAP_Black_Knocks_Behaviour.png", plot = Behaviour_Can_Knocks , width = 10, height = 6, dpi = 300)
##
#NO TRENDS- TOO MUCH OVERLAP - Add to Supp data
#IF YOU SEE A TREND IN THE UMAP PLOT USE THIS CODE TO CHECK WHICH VARIABLE IS MOST LIKELY PULLING POINTS APART

#Combine original variables with UMAP coordinates
umap_combined <- cbind(X_test, UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2])

# Correlation with UMAP dimensions
cor_UMAP1 <- sapply(X_test, function(x) cor(x, umap_result$layout[,1], method = "spearman"))
cor_UMAP2 <- sapply(X_test, function(x) cor(x, umap_result$layout[,2], method = "spearman"))

# Combine into a single data frame
cor_df <- data.frame(Variable = names(X_test),
                     UMAP1_corr = cor_UMAP1,
                     UMAP2_corr = cor_UMAP2)

# Order by strongest correlation
cor_df <- cor_df[order(abs(cor_df$UMAP1_corr) + abs(cor_df$UMAP2_corr), decreasing = TRUE), ]
print(cor_df)

############
#Black - Grunts
#############


lp("umap")

#create dataframe for UMAP behaviour plotting
lp("dplyr")

Behav_UMAP<-fishdata%>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "melanops"))%>%
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

set.seed(57)
# Run UMAP
umap_result <- umap(X_test)

# Create a dataframe for plotting
umap_df <- as.data.frame(umap_result$layout)
umap_df$Activity<-Behav_UMAP$Activity
umap_df$Site<-Behav_UMAP$Site
umap_df$mean_length<-Behav_UMAP$mean_length
umap_df$Frequency_75<-Behav_UMAP$freq_pct75
umap_df$Frequency_25<-Behav_UMAP$freq_pct25

str(umap_df)

#bin length
umap_df<- umap_df %>%
  mutate(Length = case_when(
    mean_length <= 250 ~ "≤250 mm",
    mean_length  > 250  ~ ">250 mm"
  ))

#bin freq_bandwidth
umap_df<- umap_df %>%
  mutate(Frequency_75bin = case_when(
    Frequency_75 <= 400 ~ "≤400 Hz",
    Frequency_75  > 400  ~ ">400 Hz"
  ))


##########################
#HAVING ISSUES WITH THE BLACK ROCKFISH UMAP PLOTS - COLOURS NOT SHOWING UP

#umap_df$True <- test$Common
# umap_df$Site<-test_wExtra$Site
# umap_df$fishID<- test_wExtra$fishID
levels(as.factor(umap_df$Activity))

# Define custom colors for Activity
custom_colors <- c(
  "Chase" = "#003399",   
  "Flight" = "#FF6600", 
  "No activity" = "#33CC99",
  "Approach" = "#33CCFF",
  "Feeding" = "#FFCC00",
  "Pile Perch" = "#9900CC" 
)

# Define custom colors for Length
custom_colors_L <- c(
  "≤250 mm" = "#FFCC00",   
  ">250 mm" = "#FF6600"
)




par(mfrow = c(1, 2))

###########

#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_Black_Grunts_length  <- ggplot(umap_df, aes(V1, V2, color = Length, fill =Length)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Length), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Length), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors_L) +
  scale_fill_manual(values = custom_colors_L) +
  labs(title = "Fish Length",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Black_Grunts_length 



# Define custom colors for F75
custom_colors_F75 <- c(
  "≤400 Hz" = "#003399",
  ">400 Hz" =  "#33CC99"
)



#Predicted values plot (shows how the Random Forest classified fish sounds)
Behaviour_Black_Grunts_F75  <- ggplot(umap_df, aes(V1, V2, color = Frequency_75bin, fill =Frequency_75bin)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Frequency_75bin), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Frequency_75bin), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors_F75) +
  scale_fill_manual(values = custom_colors_F75) +
  labs(title = "Frequency 75%",
       x = "UMAP 1", y = "UMAP 2", color = "Frequency 75%",  # set color legend title
       fill = "Frequency 75%") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Black_Grunts_F75 

Behaviour_Black_Grunts  <- ggplot(umap_df, aes(V1, V2, color = Activity, fill = Activity)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Behaviour",
       x = "UMAP 1", y = "UMAP 2",
       color = "Behaviour",  # set color legend title
       fill = "Behaviour") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Black_Grunts 

Behaviour_Black_Gruntsbw  <- ggplot(umap_df, aes(V1, V2, color = Frequency_25, fill = Frequency_25)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "polygon", alpha = 0.1, color = NA, show.legend = FALSE) +
  stat_ellipse(aes(group = Activity), level = 0.70, type = "norm", geom = "path", size = 1, show.legend = FALSE) +
  #scale_colour_manual(values = custom_colors) +
  #scale_fill_manual(values = custom_colors) +
  labs(title = "Behaviour",
       x = "UMAP 1", y = "UMAP 2") +
  theme_classic()+
  theme(legend.position = "right")
Behaviour_Black_Gruntsbw 

#combine Copper plots into one

lp("patchwork")
lp("cowplot")

# Combine plots
Black_UMAP_combined <- (Behaviour_Black_Grunts_length|Behaviour_Black_Grunts_F75 | Behaviour_Black_Grunts  ) +
  theme(legend.position = "right")
Black_UMAP_combined 

ggsave("figures/CH2/Black_UMAP_combined_Behaviour.png", plot = Black_UMAP_combined , width = 10, height = 6, dpi = 300)

######################################################################################################################################

###
#DATA CLEANING NOTES: might be able to delete everything after this 

#TRENDS - F75 higher for larger fish and for aggressive approaching sounds

#Combine original variables with UMAP coordinates
umap_combined <- cbind(X_test, UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2])

# Correlation with UMAP dimensions
cor_UMAP1 <- sapply(X_test, function(x) cor(x, umap_result$layout[,1], method = "spearman"))
cor_UMAP2 <- sapply(X_test, function(x) cor(x, umap_result$layout[,2], method = "spearman"))

# Combine into a single data frame
cor_df <- data.frame(Variable = names(X_test),
                     UMAP1_corr = cor_UMAP1,
                     UMAP2_corr = cor_UMAP2)

# Order by strongest correlation
cor_df <- cor_df[order(abs(cor_df$UMAP1_corr) + abs(cor_df$UMAP2_corr), decreasing = TRUE), ]
print(cor_df)

#combine Copper plots into one

lp("patchwork")
lp("cowplot")

# Combine plots
Cop_UMAP_combined <- (Behaviour_Cop_Grunts|Behaviour_Cop_Knocks ) +
  theme(legend.position = "right")
Cop_UMAP_combined

ggsave("figures/CH2/Copper_UMAP_combined_Behaviour.png", plot = Cop_UMAP_combined, width = 10, height = 6, dpi = 300)






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
