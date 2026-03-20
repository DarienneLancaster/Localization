###Calculate received and source levels for fish sounds

sound<-read.csv("wdata/Sound_Species_Behaviour_Length_20250213.csv", header = TRUE)



sound_t<- sound%>% filter(is.na(z_m))

sound$Energy..dB.FS.

####
#bring in x, y, z information from original selections to add to missing/edited selections

###load edited Raven selection tables
N<-imp_raven(path = paste0("odata/All_Localizations_Daylight_LA_filtered_2_1_FS"), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.

N_t <- N %>%
  select(`Begin Path`, Selection, x_m, y_m, z_m) %>%   # keep only relevant columns
  rename(
    Begin.Path = `Begin Path`,
    Selection_N = Selection
  )

#fill in missin x,y,z data 
sound <- sound %>%
  left_join(
    N_t, 
    by = c("Begin.Path", "Selection_N"),
    suffix = c("", "_Nt")  # temporary suffix for N_t columns
  ) %>%
  mutate(
    x_m = ifelse(is.na(x_m), x_m_Nt, x_m),
    y_m = ifelse(is.na(y_m), y_m_Nt, y_m),
    z_m = ifelse(is.na(z_m), z_m_Nt, z_m)
  ) %>%
  select(-ends_with("_Nt"))%>%  # remove temporary columns
  arrange(Begin.Path)%>%
  filter(ID_confidence != 3, Edit != "a")


# Sysgain value
sysgain <- -167.6

# Add new column Rec_Lev and subtract 20dB to account for factor 10 amplification
sound$Rec_Lev <- (sound$Energy..dB.FS.) - 20 - sysgain # maybe it was amplified by factor 10 twice?  Once when Aislyn did it and once when I reopened them?


sound$Rec_Lev

SLtest<-sound%>%
  filter(fishID == "0020_20220814")


sound1<-sound%>%
  select(Site, Selection, Begin.File, Genus, Species, t, ID_confidence, mean_length, x_m, y_m, z_m, Energy..dB.FS., Rec_Lev, fishID)

# calculate distance
sound1$distance_m <- sqrt(sound1$x_m^2 + sound1$y_m^2 + sound1$z_m^2)

# calculate source level using spherical spreading
sound1$Source_Lev <- sound1$Rec_Lev + 20*log10(sound1$distance_m)

########
#calculate mean and sd source level for each species grunts and knocks

summary_by_species <- sound1 %>%
  group_by(Species, t) %>%
  filter(Species != "", t != "e", ID_confidence != 3)%>%
  summarise(
    n       = n(),
    mean_SL = mean(Source_Lev, na.rm = TRUE),
    sd_SL   = sd(Source_Lev, na.rm = TRUE),
    max_SL  = max(Source_Lev, na.rm = TRUE),
    min_SL  = min(Source_Lev, na.rm = TRUE)
  )
summary_by_species

######
#create histograms of source levels for each species 

SLhist<-ggplot(sound1, aes(x = Source_Lev)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(x = "Source Level", y = "Count",
       title = "Histogram of Source_Lev by Species")
SLhist

quartiles_75 <- sound1 %>%
  group_by(Species) %>%
  summarise(
    Q3_Source_Lev = quantile(Source_Lev, 0.75, na.rm = TRUE)
  )

quartiles_75

mean(quartiles_75$Q3_Source_Lev)

###
#investigate why quillback have negative relationship between length and Source LEvel

QBsound<-sound1%>%
  filter(Species == "maliger", t == "d")%>%
  group_by(fishID, mean_length)%>%
  summarise(mean_SL = mean(Source_Lev))

QB_SL<-ggplot(QBsound,
       aes(x = mean_length, y = mean_SL, color = fishID)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Mean Length",
    y = "Source Level",
    color = "Fish ID",
    title = "Source Level vs Mean Length for Species: maliger"
  )
QB_SL

QB_SL <- ggplot(QBsound,
                aes(x = mean_length, y = mean_SL, color = fishID)) +
  geom_point(size = 3) +
  geom_text(aes(label = fishID), vjust = -0.7, size = 3) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Mean Length",
    y = "Source Level",
    color = "Fish ID",
    title = "Source Level vs Mean Length for Species: maliger"
  )

QB_SL
#answer:  seems like the trend is still negative across sizes but it works better to average all SL values
#across individuals so repeated calls from the same fish don't overly skew results. I looked at individual sounds
#in Raven and they seem accurate, not influence by background noise, just louder and quieter sounds.
#Also need to average all SL values
#for other species in plots below to make sure repeated calls by individual fish aren't skewing results.

#############

#add common names
summary_by_species$Common<- ifelse(summary_by_species$Species == "caurinus", "Copper Rockfish",
                               ifelse(summary_by_species$Species == "maliger", "Quillback Rockfish",
                                      ifelse(summary_by_species$Species == "pinniger", "Canary Rockfish",
                                             ifelse(summary_by_species$Species == "miniatus", "Vermillion Rockfish",
                                                    ifelse(summary_by_species$Species == "melanops", "Black Rockfish",
                                                           ifelse(summary_by_species$Species == "elongatus", "Lingcod",
                                                                  ifelse(summary_by_species$Species == "nicholsii", "Black Eyed Gobie",
                                                                         ifelse(summary_by_species$Species == "marmoratus", "Cabezon",
                                                                                ifelse(summary_by_species$Species == "stellatus", "Flatfish spp",
                                                                                       ifelse(summary_by_species$Species == "pictus", "Painted Greenling",
                                                                                              ifelse(summary_by_species$Species == "decagrammus", "Kelp Greenling",
                                                                                                     ifelse(summary_by_species$Species == "vacca", "Pile Perch", "other"))))))))))))



summary_by_species1<-summary_by_species%>%
  ungroup()%>%
  select(-Species)%>%
  rename(Species = Common)%>%
  select(Species, everything())  # move Species to first column

library(flextable)

############


summary_by_species1 <- summary_by_species1 %>%
  mutate(t = recode(t,
                    d = "Knocks",
                    g = "Grunts"))%>%
  arrange(Species, t)

# Identify rows where t == "Knocks"
knock_rows <- which(summary_by_species1$t == "Knocks")

summary_by_species1 <- summary_by_species1 %>%
  mutate(across(
    c(mean_SL, sd_SL, max_SL, min_SL),
    ~ round(., 1)
  ))

summary_by_species_ft <- flextable(summary_by_species1) %>%
  set_header_labels(
    Species    = "Species",
    t          = "Sound Type",
    n          = "n",
    mean_SL    = "Mean \nSource Level (dB)",
    sd_SL      = "SD \nSource Level (dB)",
    max_SL     = "Maximum \nSource Level (dB)",
    min_SL     = "Minimum \nSource Level (dB)"
  ) %>%
  fontsize(size = 14, part = "all") %>%
  theme_booktabs()

# Add thin bottom border only to Knock rows
summary_by_species_ft <- border(
  summary_by_species_ft,
  i = knock_rows,
  j = 1:ncol(summary_by_species1),
  border.bottom = fp_border(width = 0.5),
  part = "body"
)

summary_by_species_ft <- autofit(summary_by_species_ft)

summary_by_species_ft <- summary_by_species_ft %>%
  fontsize(size = 26, part = "all") %>%   # increase from 14 → 18 (or 20)
  autofit()

summary_by_species_ft

save_as_image(
  summary_by_species_ft,
  path = "figures/CH3/source_level_summary_by_species.png",
  zoom = 2  # increase this to make text larger in the PNG
)

save_as_image(
  summary_by_species_ft,
  path = "figures/CH3/Source_Level_summary_by_species.png"
)

doc <- read_docx() %>%
  body_add_flextable(summary_by_species_ft)

print(doc, target = "figures/CH3/Source_Level_summary_by_species.docx")
############

## calculate detection distances at different background noise levels

# Define background noise levels
noise_levels <- c(81, 99, 112)  # in dB
#noise levels pulled from 5%, 50%, and 95% quartile ranges for SPL across DR and TI

# Compute detection distances
detection_distances <- summary_by_species1 %>%
  rowwise() %>%   # work row by row
  mutate(
    dist_Q5 = 10^((mean_SL - 81)/20),
    dist_Q50 = 10^((mean_SL - 99)/20),
    dist_Q95 = 10^((mean_SL - 112)/20)
  ) %>%
  ungroup()
detection_distances


# Round numeric columns to 1 decimal for display
detection_distances_rounded <- detection_distances %>%
  mutate(across(c(mean_SL, sd_SL, max_SL, min_SL, dist_Q5, dist_Q50, dist_Q95),
                ~ round(., 1)))

# Select only the relevant columns
detection_subset <- detection_distances_rounded %>%
  select(Species, t, dist_Q5, dist_Q50, dist_Q95)

#load SPL data for full deployments
SPL_TI_dd<-read.csv("odata/SPL_LargeArray/SPL_1min_fulldeployment_TI_20to1000Hz_good20260304.csv", header = TRUE)
SPL_DR_dd<-read.csv("odata/SPL_LargeArray/SPL_1min_fulldeployment_DR_20to1000Hz_good20260304.csv", header = TRUE)

#join as one dataframe
SPL_all_dd<-rbind(SPL_TI_dd, SPL_DR_dd)

#calculate quartile ranges for SPL across all sites

SPL_all_Qsum<-SPL_all_dd%>%
  summarise(
    Q5 = quantile(SPL, 0.05, na.rm = TRUE),  # 5th percentile
    Q25 = quantile(SPL, 0.25, na.rm = TRUE),  # 25th percentile
    Q50 = quantile(SPL, 0.5, na.rm = TRUE),  # 50th percentile
    Q75 = quantile(SPL, 0.75, na.rm = TRUE),  # 75th percentile
    Q95 = quantile(SPL, 0.95, na.rm = TRUE),  # 95th percentile
    IQR = IQR(SPL, na.rm = TRUE),  # Interquartile range
    Min = min(SPL, na.rm = TRUE),
    Max = max(SPL, na.rm = TRUE)
  )
SPL_all_Qsum



# ft_detection1 <- flextable(detection_subset) %>%
#   # Set the second row labels
#   set_header_labels(
#     Species     = "\n\nSpecies",
#     t           = "\n\nSound Type",
#     dist_Q5     = "\n\nNoise Level (5%)",
#     dist_Q50    = "Detection Distance (m) \n\nNoise Level (50%)",
#     dist_Q95    = "\n\nNoise Level (95%)"
#   ) %>%
#   fontsize(size = 14, part = "all") %>%
#   theme_booktabs() %>%
#   autofit()

ft_detection1 <- flextable(detection_subset) %>%
  set_header_labels(
    Species  = "Species",
    t        = "Sound Type",
    dist_Q5  = "Noise Level (5%)",
    dist_Q50 = "Detection Distance (m)\nNoise Level (50%)",
    dist_Q95 = "Noise Level (95%)"
  ) %>%
  fontsize(size = 14, part = "all") %>%
  theme_booktabs() %>%
  autofit()

ft_detection1 <- flextable(detection_subset) %>%
  set_header_labels(
    Species  = "Species",
    t        = "Sound Type",
    dist_Q5  = "Noise Level (5%)",
    dist_Q50 = "Noise Level (50%)",
    dist_Q95 = "Noise Level (95%)"
  ) %>%
  add_header_row(
    values = c("", "", "", "Detection Distance (m)",""),
    colwidths = c(1, 1, 1,1,1)
  ) %>%
  align(align = "left", part = "header") %>%   # left align all header cells
  fontsize(size = 14, part = "all") %>%
  theme_booktabs() %>%
  autofit()
# Identify rows where t == "Knocks"
knock_rows <- which(detection_subset$t == "Knocks")

ft_detection1 <- border(
  ft_detection1,
  i = knock_rows,
  j = 1:ncol(detection_subset),
  border.bottom = fp_border(width = 0.5),
  part = "body"
)

ft_detection1


save_as_image(
  ft_detection1,
  path = "figures/CH3/Detection_distance_summary.png",
  zoom = 2  # increase this to make text larger in the PNG
)


doc <- read_docx() %>%
  body_add_flextable(ft_detection1)

print(doc, target = "figures/CH3/Detection_distance_summary.docx")

############
Sound2<-sound1%>%
  filter(t == "d")

#add common names
sound1$Common<- ifelse(sound1$Species == "caurinus", "Copper Rockfish",
                                   ifelse(sound1$Species == "maliger", "Quillback Rockfish",
                                          ifelse(sound1$Species == "pinniger", "Canary Rockfish",
                                                 ifelse(sound1$Species == "miniatus", "Vermillion Rockfish",
                                                        ifelse(sound1$Species == "melanops", "Black Rockfish",
                                                               ifelse(sound1$Species == "elongatus", "Lingcod",
                                                                      ifelse(sound1$Species == "nicholsii", "Black Eyed Gobie",
                                                                             ifelse(sound1$Species == "marmoratus", "Cabezon",
                                                                                    ifelse(sound1$Species == "stellatus", "Flatfish spp",
                                                                                           ifelse(sound1$Species == "pictus", "Painted Greenling",
                                                                                                  ifelse(sound1$Species == "decagrammus", "Kelp Greenling",
                                                                                                         ifelse(sound1$Species == "vacca", "Pile Perch", "other"))))))))))))



length_knocks_all<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger|vacca|decagrammus|elongatus|melanops|pinniger"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_CP<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_QB<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_CA<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "pinniger"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_BL<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "melanops"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_L<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "elongatus"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_PP<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "vacca"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_knocks_KG<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "d", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "decagrammus"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))



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
  "Pile Perch"         = length_knocks_PP,
  "Kelp Greenling"     = length_knocks_KG
)

response_vars <- c("mean_SL")

sapply(species_data, function(df) sum(!is.na(df$mean_length) & !is.na(df$mean_SL)))

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
lp("broom")

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
    Response == "Source_Lev" ~ "Source Level",
    TRUE ~ Response  # keep any other values unchanged
  )) %>%
  arrange(Response, Species)  # sort alphabetically
# Remove the Response column from the data
model_summary<- model_summary %>%
  select(-Response)

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
save_as_image(x = ft, path = "figures/CH3/LM_Results_Knocks_SourceLevel_Length.png")

# Create Word document
doc <- read_docx() %>%
  body_add_par("Model Summary Table", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")  # adds spacing

# Save editable Word document
print(doc, target = "figures/CH3/LM_Results_Knocks_SourceLevel_Length.docx")

################################


# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" ,
  "Kelp Greenling" = "#99E472"
)


##
lp('purrr')
lp('broom')

#Linear regression Frequency 50% vs all species (KNOCKS)

# Determine significance and assign linetypes
linetype_map <- length_knocks_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(mean_SL ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_knocks_all_linetype <- left_join(length_knocks_all, linetype_map, by = "Common")

knocks_all_SL <- ggplot(length_knocks_all_linetype, aes(x = mean_length, y = mean_SL, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "Fish length (mm)",
    y = "Source Level (dB)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a) Knocks",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

knocks_all_SL
###########################

#do Grunts

length_grunts_all<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus|maliger|melanops"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_grunts_CP<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "caurinus"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_grunts_QB<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "maliger"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))

length_grunts_BL<-sound1%>%
  filter(!is.na(mean_length)) %>%
  filter(t == "g", ID_confidence == 1|2,  Selection != 3030, str_detect(Species, "melanops"))%>%
  group_by(fishID, mean_length, Common)%>%
  summarize(mean_SL = mean(Source_Lev))




###############################
#run loop for all four knock frequency variables 

#---------------------------------------------------
# 1. Define species datasets and variable names
#---------------------------------------------------

species_data <- list(
  "Copper Rockfish"    = length_grunts_CP,
  "Quillback Rockfish" = length_grunts_QB,
  "Black Rockfish"     = length_grunts_BL
)

response_vars <- c("mean_SL")

sapply(species_data, function(df) sum(!is.na(df$mean_length) & !is.na(df$mean_SL)))

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
lp("broom")

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
    Response == "Source_Lev" ~ "Source Level",
    TRUE ~ Response  # keep any other values unchanged
  )) %>%
  arrange(Response, Species)  # sort alphabetically
# Remove the Response column from the data
model_summary<- model_summary %>%
  select(-Response)

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
save_as_image(x = ft, path = "figures/CH3/LM_Results_grunts_SourceLevel_Length.png")

# Create Word document
doc <- read_docx() %>%
  body_add_par("Model Summary Table", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")  # adds spacing

# Save editable Word document
print(doc, target = "figures/CH3/LM_Results_grunts_SourceLevel_Length.docx")

################################


# Define custom colors for species
custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" ,
  "Kelp Greenling" = "#99E472"
)


##
lp('purrr')
lp('broom')

#Linear regression Frequency 50% vs all species (grunts)

# Determine significance and assign linetypes
linetype_map <- length_grunts_all %>%
  group_by(Common) %>%
  group_modify(~ {
    model <- lm(mean_SL ~ mean_length, data = .x)
    p_val <- tidy(model) %>% filter(term == "mean_length") %>% pull(p.value)
    linetype <- ifelse(p_val < 0.05, "dashed", "solid")
    tibble(linetype = linetype)
  })

# Merge linetype info into original data
length_grunts_all_linetype <- left_join(length_grunts_all, linetype_map, by = "Common")

grunts_all_SL <- ggplot(length_grunts_all_linetype, aes(x = mean_length, y = mean_SL, color = Common)) +
  geom_point(aes(color = Common), alpha = 0.2) +
  geom_smooth(aes(group = Common, color = Common, linetype = linetype), method = "lm", se = FALSE) +
  labs(
    x = "Fish length (mm)",
    y = "Source Level (dB)"
  ) +
  scale_color_manual(name = "Species", values = custom_colors) +  # apply custom colors and rename legend
  guides(linetype = "none") +             
  theme_classic()+
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b) Grunts",
    hjust = -0.5, vjust = 1.5,
    size = 6
  )

grunts_all_SL

All_Sl<-knocks_all_SL+grunts_all_SL
All_Sl
ggsave("figures/CH3/Length_SourceLevel_Knocks_Grunts_meanSLforfishID.png", plot = All_Sl, width = 10, height = 6, dpi = 300)

