# library(dplyr)
# library(tidyr)
# library(tuneR)
# library(seewave)

#############################################################
#Taylor Islet Source Level loop

# Path to selection tables
sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo/TI"

# List all selection table files
sel_files <- list.files(sel_folder, pattern = "\\.txt$", full.names = TRUE)

# Test on first 2 files
#sel_files <- sel_files[1:2]

# Path to folder containing WAV files
wav_folder <- "E:/Bamfield_2022_Large_Array/TI20220812_Taylor_Islet/AMAR/AMAR173.4.32000.M36-V35-100"

# Calibration gain
sysgain <- -167.6

# Initialize empty list to store results
all_results <- list()

# Loop through each selection table
for (sel_file in sel_files) {
  
  message("Processing: ", sel_file)
  
  # Import Raven selection table from folder
  s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE)
  
  # Clean column names: replace spaces and make unique BEFORE filtering
  names(s) <- gsub(" ", ".", names(s))
  names(s) <- make.unique(names(s))
  
  # Extract WAV base name from selection table filename
  wav_base <- sub("\\.wav.*\\.txt$", ".wav", basename(sel_file))
  
  # Filter to Begin.File
  s <- s %>% filter(Begin.File == wav_base)
  
  if(nrow(s) == 0){
    warning("No matching Begin.File found in selection table for ", wav_base)
    next
  }
  
  # Process table
  s1 <- s %>%
    select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
           `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m) %>%
    separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE) %>%
    mutate(Selection_N = as.integer(Selection_N)) %>%
    filter(is.na(Edit) | Edit != "r")  # remove rejected rowss1 <- s1 %>%
  
  
  # Coordinate lookup for Selection_N replacement
  coord_lookup <- s1 %>% select(Selection, x_m, y_m, z_m) %>% distinct()
  s1 <- s1 %>%
    left_join(coord_lookup, by = c("Selection_N" = "Selection"), suffix = c("", "_fromSel")) %>%
    mutate(
      x_m = coalesce(x_m_fromSel, x_m),
      y_m = coalesce(y_m_fromSel, y_m),
      z_m = coalesce(z_m_fromSel, z_m)
    ) %>%
    select(-ends_with("_fromSel"))
  
  # Remove rows where Selection_N is blank and matches another Selection
  s1 <- s1 %>%
    mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N)) %>%
    filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))
  
  # Convert coordinates to numeric
  s1 <- s1 %>%
    mutate(
      x_m = as.numeric(x_m),
      y_m = as.numeric(y_m),
      z_m = as.numeric(z_m)
    )
  
  # Load corresponding WAV file safely
  wav_file <- file.path(wav_folder, wav_base)
  if(!file.exists(wav_file)){
    warning("WAV file not found: ", wav_file)
    next
  }
  wav <- readWave(wav_file)
  
  # Extract selections & bandpass filter
  extract_selection <- function(i, wav, df) {
    start <- df[["Begin.Time.(s)"]][i]
    end   <- df[["End.Time.(s)"]][i]
    low  <- df[["Low.Freq.(Hz)"]][i]
    high <- df[["High.Freq.(Hz)"]][i]
    
    seg <- extractWave(wav, from = start, to = end, xunit = "time")
    bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, n=2, output = "Wave")
  }
  
  selections <- lapply(seq_len(nrow(s1)), extract_selection, wav = wav, df = s1)
  
  # Calculate Received Level safely
  calc_rl <- function(wave, sysgain){
    x <- wave@left / (2^(wave@bit - 1))
    rms <- sqrt(mean(x^2))
    20 * log10(rms) - sysgain
  }
  RL <- sapply(selections, calc_rl, sysgain = sysgain)
  s1$RL_dB_re_uPa <- as.numeric(RL)
  
  # Calculate distance and Source Level safely
  s1$distance_m <- NA
  s1$SL_dB_re_uPa <- NA
  valid_idx <- !is.na(s1$RL_dB_re_uPa) & !is.na(s1$x_m) & !is.na(s1$y_m) & !is.na(s1$z_m)
  
  s1$distance_m[valid_idx] <- sqrt(
    s1$x_m[valid_idx]^2 + s1$y_m[valid_idx]^2 + s1$z_m[valid_idx]^2
  )
  
  s1$SL_dB_re_uPa[valid_idx] <- s1$RL_dB_re_uPa[valid_idx] + 20 * log10(s1$distance_m[valid_idx])
  
  # Add processed table to list
  all_results[[basename(sel_file)]] <- s1
}

# Combine all processed files into one dataframe
final_df_TI <- bind_rows(all_results, .id = "source_file")

# Check
str(final_df_TI)
head(final_df_TI)
mean(final_df_TI$SL_dB_re_uPa, na.rm = TRUE)

#################################################################################
#Danger Rocks Source Level loop

# Path to selection tables
sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo/DR"

# List all selection table files
sel_files <- list.files(sel_folder, pattern = "\\.txt$", full.names = TRUE)

# Test on first 2 files
#sel_files <- sel_files[1:2]

# Path to folder containing WAV files
wav_folder <- "E:/Bamfield_2022_Large_Array/DR20220908_Danger_Rocks_Danvers/AMAR/AMAR173.4.32000.M36-V35-100"

# Calibration gain
sysgain <- -167.6

# Initialize empty list to store results
all_results <- list()

# Loop through each selection table
for (sel_file in sel_files) {
  
  message("Processing: ", sel_file)
  
  # Import Raven selection table from folder
  s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE)
  
  # Clean column names: replace spaces and make unique BEFORE filtering
  names(s) <- gsub(" ", ".", names(s))
  names(s) <- make.unique(names(s))
  
  # Extract WAV base name from selection table filename
  wav_base <- sub("\\.wav.*\\.txt$", ".wav", basename(sel_file))
  
  # Filter to Begin.File
  s <- s %>% filter(Begin.File == wav_base)
  
  if(nrow(s) == 0){
    warning("No matching Begin.File found in selection table for ", wav_base)
    next
  }
  
  # Process table
  s1 <- s %>%
    select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
           `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m) %>%
    separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE) %>%
    mutate(Selection_N = as.integer(Selection_N)) %>%
    filter(is.na(Edit) | Edit != "r")  # remove rejected rowss1 <- s1 %>%
  
  
  # Coordinate lookup for Selection_N replacement
  coord_lookup <- s1 %>% select(Selection, x_m, y_m, z_m) %>% distinct()
  s1 <- s1 %>%
    left_join(coord_lookup, by = c("Selection_N" = "Selection"), suffix = c("", "_fromSel")) %>%
    mutate(
      x_m = coalesce(x_m_fromSel, x_m),
      y_m = coalesce(y_m_fromSel, y_m),
      z_m = coalesce(z_m_fromSel, z_m)
    ) %>%
    select(-ends_with("_fromSel"))
  
  # Remove rows where Selection_N is blank and matches another Selection
  s1 <- s1 %>%
    mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N)) %>%
    filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))
  
  # Convert coordinates to numeric
  s1 <- s1 %>%
    mutate(
      x_m = as.numeric(x_m),
      y_m = as.numeric(y_m),
      z_m = as.numeric(z_m)
    )
  
  # Load corresponding WAV file safely
  wav_file <- file.path(wav_folder, wav_base)
  if(!file.exists(wav_file)){
    warning("WAV file not found: ", wav_file)
    next
  }
  wav <- readWave(wav_file)
  
  # Extract selections & bandpass filter
  extract_selection <- function(i, wav, df) {
    start <- df[["Begin.Time.(s)"]][i]
    end   <- df[["End.Time.(s)"]][i]
    low  <- df[["Low.Freq.(Hz)"]][i]
    high <- df[["High.Freq.(Hz)"]][i]
    
    seg <- extractWave(wav, from = start, to = end, xunit = "time")
    bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, n = 2,  output = "Wave")
  }
  
  selections <- lapply(seq_len(nrow(s1)), extract_selection, wav = wav, df = s1)
  
  # Calculate Received Level safely
  calc_rl <- function(wave, sysgain){
    x <- wave@left / (2^(wave@bit - 1))
    rms <- sqrt(mean(x^2))
    20 * log10(rms) - sysgain
  }
  RL <- sapply(selections, calc_rl, sysgain = sysgain)
  s1$RL_dB_re_uPa <- as.numeric(RL)
  
  # Calculate distance and Source Level safely
  s1$distance_m <- NA
  s1$SL_dB_re_uPa <- NA
  valid_idx <- !is.na(s1$RL_dB_re_uPa) & !is.na(s1$x_m) & !is.na(s1$y_m) & !is.na(s1$z_m)
  
  s1$distance_m[valid_idx] <- sqrt(
    s1$x_m[valid_idx]^2 + s1$y_m[valid_idx]^2 + s1$z_m[valid_idx]^2
  )
  
  s1$SL_dB_re_uPa[valid_idx] <- s1$RL_dB_re_uPa[valid_idx] + 20 * log10(s1$distance_m[valid_idx])
  
  # Add processed table to list
  all_results[[basename(sel_file)]] <- s1
}

# Combine all processed files into one dataframe
final_df_DR <- bind_rows(all_results, .id = "source_file")

# Check
str(final_df_DR)
head(final_df_DR)
mean(final_df_DR$SL_dB_re_uPa, na.rm = TRUE)

#join DR and TI dataframes
final_df_ALL<-rbind(final_df_TI, final_df_DR)

write.csv(final_df_ALL, file = "wdata/Source_Levels_TI_DR_BW2_Waveform.csv", row.names = FALSE)

final_df_ALL<-read.csv("wdata/Source_Levels_TI_DR_BW2_Waveform.csv", header = TRUE)

#bring in fish sound species and length data and link to source level data

sound<-read.csv("wdata/Sound_Species_Behaviour_Length_20250213.csv", header = TRUE)

#testing join (seems like some selection numbers were incorrectly labeled so didn't get coordinates transfered - those sounds will be dropped)
sound1<-sound%>%
  select(Selection, Begin.File, Selection_N, Edit, t, Species, fishID, ID_confidence, mean_length)

# %>%
#   filter(Begin.File == "AMAR173.4.20220813T150710Z.wav")
# 
# final_df_TI2<-final_df_TI%>%
#   filter(Begin.File == "AMAR173.4.20220813T150710Z.wav")%>%
#   filter(!is.na(SL_dB_re_uPa))

SL_spec<-left_join(sound1, final_df_ALL, by = c("Begin.File","Selection"))


#add common names
SL_spec$Common<- ifelse(SL_spec$Species == "caurinus", "Copper Rockfish",
                                   ifelse(SL_spec$Species == "maliger", "Quillback Rockfish",
                                          ifelse(SL_spec$Species == "pinniger", "Canary Rockfish",
                                                 ifelse(SL_spec$Species == "miniatus", "Vermillion Rockfish",
                                                        ifelse(SL_spec$Species == "melanops", "Black Rockfish",
                                                               ifelse(SL_spec$Species == "elongatus", "Lingcod",
                                                                      ifelse(SL_spec$Species == "nicholsii", "Black Eyed Gobie",
                                                                             ifelse(SL_spec$Species == "marmoratus", "Cabezon",
                                                                                    ifelse(SL_spec$Species == "stellatus", "Flatfish spp",
                                                                                           ifelse(SL_spec$Species == "pictus", "Painted Greenling",
                                                                                                  ifelse(SL_spec$Species == "decagrammus", "Kelp Greenling",
                                                                                                         ifelse(SL_spec$Species == "vacca", "Pile Perch", "other"))))))))))))


custom_colors <- c(
  "Black Rockfish" = "#003399",   
  "Quillback Rockfish" = "#FF6600", 
  "Copper Rockfish" = "#33CC99",
  "Lingcod" = "#33CCFF",
  "Canary Rockfish" = "#FFCC00",
  "Pile Perch" = "#9900CC" ,
  "Kelp Greenling" = "#99E472",
  "Vermillion Rockfish" = "#FF3366"
)
#try a plot for knocks

SL_spec1 <- SL_spec %>%
  filter(ID_confidence == 1, t == "d", Edit.x != "a") %>%
  group_by(fishID,Common, mean_length, t, ID_confidence) %>%
  summarise(mean_SL = mean(SL_dB_re_uPa, na.rm = TRUE),
            n = n())

SL_K <- ggplot(SL_spec1, aes(x = mean_length, y = mean_SL, color = Common)) +
  geom_point(size = 2) +
  geom_smooth(aes(fill = Common), method = "lm", se = TRUE, alpha = 0.1, show.legend = FALSE) +  # map fill
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  # match shading to colors
  theme_classic() +
  labs(
    title = "a)",
    x = "Mean Length",
    y = "Source Level (dB re 1 µPa)",
    color = "Species",
    fill = "Species"
  )

SL_K

ggsave("figures/CH3/Length_SourceLevel_Knocks_Mar17.png", plot = SL_K, width = 10, height = 6, dpi = 300)

#try a plot for grunts

SL_spec2 <- SL_spec %>%
  filter(ID_confidence == 1, t == "g", Edit.x != "a") %>%
  group_by(fishID, Common, mean_length, t, ID_confidence) %>%
  summarise(mean_SL = mean(SL_dB_re_uPa, na.rm = TRUE),
            n = n())

SL_G <- ggplot(SL_spec2, aes(x = mean_length, y = mean_SL, color = Common)) +
  geom_point(size = 2) +
  geom_smooth(aes(fill = Common), method = "lm", se = TRUE, alpha = 0.1, show.legend = FALSE) +  # map fill
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +  # match shading to colors
  theme_classic() +
  labs(
    title = "b)",
    x = "Mean Length",
    y = "Source Level (dB re 1 µPa)",
    color = "Species",
    fill = "Species"
  )

SL_G

ggsave("figures/CH3/Length_SourceLevel_Grunts_Mar17.png", plot = SL_G, width = 10, height = 6, dpi = 300)

#combine into one plot
All_SL<-SL_K+SL_G
All_SL

ggsave("figures/CH3/Length_SourceLevel_AllKandG_Mar17.png", plot = All_SL, width = 10, height = 6, dpi = 300)

#change colours and plot formatting to match my papers
#create histograms for each species knocks and grunts (don't use mean per individual?)
#create summary table for each species knocks and grunts



#how to link fish sound data to selections?  does Selection number work correctly? or do I need Selection_N?
