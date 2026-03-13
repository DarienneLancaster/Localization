# library(dplyr)
# library(tidyr)
# library(tuneR)
# library(seewave)
# 
# # Path to selection tables
# sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo"
# 
# # List all selection table files
# sel_files <- list.files(sel_folder, pattern = "\\.txt$", full.names = TRUE)
# 
# # Test on first 2 files
# sel_files <- sel_files[1:2]
# 
# # Calibration gain
# sysgain <- -167.6
# 
# # Initialize empty list to store results
# all_results <- list()
# 
# # Loop through each selection table
# for (sel_file in sel_files) {
#   
#   message("Processing: ", sel_file)
#   
#   # # Import Raven selection table
#   # s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE) %>%
#   #   filter(Begin.File == basename(sel_file))
#   # 
#   # # Clean column names
#   # names(s) <- gsub(" ", ".", names(s))
#   # names(s) <- make.unique(names(s))
#   
#   # Import Raven selection table from the folder
#   s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE)
#   
#   # Clean column names: replace spaces and make unique BEFORE filtering
#   names(s) <- gsub(" ", ".", names(s))   # replace spaces with dots
#   names(s) <- make.unique(names(s))      # make duplicates unique
#   
#   # Now safe to filter by the specific file
#   s <- s %>% filter(Begin.File == basename(sel_file))
#   
#   # Convert column names to usable format
#   s$`Begin.Time.(s)`  # make sure column exists
#   
#   # Process table
#   s1 <- s %>%
#     select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
#            `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m) %>%
#     separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE) %>%
#     mutate(Selection_N = as.integer(Selection_N)) %>%
#     filter(Edit != "r")  # remove rejected rows
#   
#   # Coordinate lookup
#   coord_lookup <- s1 %>% select(Selection, x_m, y_m, z_m) %>% distinct()
#   s1 <- s1 %>%
#     left_join(coord_lookup, by = c("Selection_N" = "Selection"), suffix = c("", "_fromSel")) %>%
#     mutate(
#       x_m = coalesce(x_m_fromSel, x_m),
#       y_m = coalesce(y_m_fromSel, y_m),
#       z_m = coalesce(z_m_fromSel, z_m)
#     ) %>%
#     select(-ends_with("_fromSel"))
#   
#   # Remove rows where Selection_N is blank and matches another Selection
#   s1 <- s1 %>%
#     mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N)) %>%
#     filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))
#   
#   # Load corresponding WAV file
#   wav_file <- paste0(dirname(sel_file), "/", unique(s1$Begin.File))
#   if(!file.exists(wav_file)){
#     warning("WAV file not found: ", wav_file)
#     next
#   }
#   wav <- readWave(wav_file)
#   
#   # Extract selections & bandpass filter
#   extract_selection <- function(i, wav, df) {
#     start <- df[["Begin.Time.(s)"]][i]
#     end   <- df[["End.Time.(s)"]][i]
#     low  <- df[["Low.Freq.(Hz)"]][i]
#     high <- df[["High.Freq.(Hz)"]][i]
#     
#     seg <- extractWave(wav, from = start, to = end, xunit = "time")
#     bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, output = "Wave")
#   }
#   
#   selections <- lapply(seq_len(nrow(s1)), extract_selection, wav = wav, df = s1)
#   
#   # Calculate Received Level
#   calc_rl <- function(wave, sysgain){
#     x <- wave@left / (2^(wave@bit - 1))
#     rms <- sqrt(mean(x^2))
#     20 * log10(rms) - sysgain
#   }
#   RL <- sapply(selections, calc_rl, sysgain = sysgain)
#   s1$RL_dB_re_uPa <- RL
#   
#   # Calculate distance and Source Level
#   s1$distance_m <- sqrt(s1$x_m^2 + s1$y_m^2 + s1$z_m^2)
#   s1$SL_dB_re_uPa <- s1$RL_dB_re_uPa + 20 * log10(s1$distance_m)
#   
#   # Add processed table to list
#   all_results[[basename(sel_file)]] <- s1
# }
# 
# # Combine all processed files into one dataframe
# final_df <- bind_rows(all_results, .id = "source_file")
# 
# # Check
# str(final_df)
# head(final_df)
# 
# ########################
# # Path to selection tables
# sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo"
# 
# # List all selection table files
# sel_files <- list.files(sel_folder, pattern = "\\.txt$", full.names = TRUE)
# 
# # Test on first 2 files
# sel_files <- sel_files[1:2]
# 
# # Path to folder containing WAV files
# wav_folder <- "E:/Bamfield_2022_Large_Array/TI20220812_Taylor_Islet/AMAR/AMAR173.4.32000.M36-V35-100"
# 
# # Calibration gain
# sysgain <- -167.6
# 
# # Initialize empty list to store results
# all_results <- list()
# 
# # Loop through each selection table
# for (sel_file in sel_files) {
#   
#   message("Processing: ", sel_file)
#   
#   # Import Raven selection table from folder
#   s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE)
#   
#   # Clean column names: replace spaces and make unique BEFORE filtering
#   names(s) <- gsub(" ", ".", names(s))
#   names(s) <- make.unique(names(s))
#   
#   # Filter to just this selection table
#   s <- s %>% filter(Begin.File == basename(sel_file))
#   
#   # Ensure Begin.Time.(s) exists
#   s$`Begin.Time.(s)`
#   
#   # Process table
#   s1 <- s %>%
#     select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
#            `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m) %>%
#     separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE) %>%
#     mutate(Selection_N = as.integer(Selection_N)) %>%
#     filter(Edit != "r")  # remove rejected rows
#   
#   # Coordinate lookup
#   coord_lookup <- s1 %>% select(Selection, x_m, y_m, z_m) %>% distinct()
#   s1 <- s1 %>%
#     left_join(coord_lookup, by = c("Selection_N" = "Selection"), suffix = c("", "_fromSel")) %>%
#     mutate(
#       x_m = coalesce(x_m_fromSel, x_m),
#       y_m = coalesce(y_m_fromSel, y_m),
#       z_m = coalesce(z_m_fromSel, z_m)
#     ) %>%
#     select(-ends_with("_fromSel"))
#   
#   # Remove rows where Selection_N is blank and matches another Selection
#   s1 <- s1 %>%
#     mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N)) %>%
#     filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))
#   
#   # Load corresponding WAV file safely
#   wav_file <- file.path(wav_folder, unique(s1$Begin.File))
#   if(!file.exists(wav_file)){
#     warning("WAV file not found: ", wav_file)
#     next
#   }
#   wav <- readWave(wav_file)
#   
#   # Extract selections & bandpass filter
#   extract_selection <- function(i, wav, df) {
#     start <- df[["Begin.Time.(s)"]][i]
#     end   <- df[["End.Time.(s)"]][i]
#     low  <- df[["Low.Freq.(Hz)"]][i]
#     high <- df[["High.Freq.(Hz)"]][i]
#     
#     seg <- extractWave(wav, from = start, to = end, xunit = "time")
#     bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, output = "Wave")
#   }
#   
#   selections <- lapply(seq_len(nrow(s1)), extract_selection, wav = wav, df = s1)
#   
#   # Calculate Received Level
#   calc_rl <- function(wave, sysgain){
#     x <- wave@left / (2^(wave@bit - 1))
#     rms <- sqrt(mean(x^2))
#     20 * log10(rms) - sysgain
#   }
#   RL <- sapply(selections, calc_rl, sysgain = sysgain)
#   s1$RL_dB_re_uPa <- RL
#   
#   # Calculate distance and Source Level
#   s1$distance_m <- sqrt(s1$x_m^2 + s1$y_m^2 + s1$z_m^2)
#   s1$SL_dB_re_uPa <- s1$RL_dB_re_uPa + 20 * log10(s1$distance_m)
#   
#   # Add processed table to list
#   all_results[[basename(sel_file)]] <- s1
# }
# 
# # Combine all processed files into one dataframe
# final_df <- bind_rows(all_results, .id = "source_file")
# 
# # Check
# str(final_df)
# head(final_df)

############################################
# 
# # Path to selection tables
# sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo"
# 
# # List all selection table files
# sel_files <- list.files(sel_folder, pattern = "\\.txt$", full.names = TRUE)
# 
# # Test on first 2 files
# sel_files <- sel_files[1:2]
# 
# # Path to folder containing WAV files
# wav_folder <- "E:/Bamfield_2022_Large_Array/TI20220812_Taylor_Islet/AMAR/AMAR173.4.32000.M36-V35-100"
# 
# # Calibration gain
# sysgain <- -167.6
# 
# # Initialize empty list to store results
# all_results <- list()
# 
# # Loop through each selection table
# for (sel_file in sel_files) {
#   
#   message("Processing: ", sel_file)
#   
#   # Import Raven selection table from folder
#   s <- imp_raven(path = dirname(sel_file), all.data = TRUE, only.spectro.view = FALSE)
#   
#   # Clean column names: replace spaces and make unique BEFORE filtering
#   names(s) <- gsub(" ", ".", names(s))
#   names(s) <- make.unique(names(s))
#   
#   # Extract WAV base name from selection table filename
#   wav_base <- sub("\\.wav.*\\.txt$", ".wav", basename(sel_file))
#   
#   # Now safe to filter by Begin.File (matches actual WAV name)
#   s <- s %>% filter(Begin.File == wav_base)
#   
#   # Skip if no matching rows
#   if(nrow(s) == 0){
#     warning("No matching Begin.File found in selection table for ", wav_base)
#     next
#   }
#   
#   # Process table
#   s1 <- s %>%
#     select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
#            `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m) %>%
#     separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE) %>%
#     mutate(Selection_N = as.integer(Selection_N)) %>%
#     filter(Edit != "r")  # remove rejected rows
#   
#   # Coordinate lookup
#   coord_lookup <- s1 %>% select(Selection, x_m, y_m, z_m) %>% distinct()
#   s1 <- s1 %>%
#     left_join(coord_lookup, by = c("Selection_N" = "Selection"), suffix = c("", "_fromSel")) %>%
#     mutate(
#       x_m = coalesce(x_m_fromSel, x_m),
#       y_m = coalesce(y_m_fromSel, y_m),
#       z_m = coalesce(z_m_fromSel, z_m)
#     ) %>%
#     select(-ends_with("_fromSel"))
#   
#   # Remove rows where Selection_N is blank and matches another Selection
#   s1 <- s1 %>%
#     mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N)) %>%
#     filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))
#   
#   # Load corresponding WAV file safely
#   wav_file <- file.path(wav_folder, wav_base)
#   if(!file.exists(wav_file)){
#     warning("WAV file not found: ", wav_file)
#     next
#   }
#   wav <- readWave(wav_file)
#   
#   # Extract selections & bandpass filter
#   extract_selection <- function(i, wav, df) {
#     start <- df[["Begin.Time.(s)"]][i]
#     end   <- df[["End.Time.(s)"]][i]
#     low  <- df[["Low.Freq.(Hz)"]][i]
#     high <- df[["High.Freq.(Hz)"]][i]
#     
#     seg <- extractWave(wav, from = start, to = end, xunit = "time")
#     bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, output = "Wave")
#   }
#   
#   selections <- lapply(seq_len(nrow(s1)), extract_selection, wav = wav, df = s1)
#   
#   # Calculate Received Level
#   calc_rl <- function(wave, sysgain){
#     x <- wave@left / (2^(wave@bit - 1))
#     rms <- sqrt(mean(x^2))
#     20 * log10(rms) - sysgain
#   }
#   RL <- sapply(selections, calc_rl, sysgain = sysgain)
#   s1$RL_dB_re_uPa <- RL
#   
#   # Calculate distance and Source Level
#   s1$distance_m <- sqrt(s1$x_m^2 + s1$y_m^2 + s1$z_m^2)
#   s1$SL_dB_re_uPa <- s1$RL_dB_re_uPa + 20 * log10(s1$distance_m)
#   
#   # Add processed table to list
#   all_results[[basename(sel_file)]] <- s1
# }
# 
# # Combine all processed files into one dataframe
# final_df <- bind_rows(all_results, .id = "source_file")
# 
# # Check
# str(final_df)
# head(final_df)

# Path to selection tables
sel_folder <- "odata/FS_selection_tables_Edited_and_Original_combo"

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
    bwfilter(seg, f = seg@samp.rate, from = low, to = high, bandpass = TRUE, output = "Wave")
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
final_df <- bind_rows(all_results, .id = "source_file")

# Check
str(final_df)
head(final_df)
mean(final_df$SL_dB_re_uPa, na.rm = TRUE)
