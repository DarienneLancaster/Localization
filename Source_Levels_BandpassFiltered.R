#calculate fish sound source levels using bandpass filtering and waveform energy calculations

s<-read.csv("odata/FS_selection_tables_Edited_and_Original_combo/AMAR173.4.20220813T020710Z.wav.chan0.Table.1.selections.FS_E.txt")

s<-imp_raven(path = paste0("odata/FS_selection_tables_Edited_and_Original_combo"), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.

names(s) <- gsub(" ", ".", names(s))
which(names(s) %in% names(s)[duplicated(names(s))])
names(s) <- make.unique(names(s))

s$`Begin.Time.(s)`
s1<-s%>%
  filter(Begin.File == "AMAR173.4.20220813T020710Z.wav")%>%
  select(Begin.File, Selection, Edits, `Begin.Time.(s)`, `End.Time.(s)`,
         `Low.Freq.(Hz)`, `High.Freq.(Hz)`, x_m, y_m, z_m, ) %>%
  separate(Edits, into = c("Edit", "Selection_N"), sep = "_", remove = FALSE)%>%
  mutate(Selection_N = as.integer(Selection_N))

str(s1)

# create a lookup table from Selection → coordinates
coord_lookup <- s1 %>%
  select(Selection, x_m, y_m, z_m) %>%
  distinct()  # just in case

# join back to s1 where Selection_N matches Selection
s1 <- s1 %>%
  left_join(coord_lookup, by = c("Selection_N" = "Selection"),
            suffix = c("", "_fromSel"))

# now, for rows where x_m_fromSel exists, replace x_m, y_m, z_m
s1 <- s1 %>%
  mutate(
    x_m = coalesce(x_m_fromSel, x_m),
    y_m = coalesce(y_m_fromSel, y_m),
    z_m = coalesce(z_m_fromSel, z_m)
  ) %>%
  select(-ends_with("_fromSel"))  # clean up temporary columns

s1<-s1%>%
  filter(Edit != "r")

# ensure Selection_N is NA where blank
s1 <- s1 %>%
  mutate(Selection_N = ifelse(Selection_N == "", NA, Selection_N))

# remove rows meeting the condition
s1 <- s1 %>%
  filter(!(is.na(Selection_N) & Selection %in% Selection_N[!is.na(Selection_N)]))

############
#load in AMAR file

lp("tuneR")

#load in test .wav file
wav <- readWave("E:/Bamfield_2022_Large_Array/TI20220812_Taylor_Islet/AMAR/AMAR173.4.32000.M36-V35-100/AMAR173.4.20220813T020710Z.wav")

#get sample rate 32000
fs <- wav@samp.rate

#use begin and end time, and high low frequency from selection tables in dataframe s1 to bandpass filter
extract_selection <- function(i, wav, df) {
  
  start <- df[["Begin.Time.(s)"]][i]
  end   <- df[["End.Time.(s)"]][i]
  
  low  <- df[["Low.Freq.(Hz)"]][i]
  high <- df[["High.Freq.(Hz)"]][i]
  
  # extract segment first
  seg <- extractWave(
    wav,
    from = start,
    to = end,
    xunit = "time"
  )
  
  # bandpass filter segment
  seg_filt <- bwfilter(
    seg,
    f = seg@samp.rate,
    from = low,
    to = high,
    bandpass = TRUE,
    output = "Wave"
  )
  
  return(seg_filt)
}

selections <- lapply(
  seq_len(nrow(s1)),
  function(i) extract_selection(i, wav, s1)
)

plot(selections[[25]])

#calculate Received levels

#gain from calibration
sysgain <- -167.6   # dB re 1 FS/uPa

calc_rl <- function(wave, sysgain){
  
  # normalized waveform (-1 to 1)
  x <- wave@left / (2^(wave@bit - 1))
  
  # RMS amplitude
  rms <- sqrt(mean(x^2))
  
  # convert to Received level
  rl <- 20 * log10(rms) - sysgain
  
  return(rl)
}

#calculate received levels
RL <- sapply(selections, calc_rl, sysgain = sysgain)

#add to dataframe
s1$RL_dB_re_uPa <- RL

#calculate source levels
s1$distance_m <- sqrt(s1$x_m^2 + s1$y_m^2 + s1$z_m^2)

s1$SL_dB_re_uPa <- s1$RL_dB_re_uPa + 20 * log10(s1$distance_m)

#################
#compare to Raven estimates

t<-sound1%>%
  filter(Begin.File == "AMAR173.4.20220813T020710Z.wav")%>%
  select(Selection, Source_Lev)

d<-s1%>%
  select(Selection, Edits, SL_dB_re_uPa)

# Find common selections
common_sel <- intersect(t$Selection, d$Selection)

# Prepare RavenPro dataset
raven_plot <- t %>%
  filter(Selection %in% common_sel) %>%
  rename(SL = Source_Lev) %>%
  mutate(dataset = "RavenPro")

# Prepare Waveform dataset
waveform_plot <- d %>%
  filter(Selection %in% common_sel) %>%
  rename(SL = SL_dB_re_uPa) %>%
  mutate(dataset = "Waveform")

# Combine
plot_df <- bind_rows(
  raven_plot %>% select(Selection, SL, dataset),
  waveform_plot %>% select(Selection, SL, dataset)
) %>%
  mutate(Selection = as.factor(Selection))

ggplot(plot_df, aes(x = Selection, y = SL, color = dataset)) +
  geom_point(size = 3) +                          # no dodge
  geom_line(aes(group = dataset)) +               # connect points by dataset
  theme_minimal() +
  labs(
    title = "Source Level Comparison (Matching Selections Only)",
    x = "Selection",
    y = "Source Level (dB re 1 µPa)",
    color = "Dataset"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_wide <- plot_df %>%
  pivot_wider(
    names_from = dataset,   # make a column for each dataset
    values_from = SL        # the SL values fill the columns
  )

plot_wide <- plot_wide %>%
  mutate(
    RavenPro = as.numeric(as.character(RavenPro)),
    Waveform = as.numeric(as.character(Waveform)),
    SL_diff = RavenPro - Waveform
  )
