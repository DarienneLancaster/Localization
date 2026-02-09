###Calculate received and source levels for fish sounds

sound<-read.csv("wdata/Sound_Species_Behaviour_Length_20250213.csv", header = TRUE)

sound$Energy..dB.FS.

# Sysgain value
sysgain <- -167.6

# Add new column Rec_Lev and subtract 20dB to account for factor 10 amplification
sound$Rec_Lev <- (sound$Energy..dB.FS.) + (-20) - (sysgain)

sound$Rec_Lev <- sound$Energy..dB.FS. - 20 - sysgain

sound$Rec_Lev


sound1<-sound%>%
  select(Site, Begin.File, Genus, Species, x_m, y_m, z_m, Energy..dB.FS., Rec_Lev)

# calculate distance
sound1$distance_m <- sqrt(sound1$x_m^2 + sound1$y_m^2 + sound1$z_m^2)

# calculate source level using spherical spreading
sound1$Source_Lev <- sound1$Rec_Lev + 20*log10(sound1$distance_m)

summary_by_species <- sound1 %>%
  group_by(Species) %>%
  summarise(
    mean_SL = mean(Source_Lev, na.rm = TRUE),
    sd_SL   = sd(Source_Lev, na.rm = TRUE),
    max_SL  = max(Source_Lev, na.rm = TRUE),
    min_SL  = min(Source_Lev, na.rm = TRUE)
  )


