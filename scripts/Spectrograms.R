#Create spectrograms of fish sounds
source("scripts/install_packages.R")#bring in install_packages script
lp("tuneR")
lp("seewave")
lp("viridis")

# Read the .wav file (change the file path accordingly)
audio_file <- readWave("odata/Sound Clips and Images/Single_Knocks/Copper_Knock_1489_20220823T0237.wav")

#create spectrogram using seewave
spectro(wave = audio_file,
        noisereduction = 1,
        f= 32000,
        wl = 2000, # window length (determines frequency resolution)
        ovlp = 90, # percentage window overlap
        flim = c(0,1),
        grid = FALSE,
        osc = TRUE, #creates mini waveform underneath spectrogram
        colbg = "white", #changes background colour
        collevels = seq(-40,-5,1), #adjusts brightness of spectrogram
        palette = reverse.gray.colors.2)

oscillo(wave = audio_file,
        k=1,
        from = 0, to = 0.8,
        colwave="black",
        cexlab = 1)

#####################################################################
#Grunts

#black rockfish grunt

# Read the .wav file (change the file path accordingly)
Black_G <- readWave("odata/Sound Clips and Images/Single_Grunts/black_rockfish_grunt_TI20220820T0237.wav")

#create spectrogram plot
spectro(wave = Black_G,
        noisereduction = 1,
        f= 32000,
        wl = 1000, # window length (determines frequency resolution)
        ovlp = 90, # percentage window overlap
        flim = c(0,1),
        grid = FALSE,
        osc = TRUE, #creates mini waveform underneath spectrogram
        colbg = "white", #changes background colour
        collevels = seq(-30,0,1), #adjusts brightness of spectrogram
        palette = spectro.colors)

#where you want the figure saved and the name of the plot
output_file <- file.path("figures/Spectrograms/Black_Rockfish_Grunt_Spectro.png")

#specify plot dimensions
png(output_file,
    width = 12,
    height = 8,
    units = "in",
    res = 1000)

#create spectrogram plot
spectro(wave = Black_G,
        noisereduction = 1,
        f= 32000,
        wl = 1000, # window length (determines frequency resolution)
        ovlp = 90, # percentage window overlap
        flim = c(0,1),
        grid = FALSE,
        osc = TRUE, #creates mini waveform underneath spectrogram
        colbg = "white", #changes background colour
        collevels = seq(-30,0,1), #adjusts brightness of spectrogram
        palette = reverse.gray.colors.2)

#saves plot to specified folder above
dev.off()

#quillback rockfish grunt

# Read the .wav file (change the file path accordingly)
Quillback_G <- readWave("odata/Sound Clips and Images/Single_Grunts/quillback_rockfish_grunt_TI20220815T0237.wav")

#create spectrogram plot
spectro(wave = Quillback_G,
        noisereduction = 1,
        f= 32000,
        wl = 3000, # window length (determines frequency resolution)
        ovlp = 90, # percentage window overlap
        flim = c(0,1),
        grid = FALSE,
        osc = TRUE, #creates mini waveform underneath spectrogram
        colbg = "white", #changes background colour
        collevels = seq(-40,0,1), #adjusts brightness of spectrogram
        palette = spectro.colors)

#where you want the figure saved and the name of the plot
output_file <- file.path("figures/Spectrograms/Quillback_Rockfish_Grunt_Spectro.png")

#specify plot dimensions
png(output_file,
    width = 12,
    height = 8,
    units = "in",
    res = 1000)

#create spectrogram plot
spectro(wave = Quillback_G,
        noisereduction = 1,
        f= 32000,
        wl = 4000, # window length (determines frequency resolution)
        ovlp = 90, # percentage window overlap
        flim = c(0,1),
        grid = FALSE,
        osc = TRUE, #creates mini waveform underneath spectrogram
        colbg = "white", #changes background colour
        collevels = seq(-50,0,1), #adjusts brightness of spectrogram
        palette = reverse.gray.colors.2)

#saves plot to specified folder above
dev.off()
