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
#######Load in soundnvid csv#####
sv<-read.csv("wdata/SoundnVid.csv", header = TRUE)

# create a new column with species full name 
sv<- sv%>%
  mutate(FullName = paste(Family, Genus, Species, sep = " "))%>%
  mutate(Latin = paste(Genus, Species, sep = " "))%>%
  filter(ID_confidence < 3)

#rename codes to be more descriptive
sv$t<-as.factor(sv$t)
str(sv)
levels(sv$t)
sv$t <- recode_factor(sv$t, d = "Knock", 
                        e = "Unknown", g = "Grunt")

sv["Activity"][sv["Activity"] == ''] <- "Solo Fish/No Activity" #change all blank cells to check
sv$Activity<-as.factor(sv$Activity)
str(sv)
levels(sv$Activity)
sv$Activity <- recode_factor(sv$Activity, 
                               'Attracted' = "Following", 'Chase conspecific' = "Chasing", 'Chase other' = "Chasing",
                               'Feeding' = "Feeding", 'Guarding bait' = "Chasing", 'Passing' = "Other Fish Present")

#####calculate mean peak frequency by species for knocks#####
sv1<-sv%>%
  filter(t=="Knock", bs=="g", Peak.Freq..Hz.>=20, ID_confidence!="2", ID_confidence!="3",
         Latin!=" ", Latin!="Scorpaenichthys ")
sv1$Latin<-as.factor(sv1$Latin)
levels(sv1$Latin)

pfplot <- ggplot(sv1, aes(x=Latin, y=Peak.Freq..Hz., fill=Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # Changing the look of the line
  theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +# Changing the theme to get rid of the grey background
  ylab("Knock Peak Frequency (Hz)") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "italic", angle = 60, hjust=1, colour = "black"),
        legend.position="none")

pfplot
ggsave("figures/MeanKnockPeakFrequencybySpecies.png", width = 20, height = 20, units = "cm")

#####Mean peak frequency of grunts by species#####
sv2<-sv%>%
  filter(t=="Grunt", bs=="g", Peak.Freq..Hz.>=20, ID_confidence!="2", ID_confidence!="3",
         Latin!=" ", Latin!="Scorpaenichthys ")

sv2$Dur.90...s.
pfgplot <- ggplot(sv2, aes(x=Latin, y=Peak.Freq..Hz., fill= Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("Grunt Peak Frequency (Hz)") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "italic", angle = 60, hjust=1, colour = "black"), legend.position="none")

pfgplot
ggsave("figures/MeanGruntPeakFrequencybySpecies.png", width = 20, height = 20, units = "cm")

####Mean peak frequency of unknown calls by species####
sv3<-sv%>%
  filter(t=="Unknown", bs=="g", Peak.Freq..Hz.>=20, ID_confidence!="2", ID_confidence!="3",
         Latin!=" ", Latin!="Scorpaenichthys ")

sv2$Dur.90...s.
pfuplot <- ggplot(sv3, aes(x=Latin, y=Peak.Freq..Hz., fill= Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Grunt Peak Frequency") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1), legend.position="none")

pfuplot
ggsave("figures/MeanUnknownPeakFrequencybySpecies.png", width = 20, height = 20, units = "cm")

####remove extra sound variables to create svbasic####
svbasic<-sv%>%
  select(-c(View:selec.file))

str(svbasic)

#####calculate calling rate per fish#####
svbasic$tottime<-as_hms(svbasic$tottime)
str(svbasic)

#convert to lubridate hms periods
svbasic$totsec<-lubridate::hms(svbasic$tottime)
#use lubridate period to seconds to change to tottime to seconds
svbasic$totsec<-period_to_seconds(svbasic$totsec)
#calculate calls per minute (divide sounds per fish by total seconds in FOV and multiply by 60)
svbasic$callspermin<-(svbasic$soundsperfish/svbasic$totsec)*60


######calculate species richness by site#####
sr<-svbasic%>%
  count(Site,Latin,fishID)%>%
  count(Site, Latin)

#to count total individual fish identified so far
# sr<-svbasic%>%
#   count(Site,Latin,fishID)%>%
#   count(Site)

#calculate all localized calls tied to fish
# allcalls<-svbasic%>%
#   count(Site,fishID, Selection)%>%
#   count(Site)

#filter out unwanted unknown species

sr1<-sr%>%
  filter(Latin!=" ", Latin!="Scorpaenichthys ")

#plot species richness and # calling fish coloured by site
srplot<-ggplot(sr1, aes(x=Latin, y=n, fill= Site))+
  geom_bar(position="stack", stat="identity")+  
  scale_fill_manual("Site", values = c("Danger Rocks" = "cyan4", "Taylor Islet" = "goldenrod2"))+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +                                                      # Changing the theme to get rid of the grey background
  ylab("Calling Fish (count)") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 12, face = "italic", angle = 75, hjust=1, colour = "black")) 
print(srplot)
ggsave("figures/Abundance_vocalizing_fish_bySite.png", width = 20, height = 20, units = "cm")

#####Calculate calls per minute#####
#keep one row for each unique fish ID
fishunique<-svbasic%>%
  distinct(fishID, .keep_all = TRUE)

#remove NA rows
fishunique1<-fishunique%>%
  filter(!is.na(callspermin))%>%
  filter(ID_confidence < 3)%>% #keep only confidence above 3
  filter(Latin!=" ", Latin!="Scorpaenichthys ")


#calls per minute boxplot
callplot <- ggplot(fishunique1, aes(x=Latin, y=callspermin, fill=Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # make boxes variable width based on sample size
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +# Changing the theme to get rid of the grey background
  ylab("calls per minute") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1, colour = "black"),legend.position="none")

callplot
ggsave("figures/CallsPerMinute.png", width = 20, height = 20, units = "cm")

####Mean time in FOV####
FOVplot <- ggplot(fishunique1, aes(x=Latin, y=tottime, fill=Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # make boxes variable width based on sample size
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +# Changing the theme to get rid of the grey background
  ylab("Minutes on Camera") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1, colour = "black"),legend.position="none")
FOVplot
ggsave("figures/MinutesOnCamera.png", width = 20, height = 20, units = "cm")

####Number of grunts/knocks/other by species####

ct<-svbasic%>%
  filter(Latin!=" ", Latin!="Scorpaenichthys ")%>%
  filter(!is.na(t))%>%
  count(Latin,t)

# Rearrange factor levels in the order you want in the legend
ct$t <- factor(ct$t, levels = c("Knock", "Grunt", "Unknown"))

calltypeplot<-ggplot(ct, aes(x=Latin, y=n, fill= t))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual("Call Type", values = c("Knock" = "cyan4", "Unknown" = "goldenrod2", "Grunt" = "royalblue4"))+ # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  labs(fill = "Call Type")+
  theme(axis.text.x = element_text(size = 14, face = "plain", angle = 60, hjust=1, colour = "black"))
print(calltypeplot)
ggsave("figures/CallTypes.png", width = 20, height = 20, units = "cm")

#####behaviour during calling####
beha<-svbasic%>%
  filter(Latin!=" ", Latin!="Scorpaenichthys ")%>%
  filter(!is.na(t))%>%
  count(Latin,Activity)

#activity by species
activityplot<-ggplot(beha, aes(x=Activity, y=n, fill= Activity))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  scale_fill_manual("Behaviour", values = c("Following" = "cyan4", "Chasing" = "goldenrod2", "Feeding" = "royalblue4", "Other Fish Present" = "tomato1", "Solo Fish/No Activity" = "mediumturquoise"))+
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  labs(fill = "Activity")+ 
  facet_wrap(~Latin)+
  theme(axis.title.x=element_blank(),axis.text.x = element_blank(), strip.text.x = element_text(
    size = 9), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(activityplot)
ggsave("figures/CallTypes.png", width = 20, height = 20, units = "cm")

#####activity grouped by call type#####  
ctbh<-svbasic%>%
  filter(Latin!=" ", Latin!="Scorpaenichthys ")%>%
  filter(!is.na(t))%>%
  count(Latin,t,Activity)

ctactiveplot<-ggplot(ctbh, aes(x=t, y=n, fill= Activity))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  scale_fill_manual("Behaviour", values = c("Following" = "cyan4", "Chasing" = "goldenrod2", "Feeding" = "royalblue4", "Other Fish Present" = "tomato1", "Solo Fish/No Activity" = "mediumturquoise"))+
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  labs(fill = "Activity")+ 
  facet_wrap(~Latin)+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 14, face = "plain", angle = 75, hjust=1, colour = "black"), strip.text.x = element_text(
    size = 10), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(ctactiveplot)
ggsave("figures/CallTypesbyActivity.png", width = 20, height = 20, units = "cm")

#####call variables plot by species and call type#####
#TO DO - filter out bad selection, filter out unknown sounds from fish sounds, plot only one sound type at a time
sv$Center.Freq..Hz.
sv$ID_confidence<-as.factor(sv$ID_confidence)
callsplot<-ggplot(sv, aes(x=`Center.Freq..Hz.`, y=`Peak.Freq..Hz.`))+
  geom_point(aes(colour = factor(Latin), shape=ID_confidence))+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Peak Frequency") +  
  xlab("Frequency 25%")+              # Changing the text of the y axis label
  labs(fill = "Call Type")+
  facet_wrap(~t)+
  theme(axis.text.x = element_text(size = 14, face = "plain", angle = 75, hjust=1))
print(callsplot)
ggsave("figures/callsvariablesbyspeciesandcalltype.png", width = 20, height = 20, units = "cm")

SoundnVid$`Inband Power (dB FS)`
str(SoundnVid)

freq<-ggplot(SoundnVid, aes(x=`Inband Power (dB FS)` , y=`Peak Freq (Hz)`))+
  geom_point(aes(colour = factor(Species), shape=t))

print(freq)

#####mean number of calls per idividual fish by species####

numuniquecalls<-svbasic%>%
  filter(Latin!=" ", Latin!="Scorpaenichthys ")%>%
  filter(!is.na(t))%>%
  count(Latin,fishID)

numuniquecallsplot <- ggplot(numuniquecalls, aes(x=Latin, y=n)) + 
  geom_boxplot(varwidth = TRUE)+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Mean calls per individual") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1), 
        panel.background = element_rect(fill = 'paleturquoise3', color = 'purple'))

numuniquecallsplot <- ggplot(numuniquecalls, aes(x=Latin, y=n, fill=Latin)) + 
  geom_boxplot(varwidth = TRUE)+           # make boxes variable width based on sample size
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +# Changing the theme to get rid of the grey background
  ylab("Calls per individual") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1, colour = "black"),legend.position="none")

numuniquecallsplot
ggsave("figures/MeanCallsPerUniqueFish.png", width = 20, height = 20, units = "cm")





#Analyses to Do
# 1. mean number of calls per individual by species
#species richness
#     how much video has been annotated so far (daylight hours only)
# 2. total unique fish identified
# 3. grunts vs knocks total and by species
# 4. mean time in frame by species (box plot)
# 5. Mean number of calls by species
# 5. behaviours by calls by species
#6. plot call frequency by other variable for each species by call type

#sample summary code using tapply
# #mean sounds per fish while in FOV
# meansoundsperfish<-tapply(fishunique$soundsperfish, fishunique$Latin, mean)
# meansoundsperfish
# 
# #mean time in FOV
# 
# #remove NA rows
# fishunique1<-fishunique%>%
#   filter(!is.na(tottime))
# fishunique1$tottime<-as_hms(fishunique1$tottime)
# meantottime<-tapply(fishunique1$tottime, fishunique1$Latin, mean)
# meantottime
# 
# 
# meancallrate<-tapply(fishunique1$callrate, fishunique1$Latin, mean)
# meancallrate
# sdcallrate<-tapply(fishunique1$callrate, fishunique1$Latin, sd)
# sdcallrate
# 
# #create summary dataframe with mean calls per fish and mean time in FOV
# svsummary<-as.data.frame(cbind(meansoundsperfish, meantottime, meancallrate,sdcallrate))
# svsummary$meantottime<-as_hms(svsummary$meantottime)
# svsummary$meancallrate<-as_hms(svsummary$meancallrate)
# svsummary$sdcallrate<-as_hms(svsummary$sdcallrate)

