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

sv<-read.csv("wdata/SoundnVid.csv", header = TRUE)

# create a new column with species full name 
sv<- sv%>%
  mutate(FullName = paste(Family, Genus, Species, sep = " "))%>%
  mutate(Latin = paste(Genus, Species, sep = " "))%>%
  filter(ID_confidence < 3)

#remove extra sound variables
svbasic<-sv%>%
  select(-c(View:selec.file))

str(svbasic)

#calculate calling rate per fish
svbasic$tottime<-as_hms(svbasic$tottime)
str(svbasic)

#convert to lubridate hms periods
svbasic$totsec<-lubridate::hms(svbasic$tottime)
#use lubridate period to seconds to change to tottime to seconds
svbasic$totsec<-period_to_seconds(svbasic$totsec)
#calculate calls per minute (divide sounds per fish by total seconds in FOV and multiply by 60)
svbasic$callspermin<-(svbasic$soundsperfish/svbasic$totsec)*60


#calculate species richness by site
sr<-svbasic%>%
  count(Site,Latin,fishID)%>%
  count(Site, Latin)

#plot species richness and # calling fish coloured by site
srplot<-ggplot(sr, aes(x=Latin, y=n, fill= Site))+
  geom_bar(position="stack", stat="identity")+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Count (vocalizing fish)") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 14, face = "plain", angle = 60, hjust=1))
print(srplot)
ggsave("figures/Abundance_vocalizing_fish_bySite.png", width = 20, height = 20, units = "cm")

#keep one row for each unique fish ID

fishunique<-svbasic%>%
  distinct(fishID, .keep_all = TRUE)

#remove NA rows
fishunique1<-fishunique%>%
  filter(!is.na(callspermin))%>%
  filter(ID_confidence < 3) #keep only confidence above 3

#calls per minute boxplot
callplot <- ggplot(fishunique1, aes(x=Latin, y=callspermin)) + 
  geom_boxplot(varwidth = TRUE)+           # make boxes variable width based on sample size
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("calls per minute") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1))

callplot
ggsave("figures/CallsPerMinute.png", width = 20, height = 20, units = "cm")

#Mean time in FOV
FOVplot <- ggplot(fishunique1, aes(x=Latin, y=tottime)) + 
  geom_boxplot(varwidth = TRUE)+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Minutes on Camera") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1))

FOVplot
ggsave("figures/MinutesOnCamera.png", width = 20, height = 20, units = "cm")

#Number of grunts/knocks/other by species

ct<-svbasic%>%
  filter(!is.na(t))%>%
  count(Latin,t)

ct$t<-as.factor(ct$t)
str(ct)
ct$t <- recode_factor(ct$t, d = "Knock", 
                                e = "Unknown", g = "Grunt")

calltypeplot<-ggplot(ct, aes(x=Latin, y=n, fill= t))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  labs(fill = "Call Type")+
  theme(axis.text.x = element_text(size = 14, face = "plain", angle = 60, hjust=1))
print(calltypeplot)
ggsave("figures/CallTypes.png", width = 20, height = 20, units = "cm")

#behaviour during calling
beha<-svbasic%>%
  filter(!is.na(t))%>%
  count(Latin,Activity)

#rename activity labels
beha["Activity"][beha["Activity"] == ''] <- "Solo Fish/No Activity" #change all blank cells to check
beha$Activity<-as.factor(beha$Activity)
str(beha)
levels(beha$Activity)
beha$Activity <- recode_factor(beha$Activity, 
                      'Attracted' = "Following", 'Chase conspecific' = "Chasing", 'Chase other' = "Chasing",
                      'Feeding' = "Feeding", 'Guarding bait' = "Chasing", 'Passing' = "Other Fish Present")


#activity by species
activityplot<-ggplot(beha, aes(x=Activity, y=n, fill= Activity))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  labs(fill = "Activity")+ 
  facet_wrap(~Latin)+
  theme(axis.title.x=element_blank(),axis.text.x = element_blank())
print(activityplot)
ggsave("figures/CallTypes.png", width = 20, height = 20, units = "cm")

#activity grouped by call type
ctbh<-svbasic%>%
  filter(!is.na(t))%>%
  count(Latin,t,Activity)

ctbh$t<-as.factor(ctbh$t)
str(ctbh)
ctbh$t <- recode_factor(ctbh$t, d = "Knock", 
                      e = "Unknown", g = "Grunt")

ctbh["Activity"][ctbh["Activity"] == ''] <- "Solo Fish/No Activity" #change all blank cells to check
ctbh$Activity<-as.factor(ctbh$Activity)
str(ctbh)
levels(ctbh$Activity)
ctbh$Activity <- recode_factor(ctbh$Activity, 
                               'Attracted' = "Following", 'Chase conspecific' = "Chasing", 'Chase other' = "Chasing",
                               'Feeding' = "Feeding", 'Guarding bait' = "Chasing", 'Passing' = "Other Fish Present")


ctactiveplot<-ggplot(ctbh, aes(x=t, y=n, fill=Activity))+
  geom_bar(position="dodge", stat="identity")+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("Number of Calls") +                                                   # Changing the text of the y axis label
  labs(fill = "Call Type")+
  facet_wrap(~Latin)+
  theme(axis.text.x = element_text(size = 14, face = "plain", angle = 75, hjust=1))
print(ctactiveplot)
ggsave("figures/CallTypesbyActivity.png", width = 20, height = 20, units = "cm")

#call variables plot by species and call type
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


#Analyses to Do
# 1. species richness
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

