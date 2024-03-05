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
  mutate(Latin = paste(Genus, Species, sep = " "))

#remove extra sound variables
svbasic<-sv%>%
  select(-c(View:selec.file))

#calculate calling rate per fish
svbasic$tottime<-as_hms(svbasic$tottime)
str(svbasic)
svbasic$callrate<-svbasic$tottime/svbasic$soundsperfish
svbasic$callrate<-as_hms(svbasic$callrate)

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

#mean sounds per fish while in FOV
meansoundsperfish<-tapply(fishunique$soundsperfish, fishunique$Latin, mean)
meansoundsperfish

#mean time in FOV

#remove NA rows
fishunique1<-fishunique%>%
  filter(!is.na(tottime))
fishunique1$tottime<-as_hms(fishunique1$tottime)
meantottime<-tapply(fishunique1$tottime, fishunique1$Latin, mean)
meantottime


meancallrate<-tapply(fishunique1$callrate, fishunique1$Latin, mean)
meancallrate
sdcallrate<-tapply(fishunique1$callrate, fishunique1$Latin, sd)
sdcallrate

#create summary dataframe with mean calls per fish and mean time in FOV
svsummary<-as.data.frame(cbind(meansoundsperfish, meantottime, meancallrate,sdcallrate))
svsummary$meantottime<-as_hms(svsummary$meantottime)
svsummary$meancallrate<-as_hms(svsummary$meancallrate)
svsummary$sdcallrate<-as_hms(svsummary$sdcallrate)

svsummary$callspermin<-svsummary$meancallrate/00:01:00.00000

callplot <- ggplot(svbasic, aes(x=Latin, y=callrate)) + 
  geom_boxplot()+           # Changing the look of the line
  theme_bw() +                                                      # Changing the theme to get rid of the grey background
  ylab("minutes per call") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 60, hjust=1))

callplot

ggsave("figures/Abundance_vocalizing_fish_bySite.png", width = 20, height = 20, units = "cm")


#Analyses to Do
# 1. species richness
# 2. total unique fish identified
# 3. grunts vs knocks total and by species
# 4. mean time in frame by species (box plot)
# 5. Mean number of calls by species
# 5. behaviours by calls by species