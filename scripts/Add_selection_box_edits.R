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

################################################################################
####remove bad localization selections from original data and replace with new
####manually annotated good selections.  Also add additional selections for call  
####trains that were not successfully localized

###load original localization data

O<-read.csv("wdata/SoundnVid_20250114.csv", header = TRUE)

###################################################################
#create list of AMAR files that will actually need to be edited
# O8<-O%>%
#   mutate(selec.file = gsub(".txt$", "_FS.txt", selec.file))
# 
# #create list of AMAR files that actually made it into the final dataset
# unique_values <- as.data.frame(unique(O8$selec.file))
# 
# write.csv(unique_values,"wdata/SelectionTablestoEdit1.csv", row.names = FALSE)
###############################################################
#dataframe with only required video data to paste to new selection table info
O1<-O%>%
  # filter(Selection == 3621)%>%
  #filter(Begin.Path =="AMAR173.4.20220911T020952Z.wav")
  mutate(Selection_N = Selection)%>% 
  dplyr::select(Begin.Path, Site:Selection_N)%>%
  mutate(Selection_N = as.character(Selection_N))%>%
  dplyr::select(-Selection)

###load new selection tables with

N<-imp_raven(path = paste0("odata/FS_selection_tables_edited"), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.

#fix error in Edits column name
#create new dataframe with only tables with column named Edtis
N22<-N%>%
  dplyr::select(-106)%>% #remove duplicate column for selec.file
  filter(!is.na(Edtis))%>%
  dplyr::select(-Edits)%>%
  rename(Edits = Edtis)

#create new dataframe with bad Edtis column removed
N33<-N%>%
  dplyr::select(-106)%>% #remove duplicate column for selec.file
  filter(is.na(Edtis))%>%
  dplyr::select(-Edtis)

#join fixed up dataframe N22 to N33
N44<-rbind(N22, N33)

N1<-N44%>%
  separate(Edits, into = c("Edit","Selection_N"), sep = "_", remove = FALSE)%>% #separate Edits column into two columns
  filter(Edit !="r")%>% #remove any column marked with an r for Remove (these are bad selections)
  # filter(Edit =="a"| Edit == "n")%>% #keep only rows marked a (additional selection), or n (new selection - replaces bad selection boxes)
  rename_all(~ gsub(" ", ".", gsub("\\(", ".", gsub("\\)", ".", gsub("/", ".", gsub("%", ".", gsub("-", ".", .))))))) #make column names match my subbing . in for all special symbols and spaces

#check for errors in Edit column (fixed these)
# levels(as.factor(N1$Edit))
# 
# Nt <- N1 %>%
#   filter(Edit %in% c(144, 3006, 3676))

###Move Selection numbers for original localizations into new column Selection_N (only for rows where Selection_N is blank)
N1$Selection<-as.character(N1$Selection)

N11 <- N1 %>%
  mutate(Selection_N = if_else(is.na(Selection_N), Selection, Selection_N))

#Create dataframe with only additional selections (a)
N_a<-N11%>%
  filter(Edit == "a")

#remove old localization data that has been replaced by new annotations (marked with n in Edits column)
N12 <- N11 %>%
  filter(Edit != "a")%>% #remove additional selections
  group_by(Begin.Path, Selection_N) %>% # group by Begin.Path and Selection_N
  filter(!(Edit == "" & n() > 1)) %>% #remove row were Begin.Path and Selection_N have same value and where Edit column is blank (n() >1 checks for duplicate rows with same Selection Number)
  filter(s != "")%>% #remove unlabled localizations (e.g. not noise, FS or unknown sounds)
  ungroup()

#rejoin additional annotations to N12 dataframe
N13<-rbind(N12, N_a)
levels(as.factor(N13$Edit))

N14<-N13%>%
  mutate(Edit = if_else(Edit == "", "e", Edit))%>%
  dplyr::select(-Edits)

##Join edited Raven Tables to Original Tables that didn't require any edits

#Load original Raven Tables and add Edit column
R<-imp_raven(path = paste0("odata/All_Localizations_Daylight_LA_filtered_2_1_FS"), all.data =  TRUE, only.spectro.view = FALSE) #need to set only.spectro.view to false to see columns from waveform.

R1<-R%>%
  dplyr::select(-106)%>% #remove duplicate column for selec.file
  filter(s != "")%>% #get rid of unidentified localization sounds
  dplyr::select(-a)%>% #get rid of extra column a
  rename(Edit = Edits)%>% #rename column
  mutate(Edit = if_else(Edit =="", "o", Edit))%>% 
  mutate(Edit = if_else(is.na(Edit),"o", Edit))%>% #change all values of Edit to be NA
  mutate(Selection_N = Selection)%>%
  rename_all(~ gsub(" ", ".", gsub("\\(", ".", gsub("\\)", ".", gsub("/", ".", gsub("%", ".", gsub("-", ".", .))))))) #make column names match my subbing . in for all special symbols and spaces
  
#Join Edited and original together

O_E<-rbind(N14, R1)

O_Ea<-O_E%>%
  filter(Edit == "a")

#Remove Files original that were edited by removing duplicate Begin.Path and Selection Rows (remove row where those match but Edit is blank)
O_E1 <- O_E %>%
  filter(Edit != "a")%>%
  mutate(Edit)%>%
  group_by(Begin.Path, Selection_N) %>% # group by Begin.Path and Selection_N
  filter(!(Edit == "o" & n() > 1))%>% #remove row were Begin.Path and Selection_N have same value and where Edit column equals o (original) (n() >1 checks for duplicate rows with same Selection Number)
  filter(s != "")%>% #remove unlabled localizations (e.g. not noise, FS or unknown sounds)
  ungroup()

#add additional annotations back in to data

OEfinal<-rbind(O_E1, O_Ea)

OEfinal1<-OEfinal%>%
  filter(grepl("AMAR173.4.20220813T023710Z", Begin.Path))

levels(as.factor(OEfinal$Edit))

#######NOTE - too many localizations tied to fish IDs - might be something to do with
#when I attach original data to edited data, probably duplicating here (check tomorrow)

#join new selections to video data using Being Path (AMAR filename) and Selection_N (this is tied to the original selection that was replaced or had addition calls attached to it)
C<-left_join(OEfinal, O1, by = c("Begin.Path", "Selection_N"))

C1<-C%>%
  filter(!is.na(fishID)) #remove any sound localization rows where no fish was able to be identified

#recalculate sounds per fish with additional selections (this is added in as new column)
C2<-C1%>%
  count(fishID,Selection)%>%
  count(fishID)%>%
  rename(soundsperfish_E=n)

C3<-left_join(C1, C2, by="fishID")

write.csv(C3,"wdata/EditedFishSoundswVideoID_20250116.csv", row.names = FALSE)


############################################################################
#add fish length data to edited sound/video data

##########match fish measurements to existing soundnvid data##########

fish_length<-read.csv("odata/fish_measurements_20241125.csv", header = TRUE, skip = 4 )

fish_lengthIJ<-read.csv("odata/TI_ImageJ_screenshots/TI_fishmeasurements_nonstereo.csv", header = TRUE)

#############################################################################
#check relationship between EventMeasure and ImageJ fish length measurements

#add zeros to start of Localization ID
fish_lengthIJ$fishID<-with_options(
  c(scipen = 999), 
  str_pad(fish_lengthIJ$fishID, 4, pad = "0")
)

#create fishID column and average 2 length measurements
fish_lengthIJD<-fish_lengthIJ%>%
  filter(grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  unite(fishID, fishID, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_lengthIJ = mean(Length, na.rm = TRUE),
            sd_lengthIJ = sd(Length, na.rm = TRUE)) %>%
  ungroup()

#add zeros to start of Localization ID
fish_length$Selection<-with_options(
  c(scipen = 999), 
  str_pad(fish_length$Selection, 4, pad = "0")
)

###do the same for EventMeasure data###
fish_lengthEMD<-fish_length%>%
  dplyr::select(-c(3:5,7:28, 33))%>% #get rid of unnecessary columns
  filter(grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  separate(Filename, into = c("vidnum","CamName", "Date"), sep = "_", remove = FALSE)%>%
  separate(Date, into = c("Date"), sep = "T")%>%
  rename(Length =Length..mm.)%>%
  unite(fishID, Selection, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_lengthEM = mean(Length, na.rm = TRUE),
            sd_lengthEM = sd(Length, na.rm = TRUE)) %>%
  ungroup()

#join EM and IJ duplicate measure dataframes
dup_meas<-left_join(fish_lengthEMD, fish_lengthIJD, by = "fishID")

#subract EM lengths from IJ lengths and calculate mean and SD difference
dup_meas$difference<- dup_meas$mean_lengthEM-dup_meas$mean_lengthIJ

mean(dup_meas$difference)
sd(dup_meas$difference)
#EventMeasure lengths are an average of 17mm longer than ImageJ measurements with a standard deviation of 48mm. 
#Strong siginificant positive correlation between measurements (rho =0.7, p=0.02)

#Pearsons correlation between measurements
measure_cor<-cor.test(dup_meas$mean_lengthEM, dup_meas$mean_lengthIJ,  method="pearson") #exact = FALSE gets rid of impact of tied values on ranking
print(measure_cor)
#rho = 0.52, p = 0.102 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

# Create ggplot plots of rugosity metrics
measurements <- ggplot(dup_meas, aes(x = mean_lengthEM, y = mean_lengthIJ)) +
  geom_point(color = "deepskyblue") +
  labs(title = "a)") +
  sm_statCorr(corr_method="pearsons", color = "deepskyblue3", linetype = "dashed")+
  labs(x = "EM", y = "IJ")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
measurements 

#############################################################################################
###create new dataframe with fish measurements joined to sound data

#create fishID column and average 2 length measurements
fish_lengthIJ1<-fish_lengthIJ%>%
  filter(!grepl("D", Notes))%>%  #remove any rows with D in notes column (D for Duplicate measurement)
  unite(fishID, fishID, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE),
            sd_length = sd(Length, na.rm = TRUE)) %>%
  ungroup()

duplicated(fish_lengthIJ1$fishID)

###do the same for EventMeasure data###
fish_length1<-fish_length%>%
  dplyr::select(-c(3:5,7:28, 33:34))%>% #get rid of unnecessary columns
  separate(Filename, into = c("vidnum","CamName", "Date"), sep = "_", remove = FALSE)%>%
  separate(Date, into = c("Date"), sep = "T")%>%
  rename(Length =Length..mm.)

#combine fishnum (Selection) and Date
fish_length1<-fish_length1%>%
  unite(fishID, Selection, Date, sep = "_", remove = FALSE)%>%
  group_by(fishID) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE),
            sd_length = sd(Length, na.rm = TRUE)) %>%
  ungroup()

duplicated(fish_length1$fishID)

#join EM and ImageJ measurements
measures<- rbind(fish_length1, fish_lengthIJ1)
duplicated(measures$fishID)

#link measures to edited sound dataframe (C3 from above)
EditSoundVidMeas<-left_join(C3, measures, by = "fishID")

write.csv(EditSoundVidMeas, "wdata/Sound_Species_Behaviour_Length_20250116.csv", row.names = FALSE)

### look at fish length histogram by species

FishLength<-EditSoundVidMeas%>%
  group_by(fishID) %>%
  slice_head(n = 1) %>%
  filter(!is.na(mean_length))%>% ## get rid of NAs in length (these fish haven't been annotated yet)
  ungroup()


#histogram of fish lengths by species (bins of 5)
ggplot(FishLength, aes(x = mean_length)) +
  geom_histogram(binwidth = 5, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Mean Length by Species", x = "Mean Length", y = "Count") +
  coord_cartesian(ylim = c(0, 8)) +  # Fix y-axis range from 0 to 8
  theme_classic()

