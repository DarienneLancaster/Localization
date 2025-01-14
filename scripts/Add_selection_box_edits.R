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

O<-read.csv("wdata/SoundnVid_20241114.csv", header = TRUE)

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

levels(as.factor(OEfinal$Edit))

#######NOTE - too many localizations tied to fish IDs - might be something to do with
#when I attach original data to edited data, probably duplicating here (check tomorrow)

#join new selections to video data using Being Path (AMAR filename) and Selection_N (this is tied to the original selection that was replaced or had addition calls attached to it)
C<-left_join(OEfinal, O1, by = c("Begin.Path", "Selection_N"))
#remove unnecessary columns
N3<-N2%>%
  dplyr::select(-Begin.Clock.Time, -Edits)

#check if column names are matching 
#(FALSE just means there is different data in each column which is fine, 
#you don't want it to say Columns don't exist - 
#although these are fine and will be added in during bind_rows operation)
N3 %>%
  select(names(O)) %>%
  map2(O, ~ all(.x == .y))

O %>%
  select(names(N3)) %>%
  map2(N3, ~ all(.x == .y))

#create new dataframe joining new selections to original SoundnVid data (there should now be more selections than before)
ExtraSelections<-bind_rows(O, N3)

#recalculate sounds per fish with additional selections (this is added in as new column)
N4<-ExtraSelections%>%
  count(fishID,Selection)%>%
  count(fishID)%>%
  rename(soundsperfish_E=n)

ExtraSelections1<-left_join(ExtraSelections, N4, by="fishID")


###NOTE####
#once all files are fixed for bad selections remove all rows marked bs (they should be replaced by rows marked n from new selection tables)



