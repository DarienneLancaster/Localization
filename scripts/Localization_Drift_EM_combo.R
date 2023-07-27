#Script to create datasheet with all fish call localizations combined with 
#matching video files (buzzer drift adjusted) and eventually linked to
#EventMeasure species annotations

#Import annotated localization files as one dataframe
#NOTE: do we need to add extra info from Raven to localization tables at this point
#or can it be loaded back into Raven later and added? (check this)

#Filter to keep only localizations marked as fish call (FC)

#link localization times to correct video files (accounting for buzzer drift)

#create .png with localization coordinates (save to folder with loc number in file name)

#create column with .png file name for each FC localization

#Attach EventMeasure species annotations dataframe to main dataframe by linking
#by localization number