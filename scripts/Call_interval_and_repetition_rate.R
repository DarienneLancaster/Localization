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
lp("vegan")
lp("gridExtra")
lp("stringr")
lp("flextable")

fishdata<-read.csv("wdata/Sound_Species_Behaviour_Length_wPyFeatures_20250221.csv", header = TRUE)

#create new column with species common names
fishdata$Common<- ifelse(fishdata$Species == "caurinus", "Copper rockfish",
                         ifelse(fishdata$Species == "maliger", "Quillback rockfish",
                                ifelse(fishdata$Species == "pinniger", "Canary rockfish",
                                       ifelse(fishdata$Species == "miniatus", "Vermillion rockfish",
                                              ifelse(fishdata$Species == "melanops", "Black rockfish",
                                                     ifelse(fishdata$Species == "elongatus", "Lingcod",
                                                            ifelse(fishdata$Species == "decagrammus", "Kelp Greenling",
                                                                   ifelse(fishdata$Species == "vacca", "Pile Perch", "other"))))))))


###############################
#calculate call sequences (link together calls from the same fish <1 second apart as a call sequence)
str(fishdata)
fishdata$Begin.File
fishdata1<-fishdata
str(fishdata1)

#select rows with duplicate selection numbers and keep only the first occurence
fishdata88<- fishdata1 %>%
  group_by(Begin.File) %>%
  filter(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))%>%
  group_by(Selection)%>%
  slice(1)%>%
  ungroup()

#create data frame with these duplicate selections removed
fishdata77<-fishdata1%>%
  group_by(Begin.File) %>%
  filter(!(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))) 

#paste selections back into main dataframe with duplicate Selection rows removed
fishdata66<-rbind(fishdata88, fishdata77)

#arrange in ascending order
fishdata2<-fishdata66%>%
  arrange(Begin.File, Begin.Time..s.)

#double check no duplicate selections (should have zero observations)
fishdata00<- fishdata2 %>%
  group_by(Begin.File) %>%
  filter(duplicated(Selection) | duplicated(Selection, fromLast = TRUE))

#deal with the same fish crossing over multiple AMAR files
fishdata33 <- fishdata2 %>%
  group_by(fishID)%>%
  mutate(
    End = ifelse(
      Begin.File != lead(Begin.File),  # Check if Begin.File is different in next row
      End.Time..s. - 1800,  # If true, subtract 1800 from End (1800 is the number of seconds in 30 minute files)
      End.Time..s.)  # Otherwise, just copy the End value
  )

fishdata44<-fishdata33%>%
  mutate(End = ifelse(is.na(End), End.Time..s., End))# If End is NA, copy the value from End.Time..s.

#create unique identifier for call sequences (calls can be a maximum of 5 seconds apart to count towards the same call sequence)
fishdata99 <- fishdata44 %>%
  # select(Selection, Begin.Time..s., End.Time..s., fishID, Species, Begin.File, End, Selection_N, Edit)%>%
  group_by(fishID) %>%
  mutate(
    Sequence_ID = cumsum(
      Begin.Time..s. > lag(End, default = first(End)) + 5 | is.na(lag(End))
    ) + 5  # Incremental sequence for each group
  ) %>%
  ungroup()

#calculate time between each call within a call sequence (call Interval)
fishdata98 <- fishdata99 %>%
  group_by(fishID, Sequence_ID) %>%
  mutate(
    Call_Interval = ifelse(
      is.na(lag(End)),  # If lag() returns NA (i.e., first row in group)
      NA,                           # Set Call_Interval to NA
      Begin.Time..s. - lag(End)  # Otherwise, subtract Begin.Time..s. from previous row
    )
  ) %>%
  ungroup()

# #code to check if there are any overlapping selection boxes (should have 0 rows if all errors fixed)
# fishdata111<-fishdata98%>%
#   filter(Call_Interval < 0)

#calculate how many calls are repeated within a sequence
fishdata998 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  tally(name = "count")

#count number of grunts and drums in each 
fishdata001 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(g_count = sum(t == "g", na.rm = TRUE), d_count = sum(t=="d", na.rm = TRUE), e_count = sum(t=="e", na.rm = TRUE))

#join number of fish call data to main data
fishdata999<-left_join(fishdata98, fishdata998, by = c("fishID", "Sequence_ID"))%>%
  mutate(Sequence_Reps = count) %>%
  select(-count) # Remove the intermediate 'count' column, if not needed

#join number of grunts and drums to main data
fishdata002<-left_join(fishdata999, fishdata001, by = c("fishID", "Sequence_ID"))

#calculate mean and sd of call interval for each call sequence
fishdata003 <- fishdata98 %>%
  group_by(fishID, Sequence_ID) %>%
  summarize(C_Interval_mean = mean(Call_Interval, na.rm = TRUE), C_Interval_sd = sd(Call_Interval, na.rm = TRUE))

#join number of grunts and drums to main data
fishdata004<-left_join(fishdata002, fishdata003, by = c("fishID", "Sequence_ID"))

#keep only the first row
fishdata000<-fishdata004%>%
  #filter(Species == "caurinus")%>%
  group_by(fishID, Sequence_ID)%>%
  slice(1)

#histogram of calling interval (time between linked calls) by species 
ggplot(fishdata000, aes(x = C_Interval_mean)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Interval by Species", x = "Call Interval", y = "Count") +
  coord_cartesian(ylim = c(0, 20)) +  # Fix y-axis range from 0 to 8
  theme_classic()

#histogram of calling repetition (within a sequence) by species 
ggplot(fishdata000, aes(x = Sequence_Reps)) +
  geom_histogram(binwidth = 1, color = "black", fill = "darkturquoise") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Histograms of Call Repetition by Species", x = "Call Repetition Rate", y = "Count") +
  coord_cartesian(ylim = c(0, 10)) +  # Fix y-axis range from 0 to 8
  theme_classic()

#need to add back in the rest of the data (only a few column included right now)
#play with different time increments between calls for linking call sequences
#Examine grunts vs. knocks dynamics and behaviour dynamics (are calls repeated more when they're associated with fear/aggression?)

#change missing cells in t to e (for unknown sound)
fishdata<-fishdata%>%
  mutate(t = ifelse(is.na(t) & s == "e", "e", t))%>%
  mutate(t = ifelse(t == "" & s == "e", "e", t))


fishdataT<-fishdata%>%
  filter(Species == "maliger", mean_length >200)


# 
# %>%
# filter(fishID == "0005_20220912")

