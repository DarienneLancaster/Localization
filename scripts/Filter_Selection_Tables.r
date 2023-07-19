# some example code usring Rraven to bring in and export selection tables

install.packages("Rraven")
library(Rraven)
library(dplyr)
install.packages("base")
library(base)
install.packages("rgl")
library(rgl)

#working code to import localization files, filter localizations within FOV of
#cameras then export as raven table.  Currently only running one file at a time,
#need to write loop

#importing data
yourobject<-imp_raven(path="C:/Users/dlanc/Documents/PhD/RavenPro/Localization_Results_Bamfield_2022/Taylor_Islet_LA_2022",# the path to the FOLDER where your selection table is
                       files="AMAR173.4.20220823T000710Z.wav.chan0.Table.1.selections.txt",# a vector of files to import or the name of a single file
                       all.data = TRUE)#if FALSE only a portion of the columns are imported

#filter data
testfilter<- yourobject %>%
  # select(-Class, -Sound.type, -Software)%>% # can use this format to remove unnecessary columns if you want
  filter(x_err_span_m <2, y_err_span_m <2, z_err_span_m <2, 
         x_m <=1,x_m >=-1, y_m <=1,y_m >=-1,  z_m <=1,z_m >=-3)

#write data
write.table(testfilter,# the object you want to export as a selection table 
            file = "C:/Users/dlanc/Documents/PhD/RavenPro/Localization_Results_Bamfield_2022/Filtered_Selection_Tables/AMAR173.4.20220823T000710Z.wav.chan0.Table.1.selections.filtered.txt",# the path and file name using the file extension .txt
            sep = "\t", #how to delineate data
            row.names = FALSE, #row names will mess things up
            quote = FALSE)#putting things in quotes will mess things up.


#Code to plot coordinates, need to come up with a way to plot coordinates for 
#localization one by one and save image with localization name.
plot3d(x=testfilter$x_m, y=testfilter$y_m, z=testfilter$z_m)


#trying to write loop for file processing

extension <- "txt"
fileNames <- Sys.glob(paste("*.", extension, sep = ""))
fileNumbers <- seq(fileNames)
for (fileNumber in fileNumbers) {
  newFileName <-  paste("*filtered.", 
                        sub(paste("\\.", extension, sep = ""), "", fileNames[fileNumber]), 
                        ".", extension, sep = "")
  # read old data:
  sample <- read.csv(fileNames[fileNumber],
                     header = TRUE,
                     sep = ",")
  # add one to every widget value in every file:
  sample$Widgets <- sample$Widgets + 1
  
  # write old data to new files:
  write.table(sample, 
              newFileName,
              append = FALSE,
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)
}
fileNames <- Sys.glob("*.txt")

for (fileName in fileNames) {
  # read data:
  
  # add more stuff here
}

data_files<- list.files("C:/Users/dlanc/Documents/PhD/RavenPro/Localization_Results_Bamfield_2022/Taylor_Islet_LA_2022")
data_files

for(i in 1:length(data_files)) {                              # Head of for-loop
  assign(paste0("data", i),                                   # Read and store data frames
         read.table(paste0("C:/Users/dlanc/Documents/PhD/RavenPro/Localization_Results_Bamfield_2022/Taylor_Islet_LA_2022",
                           data_files[i])))
}


