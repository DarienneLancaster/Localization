#script with function to import localization tables, filter localizations to 
#within the camera FOV, and export into filtered data folder in wdata

source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")

#change folder name here for other study sites
loc<-"Taylor_Islet_LA_2022"

#start of import/filter/export function
filt<-function(loc, 
               err_span=2,#specifies error span filter limit
               box_width=1 #specifies distance either side of array center filter limit (e.g. 1m any direction. If you want higher limit for vertical(y) axis you can adjust within function (e.g. box_width*3 = 3m))
               ){

data_files<- list.files(paste0("odata/", loc))# create object with list of all files in loc folder
data_files
out_dir<-paste0("wdata/",loc,"_filtered_",err_span,"_",box_width,"/") # creates out directory in working data(wdata) folder with
dir.create(out_dir, showWarnings = FALSE) #create new folder for filtered data that specifies filter parameters (e.g. Taylor_Islet_LA_2022_filtered_2_1)

for (i in 1:length(data_files)) {
  
#importing data
yourobject<-imp_raven(path=paste0("odata/", loc),# the path to the FOLDER where your selection table is
                       files=data_files[i],# a vector of files to import or the name of a single file
                       all.data = TRUE)#if FALSE only a portion of the columns are imported

#filter data
locfilter<- yourobject %>%
  # select(-Class, -Sound.type, -Software)%>% # can use this format to remove unnecessary columns if you want
  filter(x_err_span_m <err_span, y_err_span_m <err_span, z_err_span_m <err_span, 
         x_m <=box_width,x_m >=-box_width, y_m <=box_width,y_m >=-box_width,  z_m <=box_width,z_m >=-box_width*3)

#write data
write.table(locfilter,# the object you want to export as a selection table 
            file = paste0(out_dir, data_files[i]),# the path and file name using the file extension .txt
            sep = "\t", #how to delineate data
            row.names = FALSE, #row names will mess things up
            quote = FALSE)#putting things in quotes will mess things up.

}}

filt(loc)






