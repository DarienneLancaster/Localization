####Load Packages######
source("scripts/install_packages.R")#bring in install_packages script
lp("Rraven")#use load package script to check for and load packages
lp("tidyverse")
lp("lubridate")
lp("reaT")
lp("hms")
lp("readbulk")
lp("readxl")
lp("plotly")

####import all video file names and create list of file names in odata####
#Danger Rocks
FC1DR<- as.data.frame(list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam01_right_stereo_Danger_Rocks_Danvers/AllVideos"))
FC1DR<-FC1DR%>%rename(x='list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam01_right_stereo_Danger_Rocks_Danvers/AllVideos")')

FC2DR<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam02_left_stereo_Danger_Rocks_Danvers/AllVideos"))
FC2DR<-FC2DR%>%rename(x='list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam02_left_stereo_Danger_Rocks_Danvers/AllVideos")')

FC3DR<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam03_Birdseye_Danger_Rocks_Danvers/AllVideos"))
FC3DR<-FC3DR%>%rename(x='list.files("D:/Bamfield 2022 Large Array/DR20220908 Danger Rocks Danvers/Danger_Rocks_MP4s/FishCam03_Birdseye_Danger_Rocks_Danvers/AllVideos")')
#Ohiat
FC1O<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam01_right_side_stereo_cam_Ohiat_Island/AllVideos"))
FC1O<-FC1O%>%rename(x='list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam01_right_side_stereo_cam_Ohiat_Island/AllVideos")')

FC2O<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam02_left_side_stereo_cam_Ohiat_Island/AllVideos"))
FC2O<-FC2O%>%rename(x='list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam02_left_side_stereo_cam_Ohiat_Island/AllVideos")')

FC3O<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam03_birdseye_Ohiat_Island/AllVideos"))
FC3O<-FC3O%>%rename(x='list.files("D:/Bamfield 2022 Large Array/OH20220825 Ohiat Island/Ohiat_MP4s/FishCam03_birdseye_Ohiat_Island/AllVideos")')

#Taylor Islet
FC1T<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam01_left_side_stereoTaylor_Islet/AllVideos"))
FC1T<-FC1T%>%rename(x='list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam01_left_side_stereoTaylor_Islet/AllVideos")')

FC2T<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam02_left_side_stereo_Taylor_Islet/AllVideos"))
FC2T<-FC2T%>%rename(x='list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam02_left_side_stereo_Taylor_Islet/AllVideos")')

FC3T<-  as.data.frame(list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam03_birdseye_Taylor_Islet/AllVideos"))
FC3T<-FC3T%>%rename(x='list.files("D:/Bamfield 2022 Large Array/TI20220812 Taylor Islet/Taylor_Islet_MP4s/FishCam03_birdseye_Taylor_Islet/AllVideos")')

MP4filenames<-rbind(FC1DR,FC2DR, FC3DR,FC1O, FC2O, FC3O, FC1T, FC2T, FC3T)

write.csv(MP4filenames,"odata/MP4filenames.csv")
