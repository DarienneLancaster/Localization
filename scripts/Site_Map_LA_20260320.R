#Species and Genus richness by site MAPS
library("dplyr")

#load BRUV and eDNA data - GENUS

LA <- read.csv("odata/Large Array 2022 Bamfield Deployment Coordinates decimal degrees.csv")



#filter to retain only site coordinates and site name
LA <- LA %>%
  select(Site, Latitude, Longitude)%>%
  mutate(Site = ifelse(Site == "Danger Rock", "Danger Rocks", Site))

library(sf)
library(rnaturalearth)
library(devtools)

##########################

#load in .shp file bc.coast (this needs to be saved in your data folder)

LA_sf <- st_as_sf(
  LA,
  coords = c("Longitude", "Latitude"),   # <-- change if needed
  crs = 4326                  # WGS84
)

bc.coast <- read_sf("odata/CSEE2024-R-maps-main/data")

st_crs(bc.coast)


coords <- st_coordinates(LA_sf)

LA_sf$X <- coords[, 1]
LA_sf$Y <- coords[, 2]


############################

b <- ggplot() +
  ggtitle("") +
  
  geom_sf(data = bc.coast,
          fill = "#003366",
          color = "black",
          alpha = 0.3) +
  
  geom_sf(
    data = LA_sf,
    aes(fill = "Large Array Sites"),   # mapped for legend
    shape  = 21,
    colour = "black",
    size = 4,
    stroke = 0.6
  ) +
  
  geom_sf_text(
    data = LA_sf,
    aes(label = Site),   # labels from Site column
    size = 4,
    #fontface = "bold",
    nudge_y = 0.007      # adjust as needed
  ) +
  
  scale_fill_manual(
    values = c("Large Array Sites" = "#ffCC00")
  ) +

  geom_text(
    aes(x = -125.13, y = 48.83),
    label = "Bamfield",
    size = 5,
    fontface = "bold",
    color = "black"
  )+
  # geom_text(
  #   aes(x = -123.6, y = 48.63),
  #   label = "Vancouver \nIsland",
#   size = 6,
#   fontface = "italic",
#   color = "black"
# )+
  
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "none") +
  
  coord_sf(xlim = c(-125.25, -125.03),
           ylim = c(48.98, 48.8))

b

###

##########################

ggsave(plot = b, "figures/CH3/LA_Site_Map_allsites.PNG", width = 12, height = 12, units = "cm")

##Create inset map
# Inset map showing all of Vancouver Island

# Define main map extent as an sf polygon
rect_sf <- st_as_sfc(st_bbox(c(xmin = -125.25, xmax = -125.03,
                               ymin = 48.98,  ymax = 48.8),
                             crs = st_crs(bc.coast)))

##
inset <- ggplot() +
  geom_sf(data = bc.coast, fill = "#003366", color = "black", alpha = 0.3) +
  geom_sf(data = LA_sf, shape = 21, fill = "#ffCC00", color = "black", size = 1) +
  geom_sf(data = rect_sf, fill = NA, color = "#ffCC00", linewidth = 1) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "black", size = 1.5)  # black border
  ) +
  coord_sf(xlim = c(-128.3, -122.5), ylim = c(48.2, 51))   # full Vancouver Island

inset
##


library(cowplot)
main_map <- b   # your main map

# Place inset in bottom-right corner
final_mapLA <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset, x = 0.555, y = 0.655, width = 0.35, height = 0.35)  # adjust position and size

final_mapLA
#the placement of the inset map look off in R but looks good when saves as .png

ggsave(
  plot = final_mapLA,
  filename = "figures/CH3/LA_MAP_withInset.png",
  width = 20, height = 20, units = "cm",
  dpi = 300,
  limitsize = FALSE
)

#######################################################################################################

#SPECIES RICHNESS MAP

#load BRUV and eDNA data - GENUS

SR <- read.csv("data/Combined_data/SpeciesRichness_BRUV_eDNA.csv")

#load site coordinates
coord <- read.csv("data/BRUV_data/data_BRUV_SaanichInlet_Summer2021_deploymentlog.csv")

#filter to retain only site coordinates and site name
coord1 <- coord %>%
  separate(simple_sitename,
           into = c("prefix", "code", "simple.site", "date"),
           sep = "_",
           remove = FALSE) %>%
  select(-prefix, -code, -date)%>% # keep only the new site column if desired
  filter(!simple_sitename %in% c("SEAC_1049_BrentwoodBay_20210722",
                                 "SEAC_1049_BrentwoodBay_20210723"))%>%
  select(simple.site, Lat, Lon)


#rename sites to match site codes

# Add a site code into the BRUV df so that it matches with eDNA df site codes
coord1 <- mutate(coord1, site = ifelse(simple.site == "ColesBay", "CB",
                                       ifelse(simple.site == "RusselIslandEast", "RIE",
                                              ifelse(simple.site == "PrevostNorthCentral", "PN",
                                                     ifelse(simple.site == "NavyChannelWest", "NC",
                                                            ifelse(simple.site == "PrevostSouthCentral", "PSC",
                                                                   ifelse(simple.site == "MillBayWhiskeyPoint", "MBW",
                                                                          ifelse(simple.site == "FulfordWest", "FW",
                                                                                 ifelse(simple.site == "MillBayNorth", "MBN",
                                                                                        ifelse(simple.site == "BrentwoodBay", "HC", simple.site))))))))))
coord1<-coord1%>%
  filter(!is.na(simple.site))%>%
  dplyr::rename(Site = site)

##################################
#load BRUV and eDNA data - Species

SR <- read.csv("data/Combined_data/SpeciesRichness_BRUV_eDNA.csv")

SR_coor<-left_join(SR, coord1, by = "Site")

#attach coordinates to Species Richness data
SR_coor1<-SR_coor%>%
  filter(!is.na(Lat))

write.csv(SR_coor1, file = "data/Combined_data/SpeciesRichness_Coordinates.csv", row.names = FALSE)

##########################################
# #create Species map
#######################
# install.packages("ggmap")
# library(ggmap)
# install.packages("rgdal")
# library(rgdal)
# install.packages("tmap")
# library(tmap)
# 
# 
# #Code from Wes for mapping
# install.packages(c("sf", "stars", "tidyverse",
#                    "basemaps", "rphylopic", "gganimate",
#                    "terra", "tidyterra", "palmerpenguins",
#                    "rnaturalearth", "rnaturalearthhires",
#                    "rnaturalearthdata", "leaflet", "ggnewscale",
#                    "patchwork", "mapedit","devtools"))

library(sf)
library(rnaturalearth)
library(devtools)

##########################

#load in .shp file bc.coast (this needs to be saved in your data folder)

SR_sf <- st_as_sf(
  SR_coor1,
  coords = c("Lon", "Lat"),   # <-- change if needed
  crs = 4326                  # WGS84
)

bc.coast <- read_sf("data/CSEE2024-R-maps-main/data")
st_crs(bc.coast)

coords <- st_coordinates(SR_sf)

SR_sf$X <- coords[, 1]
SR_sf$Y <- coords[, 2]

SR_labels <- SR_sf %>%
  filter(method == "BRUV")

s <- ggplot() +
  ggtitle("") +
  geom_sf(data = bc.coast, fill = "#006633", color = "black", alpha = 0.7) +
    geom_sf(
    data = rcas,
    aes(linetype = "RCA"),
    fill = "#006666",
    color = "#006666",
    linewidth = 0.1,
    alpha = 0.2
  )+
  geom_sf(
    data = SR_sf,
    aes(
      fill  = method,   
      alpha = method,   #transparency by method
      size  = species_richness
    ),
    shape  = 21,
    colour = "black",   # ⬅️ black outline
    stroke = 0.6 #outline width
  ) +

  geom_text_repel(
    data = SR_labels,
    aes(x = X, y = Y, label = Site),
    size = 3,
    fontface = "bold",
    box.padding = 0.3,
    point.padding = 0.4,
    min.segment.length = 0,
    max.overlaps = Inf
  )+
  scale_fill_manual(
    values = c("BRUV" = "orange", "eDNA" = "purple4"),
    name = "Method"
  ) +
  scale_linetype_manual(
    name = "",
    values = c("RCA" = "dashed")
  )+
  geom_text(
    aes(x = -123.455, y = 48.79),
    label = "Saltspring Island",
    size = 5,
    fontface = "italic",
    color = "black"
  )+
  geom_text(
    aes(x = -123.6, y = 48.63),
    label = "Vancouver \nIsland",
    size = 6,
    fontface = "italic",
    color = "black"
  )+
  
  scale_alpha_manual(
    values = c("BRUV" = 1, "eDNA" = 0.5),
    guide = "none"      # hide alpha legend
  ) +
  
  scale_size_continuous(
    name = "Species richness",
    range = c(2, 10), breaks = c(5, 10, 15, 20)   # makes large differences more noticeable 
  ) +
  # Make Method points bigger in legend
  guides(
    fill = guide_legend(
      override.aes = list(size = 6)   # ⬅️ increase point size in legend
    )
  ) +
  
  theme_classic() +
  theme(
    axis.title = element_blank()
  )+
  coord_sf(xlim = c(-123.65, -123.25), ylim = c(48.85, 48.58))
s

##Create inset map
# Inset map showing all of Vancouver Island

# Define main map extent as an sf polygon
rect_sf <- st_as_sfc(st_bbox(c(xmin = -123.65, xmax = -123.25,
                               ymin = 48.58,  ymax = 48.85),
                             crs = st_crs(bc.coast)))

##
inset <- ggplot() +
  geom_sf(data = bc.coast, fill = "#006633", color = "black", alpha = 0.7) +
  geom_sf(data = SR_sf, shape = 21, fill = "orange", color = "black", size = 1) +
  geom_sf(data = rect_sf, fill = NA, color = "orange", linewidth = 1) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "black", size = 1.5)  # black border
  ) +
  coord_sf(xlim = c(-128.3, -122.5), ylim = c(48.2, 51))   # full Vancouver Island

inset
##


library(cowplot)
main_map <- s   # your main map

# Place inset in bottom-right corner
final_mapSR <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset, x = 0.56, y = 0.09, width = 0.25, height = 0.25)  # adjust position and size

final_mapSR
#the placement of the inset map look off in R but looks good when saves as .png

ggsave(
  plot = final_mapSR,
  filename = "figures/MAP_Species_Richness_bySite.png",
  width = 20, height = 20, units = "cm",
  dpi = 300,
  limitsize = FALSE
)

ggsave(plot = s, "figures/MAP_Species_Richness_bySite_noinset.PNG", width = 12, height = 12, units = "cm")

###Paste Genus and Species Richness plots together

img_species <- readPNG("figures/MAP_Species_Richness_bySite.png")
img_genus   <- readPNG("figures/MAP_Genus_Richness_bySite.png")

grob_species <- rasterGrob(img_species, interpolate = TRUE)
grob_genus   <- rasterGrob(img_genus, interpolate = TRUE)

grob_species_labeled <- grobTree(
  grob_species,
  textGrob("a)", 
           x = 0.02, y = 0.98,
           just = c("left", "top"),
           gp = gpar(fontsize = 16, fontface = "bold"))
)

grob_genus_labeled <- grobTree(
  grob_genus,
  textGrob("b)", 
           x = 0.02, y = 0.98,
           just = c("left", "top"),
           gp = gpar(fontsize = 16, fontface = "bold"))
)


png("figures/combined_species_genus_Maps.png",
    width = 3000, height = 1500, res = 300)

grid.arrange(grob_species_labeled,
             grob_genus_labeled,
             ncol = 2)

dev.off()

##########################
#Full site map with all BRUV and eDNA sites

AllSites <- read.csv("data/BRUV_data/data_BRUV_eDNA_SaanichInlet_Summer2021_20260220.csv")

AllSites<-AllSites%>%
  mutate(eDNA = case_when(
    eDNA == "Yes" ~ "BRUV and eDNA",
    eDNA == "" | is.na(eDNA) ~ "BRUV",
    TRUE ~ eDNA
  ))%>%
  filter(!Annotate %in% c("Test site", "Site redone"))%>%
  mutate(Lon = -abs(Lon)) #make all coordinates negative for X values
  

#load in .shp file bc.coast (this needs to be saved in your data folder)

AllSites_sf <- st_as_sf(
  AllSites,
  coords = c("Lon", "Lat"),   # <-- change if needed
  crs = 4326                  # WGS84
)

bc.coast <- read_sf("data/CSEE2024-R-maps-main/data")
st_crs(bc.coast)

coords <- st_coordinates(AllSites_sf)

AllSites_sf$X <- coords[, 1]
AllSites_sf$Y <- coords[, 2]

Site_labels <- as.data.frame(AllSites_sf$Site.Name)

A <- ggplot() +
  ggtitle("") +
  geom_sf(data = bc.coast, fill = "#006633", color = "black", alpha = 0.7) +
  geom_sf(
    data = rcas,
    aes(linetype = "RCA"),
    fill = "grey",
    color = "darkgray",
    linewidth = 0.7,
    alpha = 0.4
  )+
  # geom_sf(
  #   data = AllSites_sf,
  #   aes(fill = eDNA, shape = eDNA),
  #   alpha = 0.6,
  #   size = 2,
  #   colour = "black",
  #   stroke = 1
  # ) +
  
  scale_fill_manual(
    name = "Method",
    values = c(
      "BRUV" = "orange",
      "BRUV and eDNA" = "purple4"
    )
  ) +
  
  scale_shape_manual(
    name = "Method",
    values = c(
      "BRUV" = 21,
      "BRUV and eDNA" = 24
    )
  ) +
  
  # single guide that combines fill and shape
  guides(
    fill = guide_legend(
      title = "Method",
      override.aes = list(
        shape = c(21, 24),    # match your shapes
        fill  = c("orange", "purple4"),
        size  = 3,
        colour = "black"
      )
    )
  )+
  
  # geom_text_repel(
  #   data = AllSites_sf,
  #   aes(x = X, y = Y, label = Site.Name),
  #   size = 3,
  #   fontface = "bold",
  #   box.padding = 0.3,
  #   point.padding = 0.4,
  #   min.segment.length = 0,
  #   max.overlaps = Inf
  # )+
  scale_linetype_manual(
    name = "",
    values = c("RCA" = "dashed")
  )+
  geom_text(
    aes(x = -123.455, y = 48.79),
    label = "Saltspring Island",
    size = 4,
    fontface = "italic",
    color = "black"
  )+
  geom_text(
    aes(x = -123.6, y = 48.63),
    label = "Vancouver \nIsland",
    size = 6,
    fontface = "italic",
    color = "black"
  )+
  
  scale_alpha_manual(
    values = c("BRUV" = 1, "eDNA" = 0.5),
    guide = "none"      # hide alpha legend
  ) +
  
  scale_size_continuous(
    name = "Species richness",
    range = c(2, 10), breaks = c(5, 10, 15, 20)   # makes large differences more noticeable 
  ) +
  # Make Method points bigger in legend
  guides(
    fill = guide_legend(
      override.aes = list(size = 3)   # ⬅️ increase point size in legend
    )
  ) +
  geom_jitter(
    data = AllSites_sf %>% mutate(st_coords = st_coordinates(.)),
    aes(x = st_coords[,1], y = st_coords[,2], fill = eDNA, shape = eDNA),
    width = 0.002,
    height = 0.002,
    size = 2,
    colour = "black",
    stroke = 1,
    alpha = 0.8
  )+
  
  theme_classic() +
  theme(
    axis.title = element_blank()
  )+
  coord_sf(xlim = c(-123.65, -123.15), ylim = c(48.85, 48.58))
A

##Create inset map
# Inset map showing all of Vancouver Island

# Define main map extent as an sf polygon
rect_sf <- st_as_sfc(st_bbox(c(xmin = -123.65, xmax = -123.25,
                               ymin = 48.58,  ymax = 48.85),
                             crs = st_crs(bc.coast)))

##
inset <- ggplot() +
  geom_sf(data = bc.coast, fill = "#006633", color = "black", alpha = 0.7) +
  geom_sf(data = AllSites_sf, shape = 21, fill = "orange", color = "black", size = 1) +
  geom_sf(data = rect_sf, fill = NA, color = "orange", linewidth = 1) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "black", size = 1.5)  # black border
  ) +
  coord_sf(xlim = c(-128.3, -122.5), ylim = c(48.2, 51))   # full Vancouver Island

inset
##


library(cowplot)
main_map <- A    # your main map

# Place inset in bottom-right corner
final_mapALL <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset, x = 0.542, y = 0.172, width = 0.25, height = 0.25)  # adjust position and size

final_mapALL
#the placement of the inset map look off in R but looks good when saves as .png

ggsave(
  plot = final_mapALL,
  filename = "figures/MAP_AllSites.png",
  width = 20, height = 20, units = "cm",
  dpi = 300,
  limitsize = FALSE
)


