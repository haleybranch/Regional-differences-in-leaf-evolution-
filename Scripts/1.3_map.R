#Import libraries
library(tidyverse)
library(cowplot)
library(ggmap)
library(ggspatial)

#Import California & Oregon Map
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#Import Site Data
site.lat.long <- read.csv("Data/sites.csv", header=T)
site.lat.long <- site.lat.long %>% filter(Site==c(1,12,2,9,10,11))

#Map Making
base_map <- ggplot(cali_or) + 
  geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) +
  geom_point(data=site.lat.long, aes(x=Longitude, y=Latitude, fill=Region),size=6, shape=21,color="black")+
  scale_fill_manual(values= c("North"="skyblue3", "South"="#FFA100"))+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  coord_sf(crs = 4326)+
  theme_nothing()
base_map
