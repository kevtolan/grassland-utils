# open grassGIS CLI
# enter 'open -na Rstudio' in console


library(tidyverse)
library(sf)
library(terra)
library(ggmap)
library(mapview)
library(leaflet)
library(ggspatial)
library(tmap)
library(leafem)
library(sp)
library(ks)
library(raster)
library(grid)
library(gridExtra)
library(classInt)
library(rnaturalearth)
library(rgrass)
library(cowplot)

ropen <- rast('/Users/kevintolan/R/Grasslab hab/LiDAR/UpperMeadows_open.tif') %>%
  project('EPSG:4326')

names(ropen) <- c("opennness")

ropendf <-   as.data.frame(ropen, xy = TRUE) %>% na.omit()

bbox <- st_bbox(ropen)

bbox84 <- bbox %>% st_transform(4326)

lon_center <- (bbox84["xmin"] + bbox84["xmax"]) / 2
lat_center <- (bbox84["ymin"] + bbox84["ymax"]) / 2

sat_map <- get_map(location = c(lon_center, lat_center), zoom = 15, maptype = 'satellite')

ggmap(sat_map) +
  geom_raster(data = ropendf, aes(x = x, y = y, fill = opennness*0.9*100), alpha = 0.75) +
  scale_fill_gradientn(
    colors = c("#5e3c99","#4175b4","#4da49b","#a6dba0","#e6f598","#ffffbf","#fee08b","#fdae61","#f46d43","#d73027","#a50026"),
    name = "    Mean\nopenness (º)", limits = c(70, 90), breaks = seq(70, 90, by = 5), oob = scales::squish, na.value = "#ebf2fa",
    guide = guide_colourbar(barwidth = 1, barheight = 10, frame.colour = "black", title.vjust = 0.8)) +
  coord_sf(xlim = c(bbox84[1]+0.001, bbox84[3]-0.001), ylim = c(bbox84[2]+0.001, bbox84[4]-0.001), crs = 4326) +
  labs(x = "Longitude", y = 'Latitude') +
  annotation_scale(aes(text_col = 'white', line_col = 'white',
                       width_hint = 0.4), height = unit(0.5, "cm"), text_cex = 1) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "white",
      text_size = 10,
      text_col = "white"))

