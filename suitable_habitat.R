library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(tmap)
library(leafem)
library(sp)
library(ks)
library(exactextractr)
library(raster)
library(ggspatial)
library(ggmap)
library(gganimate)
library(timetk)
library(magick)
library(sf)
library(cowplot)
library(arcpullr) # download ESRI-hosted data
library(ggspatial)
library(cowplot)
library(patchwork)

# download political boundaries
towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0",
                                   out_fields = c("TOWNNAMEMC")) %>% st_transform(crs = 32145) %>% st_make_valid()#%>% st_buffer(10) #hydrology polygons
counties <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0",
                           out_fields = c("CNTYNAME")) %>% st_transform(crs = 32145) %>% st_make_valid()#%>% st_buffer(10) #hydrology polygons

# download agr cover from 2016 and 2022, filter to exclude row crops
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2022.zip", destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)

VT_hay_pasture_22 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2022/LandLandcov_Agriculture2022.gdb"),
                        layer = "LandLandcov_Agriculture2022_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  dplyr::filter(Class != 'Crops') %>%
  mutate(area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105,
         perim_area = Shape_Length/Shape_Area) %>%
  filter(perim_area <= 0.03,
         area_ac >= 10) %>%
  st_simplify(dTolerance = 5, preserveTopology = T) %>%
  st_join(towns) %>%
  st_join(counties)



download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2016.zip", destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)

VT_hay_pasture_16 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2016/LandLandcov_Agriculture2016.gdb"),
                             layer = "LandLandcov_Agriculture2016_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  dplyr::filter(Class != 'Crops') %>%
  mutate(area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105,
         perim_area = Shape_Length/Shape_Area) %>%
  filter(perim_area <= 0.03,
         area_ac >= 10) %>%
  st_simplify(dTolerance = 1)
