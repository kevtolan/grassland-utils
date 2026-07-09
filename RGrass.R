# open grassGIS CLI
# enter 'open -na Rstudio' in console


library(tidyverse)
library(sf)
library(ggmap)
library(terra)
library(mapview)
library(leaflet)
library(tmap)
library(leafem)
library(sp)
library(ggspatial)
library(ks)
library(raster)
library(grid)
library(gridExtra)
library(classInt)
library(rnaturalearth)
library(rgrass)
library(cowplot)
# library(usethis)
# edit_r_environ()

# point_str <- '43.66086759147965, -73.3608887087686'

point_str <- '44.24459, -73.24040'
raster_paths <- c(
  north   = "/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Northern/Northern_2023_35cm_DSMFR.tif",
  central = "/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Central/Central_2023_35cm_DSMFR.tif",
  south   = "/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Southern/Southern_2023_35cm_DSMFR.tif"
)

coords <- as.numeric(strsplit(point_str, ", ")[[1]])
pt_wgs84 <- vect(matrix(c(coords[2], coords[1]), ncol=2), crs="+proj=longlat +datum=WGS84")

pt_metric <- project(pt_wgs84, "+proj=utm +zone=18 +datum=WGS84 +units=m")
bbox_metric <- buffer(pt_metric, width = 250)

matching_raster_path <- NULL
target_raster <- NULL

for (name in names(raster_paths)) {
  r <- rast(raster_paths[name])
  pt_raster_crs <- project(pt_wgs84, crs(r))

  pt_coords <- crds(pt_raster_crs)
  e <- ext(r)

  if (pt_coords[1] >= e$xmin && pt_coords[1] <= e$xmax &&
      pt_coords[2] >= e$ymin && pt_coords[2] <= e$ymax) {

    cat("Point overlaps with the", name, "raster.\n")
    matching_raster_path <- raster_paths[name]
    target_raster <- r
    break
  }}

if (!is.null(target_raster)) {
  bbox_raster_crs <- project(bbox_metric, crs(target_raster))

  raster_crop <- crop(target_raster, ext(bbox_raster_crs))

  print(raster_crop)
} else {
  stop("The point does not overlap with any of the provided rasters.")
}


if (!is.null(target_raster)) {
  bbox_raster_crs <- project(bbox_metric, crs(target_raster))

  raster_crop <- crop(target_raster, ext(bbox_raster_crs))

  print(raster_crop)
} else {
  stop("The point does not overlap with any of the provided rasters.")
}


r <- raster(raster_crop)
crs(r) <- CRS('+init=EPSG:32145')
r1 <- rast(r)
plot(r1)

td <- tempdir()
loc <- initGRASS(gisBase= '/Applications/GRASS-8.2.app/Contents/Resources',
                 home = td,
                 SG = r1,
                 override = TRUE)
write_RAST(r1, vname="rmask")

start <- Sys.time()

execGRASS("r.skyview", flags=c("o","overwrite"),
          input= 'rmask', output = 'rmask.Open')

end <- Sys.time()
elapsed <- difftime(end, start, units = "hours")

difftime(end, start, unit="hours")
message("Time elapsed: ", round(elapsed, 3), " hours")

u1 <- read_RAST('rmask.Open')
# writeRaster(u1, "~/R/Grasslab hab/LiDAR/ShelbFarms2023.tif", overwrite = T)

u2 <- u1
u1[u1[] >= 1 ] <- NA
u1[u1[] <= .8 ] <- NA

# writeRaster(u1, "~/R/Grasslab hab/LiDAR/Dutton_open.tif", overwrite = T)


ropen <- u1 %>%
  project('EPSG:4326')
# ropen <- rast("~/R/Grasslab hab/LiDAR/Dutton_open.tif") %>%
  # project('EPSG:4326')

names(ropen) <- c("opennness")


ropendf <-   as.data.frame(ropen, xy = TRUE) %>%
  na.omit()

bbox <- st_bbox(ropen)


bbox84 <- bbox %>% st_transform(4326)

lon_center <- (bbox84["xmin"] + bbox84["xmax"]) / 2
lat_center <- (bbox84["ymin"] + bbox84["ymax"]) / 2

sat_map <- get_map(location = c(lon_center, lat_center), zoom = 17, maptype = 'satellite')


# lon_center <- (bbox["xmin"] + bbox["xmax"]) / 2
# lat_center <- (bbox["ymin"] + bbox["ymax"]) / 2

ggmap(sat_map) +
  geom_raster(data = ropendf, aes(x = x, y = y, fill = opennness*0.9*100), alpha = 0.75) +
  scale_fill_gradientn(colors = c("#5e3c99","#4175b4","#4da49b","#a6dba0",
                                  "#e6f598","#ffffbf","#fee08b","#fdae61",
                                  "#f46d43","#d73027","#a50026"),
                       name = "    Mean\nopenness (º)",limits = c(70, 90), breaks = seq(70, 90, by = 5), oob = scales::squish, na.value = "#ebf2fa",
                       guide = guide_colourbar(barwidth = 2, barheight = 25, frame.colour = "black",title.vjust = 0.8)) +
  coord_sf(xlim = c(bbox84[1], bbox84[3]), ylim = c(bbox84[2], bbox84[4]), crs = 4326) +
  # coord_sf(xlim = c(bbox84[1]+0.003, bbox84[3]-0.005), ylim = c(bbox84[2]+0.001, bbox84[4]-0.001), crs = 4326) +
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
      text_size = 15,
      text_col = "white")) +
  theme(axis.title=element_text(size=20, face = "bold"),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 15))


