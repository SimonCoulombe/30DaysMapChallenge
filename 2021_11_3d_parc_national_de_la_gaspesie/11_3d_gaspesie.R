#install.packages("remotes")
#remotes::install_github("tylermorganwall/rayshader")
#remotes::install_github("tylermorganwall/rayimage")

library(rayshader)
library(raster)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(mapview)
library(purrr)
library(stars)
library(terra)
# telecharger trace gps acropole des draveurs
#https://en-ca.gps-viewer.com/tracks/gyj/Acropole-des-draveurs/
acropole <- read_sf(here("2021_11_3d_parc_national_de_la_gaspesie/data/Acropole des draveurs.gpx"), layer = "track_points")  %>%
  select(time, rownum = track_seg_point_id, ele) %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]) %>%
  mutate(track ="acropole") 

# trouver le point le plus loin ppour couper le retour.

aller_acropole <- acropole[1:which.max(acropole$lat),]

trail <-aller_acropole %>% 
  group_by(track) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_set_crs(4326)
  
mapview(trail)

add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}


## canada high resolution DEM
# https://open.canada.ca/data/en/dataset/957782bf-847c-4644-a757-e383c0057995

if(FALSE){

## first get the dataset footprints
# https://open.canada.ca/data/en/dataset/957782bf-847c-4644-a757-e383c0057995/resource/1f54365a-cdc2-42a2-b22c-b1aae563a5f9

footprints <- read_sf(here("2021_11_3d_parc_national_de_la_gaspesie/data/Datasets_Footprints/Datasets_Footprints.shp")) %>%
  st_transform(4326)





mapview(footprints %>% st_crop(st_bbox(trail %>% add_buffer_in_meters_and_return_to_original_crs(., 3000))))
 

# on veut Tile_name 	1m_utm19_w_11_130  et 	1m_utm19_w_10_130 
footprints_charlevoix <- footprints %>% filter(Project == "600015-35", Tile_name %in% c("1m_utm19_w_11_130", "1m_utm19_w_10_130"))


# download tiles 
map(footprints_charlevoix$Ftp_dtm,~ download.file(.x, destfile = here("2021_11_3d_parc_national_de_la_gaspesie/data/", basename(.x)) ))
# 
# stars1 <- read_stars(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_10_130.tif")) %>%
#   st_as_stars() 
# 
# stars2 <- read_stars(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_11_130.tif")) %>%
#   st_as_stars() 
# 
# 
# stars1b <-  stars1 %>% mutate(elev = dtm_1m_utm19_w_10_130.tif) %>% select(-dtm_1m_utm19_w_10_130.tif)
# stars2b <-  stars2 %>% mutate(elev = dtm_1m_utm19_w_11_130.tif) %>% select(-dtm_1m_utm19_w_11_130.tif)
# # merge stars spatially
# # https://tmieno2.github.io/R-as-GIS-for-Economists/
# # https://tmieno2.github.io/R-as-GIS-for-Economists/merging-stars-objects-using-c-and-st-mosaic.html#merging-stars-objects-of-different-spatial-extents
# #
# both_stars <- st_mosaic(stars1b, stars2b)
#   
# cropped_stars <- both_stars %>% st_crop(trail %>% add_buffer_in_meters_and_return_to_original_crs(., 500) %>% st_transform(st_crs(both_stars)) )
# 
# raster1 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_5_61.tif"))
# raster2 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_5_62.tif"))


}

raster1 <- terra::rast(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_10_130.tif")) 
raster2 <- terra::rast(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_11_130.tif")) 


both_rasters <- terra::merge(raster1, raster2)
both_rasters_crop <-  terra::crop(both_rasters, trail %>% add_buffer_in_meters_and_return_to_original_crs(., 1500) %>% st_transform(st_crs(both_rasters)))

rm(both_rasters, raster1, raster2)
gc()
bryce <- as(both_rasters_crop, "Raster")

bryce_mat = raster_to_matrix(bryce)

bryce_small <- resize_matrix(bryce_mat, 0.25)


bryce_zoom_mat <- bryce_small

base_map = bryce_zoom_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(bryce_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_zoom_mat), 0) %>%
  add_shadow(ambient_shade(bryce_zoom_mat),0) %>% 
  add_shadow(texture_shade(bryce_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)


plot_map(base_map)


projected_range_in_crs_lat_long <-
  st_bbox(bryce) %>%
  st_as_sfc() %>%
  st_transform(4326)  %>%
  st_bbox()


extent_zoomed = extent(st_bbox(bryce)[1], st_bbox(bryce)[3], st_bbox(bryce)[2], st_bbox(bryce)[4])

bryce_highway <- opq(projected_range_in_crs_lat_long) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 


bryce_lines <- bryce_highway$osm_lines %>% st_transform(st_crs(bryce))

bryce_trails = bryce_lines %>% 
  filter(highway %in% c("path","footway", "track"))


bryce_roads = bryce_lines %>% 
  filter(highway %in% c("unclassified", "service"))


trails_layer = generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="black", lty=3, offset = c(2,-2),
                                    heightmap = bryce_zoom_mat)%>%
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 11, color="white",
                                    heightmap = bryce_zoom_mat)) %>%  
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 8, color="grey30",
                                    heightmap = bryce_zoom_mat)) 

bryce_water_lines = opq(projected_range_in_crs_lat_long) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 

bryce_streams = st_transform(bryce_water_lines$osm_lines,crs=st_crs(bryce)) 

stream_layer = generate_line_overlay(bryce_streams,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = bryce_zoom_mat)

base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()



base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_3d(bryce_zoom_mat, windowsize=c(1200,800))


## ça manque de lac..

osm_lakes.sf <- 
  opq(bbox = projected_range_in_crs_lat_long) %>%
  add_osm_feature(key = 'water', value = 'lake') %>%
  osmdata_sf()
osm_lakes.sf <- osm_lakes.sf$osm_polygons %>% st_transform(st_crs(bryce))

polygon_layer = generate_polygon_overlay(osm_lakes.sf, extent = extent_zoomed,
                                         heightmap = bryce_zoom_mat, palette="skyblue2") 


final_map <- base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) 
plot_map(final_map)
render_snapshot("2021_11_3d_parc_national_de_la_gaspesie/map2d.png", clear = TRUE)

base_map %>% 
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_3d(bryce_zoom_mat, windowsize=c(1200,800))



render_camera(theta=-45,  phi=45, zoom=0.5,  fov=75)

render_snapshot("2021_11_3d_parc_national_de_la_gaspesie/map3d.png", clear = TRUE)

