library(sf)
library(tidyverse)
library(osmdata)
library(mapview)
library(elevatr)
library(raster)
library(rnaturalearth)
library(cmocean) # for color palette
library(scico) # for scientific colour maps by fabio crameri https://github.com/thomasp85/scico
library(crsuggest)
devtools::install_github("eliocamp/ggnewscale")
library(ggnewscale)
library(tidygeocoder)
library(colorspace) #  for lightern()
library(polylabelr) # to get pole of inacessibility (point furthest from any edge, to give us an idea of the distance between water lines)
# show_col is used to display palettes
#show_col(scico::scico(11, palette = "oslo"))
scico_palette_show()
# First, let's get an area we want to plot.

# Option 1 : start from address, get a point, then add a buffer around it.  
# LE massif de Charlevoix's address is "1350 Rue Principale, Petite-Rivière-Saint-François" 


massif_sf <- tidygeocoder::geocode( tibble(address = "Notre-Dame-des-Monts, Québec"), 
                                    address = address)  %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) 

add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}

massif_shape <- massif_sf %>% 
  add_buffer_in_meters_and_return_to_original_crs(., 20000)

massif_bbox = sf::st_bbox(massif_shape)

# Option 2 : get a shape for a city from open street map
quebec_shape <- osmdata::getbb("Quebec city, Canada", format_out = "sf_polygon")  
quebec_shape <-  quebec_shape[1,] # garder le polygone de la ville de québec


quebec_bbox <- sf::st_bbox(quebec_shape)


## Pick one option to keep moving 
data_sf <- massif_shape


## get the rasters
dem.raster <- elevatr::get_elev_raster(data_sf, z= 9, clip = "bbox")

# get rivers + lakes from rnatural earth (nah.. too little details)
#rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
#lakes10 <- ne_download(scale = 10, type = 'lakes', category = 'physical')


# get rivers + lakes + roads from osmdata
osm_lakes.sf <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'water', value = 'lake') %>%
  osmdata_sf()
osm_lakes.sf <- osm_lakes.sf$osm_multipolygons

osm_rivers.sf <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()
osm_rivers.sf <- osm_rivers.sf$osm_lines

osm_rivers.sf2 <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'water', value = 'river') %>%
  osmdata_sf()
osm_rivers.sf2 <- osm_rivers.sf2$osm_multipolygons


osm_roads_primary.sf <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>%
  osmdata_sf()
osm_roads_primary.sf <- osm_roads_primary.sf$osm_lines
osm_roads_secondary.sf <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata_sf()
osm_roads_secondary.sf <- osm_roads_secondary.sf$osm_lines
osm_roads_tertiary.sf <- 
  opq(bbox = st_bbox(data_sf)) %>%
  add_osm_feature(key = 'highway', value = 'tertiary') %>%
  osmdata_sf()
osm_roads_tertiary.sf <- osm_roads_tertiary.sf$osm_lines



# create slope and hillshade
slope.raster = terrain(dem.raster, opt='slope')
aspect.raster = terrain(dem.raster, opt='aspect')
hill.raster = hillShade(slope.raster, aspect.raster, 40, 270)

dem_spdf <- as(dem.raster, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("elev", "lon", "lat")

hill_spdf <- as(hill.raster, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("hill", "lon", "lat")

# approche de https://gist.github.com/dirkseidensticker/ce98c6adfe16d5e4590e95c587ea0432#file-dem-hillshade-r-L15
# dem + hillshade
p_dirk <- ggplot() +
  geom_tile(data = hill_spdf, aes(x = lon, y = lat, fill = hill)) + # geom_raster = geom_tile, but assumes perfect grid ()
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_tile(data = dem_spdf, aes(x = lon, y = lat, fill = elev), alpha=0.4) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  geom_sf(data = osm_lakes.sf, fill = '#9ecae1', colour = NA) +
  geom_sf(data= osm_rivers.sf2, colour = '#9ecae1', size = 0.4)+
  geom_sf(data = osm_rivers.sf, colour = '#9ecae1', size = 0.05) +
  geom_sf(data = osm_roads_primary.sf, colour = '#636363', size = 0.1) +
  geom_sf(data = osm_roads_secondary.sf, colour = '#636363', size = 0.05) +
  geom_sf(data = osm_roads_tertiary.sf, colour = '#636363', size = 0.02) +
  geom_sf(data = data_sf, fill = NA) + 
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_continuous("", expand = c(0, 0)) +
  coord_sf(xlim = c(st_bbox(data_sf)$xmin, st_bbox(data_sf)$xmax),
           ylim = c(st_bbox(data_sf)$ymin, st_bbox(data_sf)$ymax)) +
  theme_bw() +
  theme(legend.position="none") + 
  labs(title = "hillshade black & white + élévation terrain color")

p_dirk
# approche de francisco https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/
# dem seulement

water_fill_francisco <- '#9ecae1'
p_francisco <- ggplot() +
  geom_raster(data = hill_spdf, aes(lon, lat, fill = hill), alpha = 45) +
  scale_fill_gradientn(colours = grey.colors(100)) +
  geom_sf(data = osm_lakes.sf, fill = water_fill_francisco, colour = NA) +
  geom_sf(data= osm_rivers.sf, colour = water_fill_francisco,fill = water_fill_francisco,  size = 0.4)+
  geom_sf(data= osm_rivers.sf2, colour = water_fill_francisco, fill = water_fill_francisco ,size = 0.4)+
  geom_sf(data = osm_roads_primary.sf, colour = '#636363', size = 0.1) +
  geom_sf(data = osm_roads_secondary.sf, colour = '#636363', size = 0.05) +
  geom_sf(data = osm_roads_tertiary.sf, colour = '#636363', size = 0.02) +
  geom_sf(data = data_sf, fill = NA) + 
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_continuous("", expand = c(0, 0)) +
  coord_sf(xlim = c(st_bbox(data_sf)$xmin, st_bbox(data_sf)$xmax),
           ylim = c(st_bbox(data_sf)$ymin, st_bbox(data_sf)$ymax)) +
  theme_bw() +
  theme(legend.position="none")+ 
  labs(title = "hillshade grey")

p_francisco
# test contour lines  
p_francisco + 
  geom_contour(data = dem_spdf, 
               aes(x=lon, y=lat, z = elev, alpha = after_stat(level))
  )  +

# approche simon  hillshade + elevation colorée oslo aul ieu de terrain pour pas mettre des sommets enneigés sur une bute.


water_fill_joly <- "#FAFDFF"
water_outline_joly <- "#89A4B2"

p_simon <- ggplot() +
  geom_tile(data = dem_spdf, aes(x = lon, y = lat, fill = elev), alpha=0.2) +
  scale_fill_gradientn(colours = scico::scico(11, palette = "oslo")) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = hill_spdf, aes(lon, lat, fill = hill), alpha = .45) +
  scale_fill_gradientn(colours = grey.colors(100)) +
  geom_sf(data = osm_lakes.sf, fill = water_fill_joly, colour = water_outline_joly) +
  geom_sf(data= osm_rivers.sf2, colour = water_outline_joly,fill = water_fill_joly,  size = 0.4)+
  geom_sf(data= osm_rivers.sf, colour = water_outline_joly,fill = water_fill_joly,  size = 0.4)+
  geom_sf(data = osm_roads_primary.sf, colour = '#636363', size = 0.1) +
  geom_sf(data = osm_roads_secondary.sf, colour = '#636363', size = 0.05) +
  geom_sf(data = osm_roads_tertiary.sf, colour = '#636363', size = 0.02) +
  geom_sf(data = data_sf, fill = NA) + 
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_continuous("", expand = c(0, 0)) +
  coord_sf(xlim = c(st_bbox(data_sf)$xmin, st_bbox(data_sf)$xmax),
           ylim = c(st_bbox(data_sf)$ymin, st_bbox(data_sf)$ymax)) +
  theme_bw() +
  theme(legend.position="none")+ 
  labs(title = "élévation oslo  + hillshade grey")

