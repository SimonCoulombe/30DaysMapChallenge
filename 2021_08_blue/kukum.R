# TODO: ajouter des effets à l'intérieur du lac st jean '
# https://www.katiejolly.io/blog/2020-03-06/inner-glow


#library(tint)
library(ggplot2)
#mtcars$am <- factor(mtcars$am, labels=c("manual", "automatic"))
library(tidyverse)
library(renojouet)
library(ggmap)
library(sf)
#library(cancensus)
library(mapview)
library(snapbox)
library(ggspatial)
#library(covidtwitterbot)
library(sf)
library(kableExtra)
library(here)
library(osmdata)
library(hereR)
library(lubridate)
library(leaflet)
library(cowplot)
library(tidygeocoder)
library(ggtext)
# pour ajouter routes https://www.dshkol.com/2018/better-maps-with-vector-tiles/
## todo utiliser  helpers function de @vb_jens You should try out the `geom_water()`, `geom_roads()` and `coord_bbox(bbox)` functions from my {mountainmathHelpers} package (not on CRAN).
library(rmapzen)
options(nextzen_API_key=Sys.getenv("nextzen_api_key"))
mz_set_tile_host_nextzen(key = getOption("nextzen_API_key"))
quebec_lambert <- '+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
register_google(key = Sys.getenv("googlemap_api_key"),
                account_type="premium")


hereR::set_key(Sys.getenv("HERE_key"))
Sys.setenv(MAPBOX_ACCESS_TOKEN = Sys.getenv("mapbox"))
library(snapbox)

get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

# how enrigh maps 
# https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/

# how get UTM grid number http://www.dmap.co.uk/utmworld.htm


# villes 
# https://fr.wikipedia.org/wiki/Chemin_de_fer_Quebec_and_Lake_St-John
villes = c ("Roberval", "Rivière-à-Pierre", "Saint-Raymond", "Shannon", "Québec", "Mashteuiatsh")

points_villes <- tibble(villes = villes) %>%
  mutate(municipalite = paste0(villes, " Québec, Canada")) %>%
  geocode (address = municipalite, method = "osm", lat = latitude, long = longitude) %>%
  st_as_sf(coords = c("longitude", "latitude" ), crs = 4326, remove= FALSE)  


# historical rails 
#https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/UCCFVQ

#
#EPSG:2138
#NAD27(CGQ77) / Quebec Lambert
rail <- read_sf("~/git/adhoc_prive/data/downloads/HR_rails_new/HR_rails_NEW.shp")
rails_qlsjr <- c(1844,43, 44, 1833,  1832, 1929,1930, 1886, 1894, 1895,1882,57)
qlsjr <- rail %>% filter(OBJECTID %in% rails_qlsjr) %>%
  st_transform(crs = 4326)



# bounding box du québec pour nos recherches openstreet map 
bb <- getbb("Quebec, Canada", format_out = "sf_polygon") 
bb <-  bb[1,] # garder le polygone de la province de québec

mes_objets_osmdata <- list()
mes_objets_osmdata["riviere_peribonka"] <- list(riviere_peribonka)
## On va chercher l'eau d'intérêt, soit  rivière péribonka, le lac péribonka, le lac saint jean

write_rds(riviere_peribonka, "kukum_riviere_peribonka.rds")
write_rds(lac_peribonka, "kukum_lac_peribonka.rds")

riviere_peribonka <-  bb %>% opq(timeout = 120) %>%
  add_osm_feature("name" , "Rivière Péribonka") %>%
  osmdata_sf()

riviere_peribonka_nice <- riviere_peribonka$osm_multipolygons

lac_peribonka <-  bb %>% opq(timeout = 120) %>%
  add_osm_feature("name" , "Lac Péribonka") %>%
  osmdata_sf()

lac_peribonka_nice <- lac_peribonka$osm_multipolygons

lac_st_jean <-  bb %>% opq() %>%
  add_osm_feature("name" , "Lac Saint-Jean") %>%
  osmdata_sf() 

lac_st_jean_nice <- lac_st_jean$osm_multipolygons %>%
  filter(osm_id != 	1596407)

riviere_manouane <-  bb %>% opq(timeout = 120) %>%
  add_osm_feature("name" , "Rivière Manouane") %>%
  osmdata_sf()

lac_manouane <-  bb %>% opq(timeout = 120) %>%
  add_osm_feature("name" , "Lac Manouane") %>%
  osmdata_sf()

passes_dangereuses <- bb %>% opq(timeout = 120) %>%
  add_osm_feature("name", "Passes Dangereuses") %>%
  osmdata_sf()

passes_dangereuses_nice <- passes_dangereuses$osm_points[1,] %>% 
  mutate(
    longitude= map_dbl( geometry, ~st_coordinates(.x)[1]),
    latitude= map_dbl( geometry, ~st_coordinates(.x)[2])
  ) 

eau_qui_minteresse <- bind_rows(lac_st_jean_nice, lac_peribonka_nice, riviere_peribonka_nice)



"data/downloads/SR_50M/"
library(rnaturalearth)
# https://github.com/bydata/30DayMapChallenge-2021/blob/main/R/day02-lines.R
# Download shaded relief raster (SR) in high/low resolution (HR/LR)
# https://www.naturalearthdata.com/features/
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/SR_50M.zip


#ce code marche pas pour moi 
dir_raster <- here("data", "downloads" ,"SR_LR")
# if (!file.exists(dir_raster)) {
   ne_download(scale = 50, type = "SR_LR", category = "raster",
               destdir = dir_raster, load = FALSE)
# }


# Read raster data for relief
relief <- raster::raster(here(dir_raster, "SR_LR.tif")) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value =  SR_LR)


relief_filtered <- relief %>%
  filter(x >= -72.6, x <= -71, y >= 48.2, y <= 50.5) %>% # numbers from bounding box around my data
  mutate(geometry = map2(x, y, ~st_point(c(.x, .y)))) %>%
  st_as_sf()
st_crs(relief_filtered) <- "EPSG:4326"



vector_tiles <- get_vector_tiles(st_bbox(eau_qui_minteresse))
water <- as_sf(vector_tiles$water)

#  water %>% ggplot() + geom_sf() # ouin il y a pas grand chose..

# https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/
## eau autour de ce qui m'intéresse.     
#https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/



## autre option pour shaded relief 
#Une version de relief shadé en rstats
#https://eliocamp.github.io/codigo-r/2018/02/how-to-make-shaded-relief-in-r/?utm_content=buffer628d9&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer


library(osmdata)
osm_lakes.sf <- 
  opq(bbox = st_bbox(eau_qui_minteresse)) %>%
  add_osm_feature(key = 'water', value = 'lake') %>%
  osmdata_sf()
osm_lakes.sf <- osm_lakes.sf$osm_multipolygons

osm_rivers.sf <- 
  opq(bbox = st_bbox(eau_qui_minteresse)) %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()
osm_rivers.sf <- osm_rivers.sf$osm_lines




osm_rivers.sf2 <- 
  opq(bbox = st_bbox(eau_qui_minteresse)) %>%
  add_osm_feature(key = 'water', value = 'river') %>%
  osmdata_sf()
osm_rivers.sf2 <- osm_rivers.sf2$osm_multipolygons



  
ggplot() +
  geom_raster(data = relief_filtered, aes(x ,y,  alpha = value), fill = "white", 
              show.legend = FALSE
  )+ 
  geom_sf(data =osm_lakes.sf, fill = '#9ecae1', colour = NA)+
  geom_sf(data = osm_rivers.sf2, fill = '#9ecae1', colour = NA)+
  geom_sf(data = eau_qui_minteresse,fill =  '#9ecae1', color =  '#9ecae1') +
  # geom_sf( data = qlsjr ) + 
  
  geom_sf(data = points_villes %>% filter(villes == "Mashteuiatsh")) +
  
  ggrepel::geom_text_repel(data = points_villes  %>% filter(villes == "Mashteuiatsh"),
                           aes(x = longitude, y = latitude, label = villes),  #
                           fontface = "bold"#,
                           #nudge_x = c(0   , 0.4    , 0    , 0, 0      , 0 ),
                           #nudge_y = c(0   , 0    , 0    , 0, 0      , 0 )
  ) +
  geom_sf(data = passes_dangereuses_nice)+ 
  ggrepel::geom_text_repel(data = passes_dangereuses_nice ,
                           aes(x = longitude, y = latitude, label = name),  #
                           fontface = "bold")  + 
  scale_x_continuous(breaks = c(-72.5, -72 ,-71.5, -71, -70.5)) + 
  scale_y_continuous(breaks = c(48.5, 49, 49.5, 50)) +
  coord_sf( xlim = c(-72.5, -71.1), ylim = c(48.4, 50.4))  + 
  scale_alpha(c(0.7, 0)) + 
  theme_minimal(base_family = "Roboto") + 
  theme(plot.background = element_rect(color = NA, fill = "grey98"),
        panel.background = element_rect(color = NA, fill = "#b8a149"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, l = 16, r = 16, b = 2),
        plot.caption = element_markdown(family = "Roboto", size = 11,
                                        hjust = 0.5, color = "grey35",
                                        margin = margin(t = 20, b = 6)))+ 
  labs(
    
  caption = "Data: **Natural Earth**, **OpenStreetMap** |
     code largement inspiré par **@_ansgar**",
       x = NULL, y = NULL) 
  

#cowplot::theme_map()

mapview(eau_qui_minteresse)
fulldata <- bind_rows(eau_qui_minteresse, qlsjr)

# get eleavatio ndata  
library(elevatr)
library(raster)

bbox_fullmap <- st_bbox(fulldata)
# Generate a data frame of lat/long coordinates for get_elev_raster()
ex.df <- data.frame(x=seq(from=bbox_fullmap$xmin , to=bbox_fullmap$xmax, length.out=100), 
                    y=seq(from=bbox_fullmap$ymin , to=bbox_fullmap$ymax, length.out=100))

# Use elevatr package to get elevation data for each point.

# Specify projection. (utm not used) ----
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
elevation_raster_zoom <- 10


elevation <- get_elev_raster(ex.df, prj = prj, z = elevation_raster_zoom, clip = "bbox")

elmat <-
  as.matrix(elev) %>%
  apply(., 2, rev)


# contourmap 2d
raster::contour(elev)

library(rayshader)
elev_mat <- raster_to_matrix(elevation)


elev_mat %>%
  sphere_shade() %>%
  plot_map()


ray_shadow <- ray_shade(elev_mat, sunaltitude = 40, zscale = 30, multicore = TRUE)
ambient_shadow <- ambient_shade(elev_mat, zscale = 30)




SR_50M.tif
elev_mat %>%
  sphere_shade() %>%
  #add_overlay(topo_rgb_array) %>%
  add_shadow(ray_shadow, max_darken = 0.7) %>%
  add_shadow(ambient_shadow, 0.25) %>%
  plot_map()

# https://michaelpaulschramm.com/posts/2020-10-08-rayshading_maps/

st_bbox(eau_qui_minteresse)
# ## on bind l eau et le rail, ça donnera un nouveau bounding box pour notre carte dans lequel aller chercher toute l'eau.
# 
# mydata <- bind_rows(eau_qui_minteresse, qlsjr, points_villes )
# mapview(mydata)
# 
# 
# bbox_arrond <- st_bbox(arrond)
# bbox_arrond_quebec_lambert <-st_bbox(st_transform(arrond, crs = quebec_lambert) )
# vector_tiles <- get_vector_tiles(my_bbox_bbx)
# 
# water <- as_sf(vector_tiles$water)
# roads <- as_sf(vector_tiles$roads)
# 
# water <- as_sf(vector_tiles$water)
# roads <- as_sf(vector_tiles$roads)
# 
# 
# 
# ## code de mes monuments du québec..
# historic_monument <-  bb %>% opq(timeout = 120) %>%
#   add_osm_feature(key= "water") %>%
#   osmdata_sf()
# 
# 
# # get river from openstreetmap https://gis.stackexchange.com/questions/98371/getting-shapefile-of-river-from-openstreetmap
# # dominic roye where can we find mercadona supermarkets https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
# 
# 
# 

# 
# ```{r, echo = FALSE}
# 
# 
# # x <- bb %>% opq() %>%
# #   add_osm_feature(key= "memorial"
# #   )%>%
# #   osmdata_sf()
# # 
# # y <-  bb %>% opq() %>%
# #   add_osm_feature(key= "monument"
# #   )%>%
# #   osmdata_sf()
# # 
# # statue <-  bb %>% opq() %>%
# #   add_osm_feature(key= "statue"
# #   )%>%
# #   osmdata_sf()
# # 
# # 
# # historic <-  bb %>% opq() %>%
# #   add_osm_feature(key= "historic"
# #   )%>%
# #   osmdata_sf()
# # 
# # 
# # tourism <-  bb %>% opq() %>%
# #   add_osm_feature(key= "tourism"
# #   )%>%
# #   osmdata_sf()
# # 
# 
