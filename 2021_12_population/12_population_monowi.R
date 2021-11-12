library(osmdata)
library(sf)
library(tidyverse)
library(mapview)
library(snapbox)
library(ggspatial)
library(crsuggest)
library(ggtext)
osm_polygon <- osmdata::getbb(place_name ="Monowi, Boyd County, Nebraska,United states", format_out = "sf_polygon")

# 
# my_crs <- crsuggest::suggest_crs(osm_polygon, type = "projected", units = "m", gcs  =4326) %>%
#   pull(crs_code) %>% .[1] %>% as.numeric()# utm 14N
# 
# 
# 
# add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
#   current_crs <- sf::st_crs(data)
#   meter_crs <- crsuggest::suggest_crs(data, type = "projected", units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
#   data %>% 
#     st_transform(crs = meter_crs) %>%
#     st_buffer(dist = distance) %>%
#     st_transform(crs = current_crs)
# }
# 
# 
# city_limit_projected <- osm_polygon %>% st_transform(my_crs)
# 
# city_limit_projected_bbox_in_4326 <-city_limit_projected %>% 
#   st_bbox() %>% 
#   st_as_sfc() %>% 
#   st_transform(4326) 
# 
# osm_rivers.sf <- 
#   opq(bbox = city_limit_projected_bbox_in_4326) %>%
#   add_osm_feature(key = 'waterway', value = 'river') %>%
#   osmdata_sf()
# osm_rivers.sf <- osm_rivers.sf$osm_lines
# 
# 
# 
# osm_highway.sf  <- opq(city_limit_projected_bbox_in_4326) %>% 
#   add_osm_feature("highway") %>% 
#   osmdata_sf() 
# mapview(osm_highway.sf$osm_lines, zcol = "highway")



one_person_dot <- tibble(x = -98.330, y= 42.8287) %>% st_as_sf(coords = c("x","y"), crs = 4326 )

ggplot() + 
  layer_mapbox(city_limit_projected_bbox_in_4326, scale_ratio = 0.5,
               #mapbox_api_access_token = "pk.eyJ1IjoibW9yZ2x1bSIsImEiOiJjaWVvOHQzamQwaHMwc21rbThkN3Z5OTVxIn0.QgvzvOHp46vl5Ht3JwGNRg",
               mapbox_light()
               #mapbox_satellite()
  )  +
  labs(x = NULL, y= NULL, 
       title = "Population map of Monowi, Nebraska",
       subtitle = "1 dot = 1 person",
       caption ="map by @coulsim in #rstats using {ggplot2}. base layer from mapbox using {snapbox}") + 
  geom_sf(data = one_person_dot, color = "red", size = 3 ) +

  coord_sf(expand = FALSE)


png_file <- here::here("2021_12_population/day12_monowi.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)


