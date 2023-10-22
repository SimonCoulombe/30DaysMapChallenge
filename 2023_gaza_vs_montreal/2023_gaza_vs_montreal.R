library(magick)
library(ggspatial)
library(osmdata)
library(sf)
library(tidyverse)
library(mapview)
#remotes::install_github("anthonynorth/snapbox")
library(snapbox)
library(ggspatial)
library(crsuggest)
library(ggtext)




# 1 get gaza strip polygon
gaza_osm_polygon <- osmdata::getbb(place_name ="Gaza strip", format_out = "sf_polygon")

# 2 get the crs for the projection of gaza strip area in meters
# get the centroid of gaza in that projection.  we will use that to 
# transpose gaza over montreal
gaza_crs <-  crsuggest::suggest_crs(gaza_osm_polygon, type = "projected", units = "m", gcs  =4326) %>%
  pull(crs_code) %>% .[1] %>% as.numeric()
gaza_projected <- st_transform(gaza_osm_polygon, gaza_crs) # this is "sf"         "data.frame"
gaza_centroid  <- st_centroid(gaza_projected)# this is  "sf"         "data.frame"

# 3 we get the polygon(s) for Montreal and union them into a single polygon
montreal_osm_polygon <- osmdata::getbb(place_name ="Montreal, Canada", format_out = "sf_polygon")
montreal_unioned <- st_union(montreal_osm_polygon) %>% 
  st_sf()

# 4- get the crs for the project of the montreal area in meters:

montreal_crs <- crsuggest::suggest_crs(montreal_unioned, type = "projected", units = "m", gcs  =4326) %>%
  pull(crs_code) %>% .[1] %>% as.numeric()

# 5 get the centroid of montreal in that projection.  we will use that to 
# transpose gaza over montreal
montreal_projected <- st_transform(montreal_unioned, montreal_crs)
montreal_centroid  <- st_centroid(montreal_projected)

# 6 move montreal over gaza and set to gaza CRS
montreal_centered <- montreal_projected - montreal_centroid # this is a data.frame
montreal_centered_sf <- st_as_sf(montreal_centered) # this is "sf"         "data.frame", with no CRS

z <- montreal_centered_sf +  gaza_centroid   # add gaza centroid    
montreal_over_gaza_strip <- z %>% st_sf(crs = gaza_crs)  # set crs to gaza_crs

# 7 quick mapview

mapview(montreal_over_gaza_strip)

# 8 get 50km buffer around gaza strip for  ggplot 
add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, type = "projected", units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>%
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}

area_of_interest <- gaza_projected %>% 
  add_buffer_in_meters_and_return_to_original_crs(.,20000) 



# The palette from: https://jfly.uni-koeln.de/color/#pallet
okabeito_colors_list <- c(
  `orange` = "#E69F00",
  `light blue` = "#56B4E9",
  `green` = "#009E73",
  `yellow` = "#F0E442",
  `blue` = "#0072B2",
  `red` = "#D55E00",
  `purple` = "#CC79A7",
  `grey` = "#999999",
  `black` = "#000000",
  `sky blue` = "#56B4E9",
  `bluish green` = "#009E73",
  `vermillion` = "#D55E00",
  `reddish purple` = "#CC79A7",
  `dark yellow` = "#F5C710",
  `amber` = "#F5C710"
)
# 
# todo get title working
# title_location <- tibble(lat = 31.37, lon = 34.3) %>% 
#   st_as_sf(coords= c("lon", "lat"), crs = 4326) %>% 
#   st_transform(gaza_crs)  %>%
#   dplyr::mutate(x = sf::st_coordinates(.)[,1],
#                 y = sf::st_coordinates(.)[,2])


ggplot() + 
  layer_mapbox(area_of_interest, scale_ratio = 0.5,
               mapbox_api_access_token = Sys.getenv("mapbox_api_token"),
               mapbox_dark()
               #mapbox_satellite()
  )  +
  labs(x = NULL, y= NULL#, 
#       title = "Comparaison de la superficie de Montréal et de la bande de Gaza",
       #subtitle = "",
       #caption ="made by @coulsim in #rstats, base layer from mapbox using {snapbox}, polygons from OpenStreetMaps"
) + 
  geom_sf(data = montreal_over_gaza_strip, color = okabeito_colors_list["orange"], fill  = NA, linewidth =1 ) +
  geom_sf(data = gaza_projected, color = okabeito_colors_list["sky blue"], fill  = NA, linewidth =1  ) +
  ggspatial::annotation_scale(location="br") + 
  coord_sf(expand = FALSE) + 
  # annotate("richtext",
  #          label = "KUKUM",
  #          x = title_location$x , y = title_location$y,
  #          # family = "Roboto",
  #          # size = 24, col = "grey15",
  #          label.color = NA, fill = NA,
  #          hjust = 0,
  # )+
  ggthemes::theme_map() # remove lat/long axis


png_file <- here::here("montreal_gaza.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)


# 9 todo ajouter les autoroutes sur montreal?
montreal_bbox <-  montreal_unioned %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_transform(4326) 

osm_highway.sf  <- opq(montreal_bbox) %>%
  add_osm_feature(key="highway", value= c("motorway") ) %>%
  osmdata_sf()

highway_lines <- osm_highway.sf$osm_lines
mapview(highway_lines, zcol = "highway")


