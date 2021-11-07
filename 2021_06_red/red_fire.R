#just apply dominic royé'S awesome fireflyblo post to canada wildfires


# install the packages if necessary

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("terra")) install.packages("terra")
if(!require("raster")) install.packages("raster")
if(!require("plotwidgets")) install.packages("plotwidgets")
if(!require("ggshadow")) install.packages("ggshadow")
if(!require("ggspatial")) install.packages("ggspatial")
if(!require("ggnewscale")) install.packages("ggnewscale")
if(!require("janitor")) install.packages("janitor")
if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("crsuggest")) install.packages("crsuggest")
if(!require("here")) install.packages("here")
if(!require("mapview")) install.packages("mapview")
library(ggtext) # for element markdown
# download blue marble geotiff
#worldview.earthdata.nasa.gov 
# add blue marble layer
# "Take a snapshot" , export to geotiff, 5km/ pixel

library(conflicted)
conflict_prefer("plotRGB", "terra")
conflict_prefer("filter", "dplyr")

limits <- ne_countries(scale = 50, returnclass = "sf")
canada <- ne_countries(scale = 50, returnclass = "sf", country = "canada")

provinces <- ne_states(returnclass = "sf", country = "canada")
bc <- provinces %>% filter(name  %in% c("British Columbia", "Alberta", "Saskatchewan"))
#bc <- provinces %>% filter(name  %in% c("British Columbia", "Alberta"))

add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}
bc_100km_buffer <- add_buffer_in_meters_and_return_to_original_crs(bc,100000)


suggest_crs(bc_100km_buffer, type ="projected", gcs = "4326", units = "m") ## 3347 statistics canada lambert # 	 3153 NAD83(CSRS) / BC Albers

#my_crs <- 3153
# https://gis.stackexchange.com/questions/215177/which-projection-to-use-to-map-three-canadian-provinces
my_crs <- "+proj=lcc +lat_0=40 +lon_0=-120 +lat_1=50 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"      ## pour 3 porvs de l'ouest
#my_crs <- 3153
 
#https://firms.modaps.eosdis.nasa.gov/download/
# create new request 
fires <- read_sf(here("2021_06_red/data/DL_FIRE_M-C61_232787/fire_archive_M-C61_232787.shp"))
fires <- fires %>% st_transform(crs = my_crs)

bm <- rast(here("2021_06_red/data/snapshot-2021-11-06T00_00_00Z.tiff"))

#bm <- terra::crop(bm, extent(-144 ,-90,46 ,70)) #pour 3 provinces
#bm <- terra::crop(bm, extent(-144 ,-102,46 ,64)) 

# function to change saturation from RGB

saturation <- function(rgb, s = .5){
  
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  
  rgb_new <- as.vector(t(hsl2rgb(hsl)))
  
  return(rgb_new)
  
}


# apply the function to unsaturate with 5%
# bm_desat <- bm temp
bm_desat <- app(bm, saturation, s = .05)

# plot new RGB image

# project 

bm_desat <- terra::project(bm_desat, paste0("epsg:", my_crs))


# define the final map extent
# 
# bx <- tibble(x = c(-141.00215, -52.65366), y = c(41.67485, 70.1)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   st_transform(my_crs) %>% 
#   st_bbox()
bx <- bc %>% st_transform(my_crs) %>% st_bbox()


# create map graticules
grid <- st_graticule(limits %>% st_transform(my_crs)) 

fire_red <- "#F73718"

# get location of title in the USA (bottom left) in crs 3857 for title annotation
tibble(lon = -139, lat = 50) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(my_crs)
#(73184.87  639122.7)

ggplot() +
  layer_spatial(data = stack(bm_desat)) + # blue marble background map
  geom_sf(data = limits %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # country boundaries
  geom_sf(data = provinces %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # country boundaries
  #geom_sf(data = grid, colour = "white", size = .1, alpha = .5) +
  geom_glowpoint(data = fires,
                 aes(geometry = geometry), 
                 alpha = .8,
                 color = fire_red,
                 shadowcolour = fire_red,
                 shadowalpha = .1,
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.1, 1.5)) +
  new_scale("size") +
  geom_glowpoint(data = fires,
                 aes(geometry = geometry), 
                 alpha = .6,
                 shadowalpha = .05,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.01, .7)) +
  coord_sf(xlim = c(bx$xmin, bx$xmax),
           ylim = c(bx$ymin, bx$ymax),
           crs = my_crs
           #expand = FALSE
  ) +
  labs(#title = "Western Canada wild fires, summer 2021",
       caption= "Source: NASA, map by @coulsim using code and design by @dr_xeo | (shitty projectionn, but I'm done)") +
  annotate("richtext",
           label = "Wildfires in Western Canada <br>Summer 2021",
           x = 73184.87 , y = 639122.7,
           family = "Roboto",
           size = 8, col = "grey65",
           label.color = NA, fill = NA,
           hjust = 0,
  )  + 
  theme_void()

ggsave("fire_map_west.png", width = 15, height = 15, units = "in", dpi = 300)
