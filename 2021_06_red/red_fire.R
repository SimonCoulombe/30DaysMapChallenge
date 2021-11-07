#just an attempt to apply dominic royé'S awesome fireflyblo post to canada wildfires


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

library(conflicted)
conflict_prefer("plotRGB", "terra")
conflict_prefer("filter", "dplyr")


# Download fire data here 
#https://firms.modaps.eosdis.nasa.gov/download/
# create new request 
fires <- read_sf(here("2021_06_red/data/DL_FIRE_M-C61_232787/fire_archive_M-C61_232787.shp"))

# download blue marble geotiff base layer by going to  worldview.earthdata.nasa.gov 
# add blue marble layer to map
# "Take a snapshot" , export to geotiff, 5km/ pixel
bm <- rast(here("2021_06_red/data/snapshot-2021-11-06T00_00_00Z.tiff"))


# get country limits, province limites, etc..
limits <- ne_countries(scale = 50, returnclass = "sf")
canada <- ne_countries(scale = 50, returnclass = "sf", country = "canada")
provinces <- ne_states(returnclass = "sf", country = "canada")

# these are the 3 provinces we want the map about.
bc <- provinces %>% filter(name  %in% c("British Columbia", "Alberta", "Saskatchewan"))


## I toyed about adding a 100 km buffer around the provinces, but didnt do it.
add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}
#bc_100km_buffer <- add_buffer_in_meters_and_return_to_original_crs(bc,100000)


## What CRS should I use for these 3 provinces? 
# crsuggest::suggest_crs() suggests BC Alberts (crs 3153), but I'm going to do a customised lambert comform conic centered on the three provinces
# as explained here https://gis.stackexchange.com/questions/215177/which-projection-to-use-to-map-three-canadian-provinces
suggest_crs(bc, type ="projected", gcs = "4326", units = "m") ## 3347 statistics canada lambert # 	 3153 NAD83(CSRS) / BC Albers

my_crs <- "+proj=lcc +lat_0=40 +lon_0=-120 +lat_1=50 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"      ## pour 3 porvs de l'ouest
#my_crs <- 3153


# project everything to my crs
fires_projected <- fires %>% st_transform(crs = my_crs)
bc_projected <- bc %>% st_transform(my_crs)
bm_projected <-terra::project(bm, my_crs)  # if we specify a proj4, we pass it as a text string to terra::project
#bm_projected <-terra::project(bm, paste0("epsg:", my_crs)) # if you specify a crs number, it has be be passed as espg:3151


# crop  the rater to the projected provinces
bm_projected_crop <- terra::crop(bm_projected, bc_projected)


# function by Dr Royé to change saturation from RGB
saturation <- function(rgb, s = .5){
  
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  
  rgb_new <- as.vector(t(hsl2rgb(hsl)))
  
  return(rgb_new)
  
}
# apply the function to unsaturate with 5%
bm_projected_crop_desat <- app(bm_projected_crop, saturation, s = .05)


# define the final map extent
# 
# bx <- tibble(x = c(-141.00215, -52.65366), y = c(41.67485, 70.1)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   st_transform(my_crs) %>% 
#   st_bbox()
bx <- bc_projected %>% st_bbox()


# create map graticules
grid <- st_graticule(bc_projected) 

# define color
fire_red <- "#F73718"

# get location of title in the USA (bottom left) in crs 3857 for title annotation
# I just opened mapview(bc) , picked a spot in the bottom right and wrote the lat/lon here:
#tibble(lon = -102, lat = 47.4) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(my_crs)
# # A tibble: 1 x 1
# geometry
# *       <POINT [m]>
#1 (1350133 1009602)


# map the damn thing!!
ggplot() +
  layer_spatial(data = stack(bm_projected_crop_desat)) + # blue marble background map
  geom_sf(data = limits %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # country boundaries
  geom_sf(data = provinces %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # provinces boundaries
  #geom_sf(data = grid, colour = "white", size = .1, alpha = .5) +
  geom_glowpoint(data = fires_projected,
                 aes(geometry = geometry), 
                 alpha = .8,
                 color = fire_red,
                 shadowcolour = fire_red,
                 shadowalpha = .1,
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.1, 1.5)) +
  new_scale("size") +
  geom_glowpoint(data = fires_projected,
                 aes(geometry = geometry), 
                 alpha = .6,
                 shadowalpha = .05,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.01, .7)) +
  coord_sf(xlim = c(bx$xmin, bx$xmax),
           ylim = c(bx$ymin, bx$ymax),
           crs = my_crs,
           expand = FALSE
  ) +
  theme_void(base_family = "Roboto")+
  theme(
    plot.title = element_text(size = 50, vjust = -5, colour = "white", hjust = .95)
  )+
  labs(
    title = "Western Canada Wild Fires"
  )+
  annotate("richtext",
           label = "Fire data: NASA FIRMS (Summer 2021), Base layer: NASA Blue Marble, original code and theme: @dr_xeo,   map by @coulsim  ",
           x = 1350133 , y = 970000,
           family = "Roboto",
           size = 6, col = "white",
           label.color = NA, fill = NA,
           hjust = 1,
  ) 


# use knitr to remove white borders around plots..
# https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/
png_file <- here("2021_06_red/fire_map_west.png")

ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)

knitr::plot_crop(png_file)



#### on recommence avec le québec!!



# these are the 3 provinces we want the map about.
bc <- provinces %>% filter(name  %in% c("Québec"))


## I toyed about adding a 100 km buffer around the provinces, but didnt do it.
add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}
bc_100km_buffer <- add_buffer_in_meters_and_return_to_original_crs(bc,100000)


## What CRS should I use for these 3 provinces? 
# crsuggest::suggest_crs() suggests BC Alberts (crs 3153), but I'm going to do a customised lambert comform conic centered on the three provinces
# as explained here https://gis.stackexchange.com/questions/215177/which-projection-to-use-to-map-three-canadian-provinces
suggest_crs(bc, type ="projected") ## 3347 statistics canada lambert # 	 3153 NAD83(CSRS) / BC Albers

#my_crs <- "+proj=lcc +lat_0=40 +lon_0=-120 +lat_1=50 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"      ## pour 3 porvs de l'ouest
#my_crs <- 3153 bc
my_crs <- 32198  # quebec lambert nad83


# project everything to my crs
fires_projected <- fires %>% st_transform(crs = my_crs)
bc_projected <- bc %>% st_transform(my_crs)
bc_100km_buffer_projected <-  bc_100km_buffer %>% st_transform(my_crs)
#bm_projected <-terra::project(bm, my_crs)  # if we specify a proj4, we pass it as a text string to terra::project
bm_projected <-terra::project(bm, paste0("epsg:", my_crs)) # if you specify a crs number, it has be be passed as espg:3151


# crop  the rater to the projected provinces
bm_projected_crop <- terra::crop(bm_projected, bc_100km_buffer_projected)


# function by Dr Royé to change saturation from RGB
saturation <- function(rgb, s = .5){
  
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  
  rgb_new <- as.vector(t(hsl2rgb(hsl)))
  
  return(rgb_new)
  
}
# apply the function to unsaturate with 5%
bm_projected_crop_desat <- app(bm_projected_crop, saturation, s = .05)


# define the final map extent
# 
# bx <- tibble(x = c(-141.00215, -52.65366), y = c(41.67485, 70.1)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   st_transform(my_crs) %>% 
#   st_bbox()
bx <- bc_100km_buffer_projected %>% st_bbox()


# create map graticules
grid <- st_graticule(bc_100km_buffer_projected) 

# define color
fire_red <- "#F73718"

# get location of title in the USA (bottom left) in crs 3857 for title annotation
# I just opened mapview(bc) , picked a spot in the bottom right and wrote the lat/lon here:
tibble(lon = -60, lat = 45) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(my_crs)
# # A tibble: 1 x 1
# geometry
# *       <POINT [m]>
#1 (670075.3 151339.6)


# map the damn thing!!
ggplot() +
  layer_spatial(data = stack(bm_projected_crop_desat)) + # blue marble background map
  geom_sf(data = limits %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # country boundaries
  geom_sf(data = provinces %>% st_transform(my_crs), fill = NA, size = .3, colour = "white") + # provinces boundaries
  #geom_sf(data = grid, colour = "white", size = .1, alpha = .5) +
  geom_glowpoint(data = fires_projected,
                 aes(geometry = geometry), 
                 alpha = .8,
                 color = fire_red,
                 shadowcolour = fire_red,
                 shadowalpha = .1,
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.1, 1.5)) +
  new_scale("size") +
  geom_glowpoint(data = fires_projected,
                 aes(geometry = geometry), 
                 alpha = .6,
                 shadowalpha = .05,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.01, .7)) +
  coord_sf(xlim = c(bx$xmin, bx$xmax),
           ylim = c(bx$ymin, bx$ymax),
           crs = my_crs,
           expand = FALSE
  ) +
  theme_void(base_family = "Roboto")+
  theme(
    plot.title = element_text(size = 50, vjust = -5, colour = "white", hjust = .95)
  )+
  labs(
    title = "Quebec Wild Fires"
  )+
  annotate("richtext",
           label = "Fire data: NASA FIRMS (Summer 2021), Base layer: NASA Blue Marble, original code and theme: @dr_xeo,   map by @coulsim  ",
           x = 670075.3 , y = 151339.6,
           family = "Roboto",
           size = 6, col = "white",
           label.color = NA, fill = NA,
           hjust = 1,
  ) 


# use knitr to remove white borders around plots..
# https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/
png_file <- here("2021_06_red/fire_map_quebec.png")

ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)

knitr::plot_crop(png_file)
