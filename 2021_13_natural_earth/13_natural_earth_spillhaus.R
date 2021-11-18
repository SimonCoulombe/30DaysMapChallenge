library(rnaturalearth)
library(mapview)
library(sf)
library(tidyverse)
rnaturalearth_cache <- "/home/simon/rnaturalearth_cache/"
ocean_bottom <- ne_download(scale = 50, type = 'OB_50M', category = 'raster', destdir = rnaturalearth_cache)


ne_download( scale = 50, type = 'coastline', destdir = rnaturalearth_cache, category = "physical")
ne_download( scale = 50, type = 'land', destdir = rnaturalearth_cache, category = "physical")
coastlines_sf <- ne_load( scale = 50, type = 'coastline', destdir = rnaturalearth_cache, category = "physical", returnclass =  "sf")
mapview(coastlines_sf)

ggplot() +
  
  geom_sf(data = coastlines_sf,  aes(fill = "black"))
