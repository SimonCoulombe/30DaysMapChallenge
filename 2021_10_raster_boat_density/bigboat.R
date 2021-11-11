library(sf)
library(raster)
library(terra)
library(tidyverse)
library(mapview)
library(here)
library(stars)
library(rnaturalearth)
library(ggspatial) # layer_spatial
# Global Shipping Traffic Density
#  total number of (hourly)AIS positions that have been reported by ships in 
# each grid cell with dimensions of 0.005 degree by 0.005 degree
# (approximately a 500m x 500m grid at the Equator)
#https://datacatalog.worldbank.org/search/dataset/0037580/Global-Shipping-Traffic-Density
oil <- rast(here("2021_10_raster_boat_density/data/ShipDensity_OilGas1.tif"))
oil_small <- aggregate(oil, fact=20, fun=sum)
rm(oil)
gc()
fish <- rast(here("2021_10_raster_boat_density/data/ShipDensity_Fishing1.tif"))
fish_small <- aggregate(fish, fact=20, fun=sum)
fish_super_small <- aggregate(fish, fact=200, fun=sum)
rm(fish)
gc()

commercial <- rast(here("2021_10_raster_boat_density/data/ShipDensity_Commercial1.tif"))
commercial_small <- aggregate(commercial, fact=20, fun=sum)
rm(commercial)
gc()
passenger <- rast(here("2021_10_raster_boat_density/data/ShipDensity_Passenger1.tif"))
passenger_small <- aggregate(passenger, fact=20, fun=sum)
rm(passenger)
gc()

oil_stars <- st_as_stars(oil_small) %>% mutate(type = "oil", density = ShipDensity_OilGas1) %>% select(-ShipDensity_OilGas1)
fish_stars <- st_as_stars(fish_small)%>% mutate(type = "fish", density = ShipDensity_Fishing1) %>% select(-ShipDensity_Fishing1)
fish_super_small_stars <- st_as_stars(fish_super_small)%>% mutate(type = "fish", density = ShipDensity_Fishing1) %>% select(-ShipDensity_Fishing1)
commercial_stars <- st_as_stars(commercial_small) %>% mutate(type = "commercial", density = ShipDensity_Commercial1) %>% select(-ShipDensity_Commercial1)
passenger_stars <- st_as_stars(passenger_small)%>% mutate(type = "passenger", density = ShipDensity_Passenger1) %>% select(-ShipDensity_Passenger1)

oil_stars_projected <- oil_stars %>% st_transform(crs= "+proj=wintri")

fish_super_small_projected <- fish_super_small_stars %>% st_transform(4326)

#ggplot( ) + 
#  geom_stars(data = fish_super_small_stars)
# oil_spdf <- as(raster(oil_small), "SpatialPixelsDataFrame")
# oil_spdf <- as.data.frame(oil_spdf)
# colnames(oil_spdf) <- c("oil", "lon", "lat")

ggplot() + 
  geom_stars(data = oil_stars, aes(x=x, y=y, fill = density))+ 
  scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), trans = "log2",  na.value = "black") +
  coord_sf(expand = FALSE)+ 
  labs(
    title = "Map #1",
    x = NULL, y = NULL
  ) 
png_file <- here("2021_10_raster_boat_density/oil.png")
ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)
knitr::plot_crop(png_file)


ggplot() + 
  geom_stars(data = passenger_stars, aes(x=x, y=y, fill = density))+ 
  scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), trans = "log2",  na.value = "black") +
  coord_sf(expand = FALSE)+ 
  labs(
    title = "Map #2",
    x = NULL, y = NULL
  ) 
png_file <- here("2021_10_raster_boat_density/passenger.png")
ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)
knitr::plot_crop(png_file)


ggplot() + 
  geom_stars(data = commercial_stars, aes(x=x, y=y, fill = density))+ 
  scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), trans = "log2",  na.value = "black") +
  coord_sf(expand = FALSE)+ 
  labs(
    title = "Map #3",
    x = NULL, y = NULL
  ) 
png_file <- here("2021_10_raster_boat_density/commercial.png")
ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)
knitr::plot_crop(png_file)


ggplot() + 
  geom_stars(data = fish_stars, aes(x=x, y=y, fill = density))+ 
  scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), trans = "log2",  na.value = "black") +
  coord_sf(expand = FALSE)+ 
  labs(
    title = "Map #4",
    x = NULL, y = NULL
  ) 
png_file <- here("2021_10_raster_boat_density/fish.png")
ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)
knitr::plot_crop(png_file)






# 
# 
# ggplot() +
#   #layer_spatial(data = stack(ra)) + # blue marble background map
#   geom_raster(data = oil_spdf, aes(x=lon, y = lat, fill = oil))+ 
#   scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), trans = "log2",  na.value = "black") 
#   #coord_sf(ylim = c(75, 89))
# 
# 
# png_file <- here("2021_10_raster_boat_density/fire_map_quebec.png")
# ggsave(png_file, width = 15, height = 15, units = "in", dpi = 300)
# knitr::plot_crop(png_file)
