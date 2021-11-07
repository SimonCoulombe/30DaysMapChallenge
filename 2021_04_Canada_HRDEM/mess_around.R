library(tidyverse)
library(sf)
library(mapview)
library(here)
library(raster)
data <- read_sf("2021_04_Canada_HRDEM/Datasets_Footprints/Datasets_Footprints.shp")
data %>% filter(Tile_name %in% c("1m_utm19_w_12_123", 	"2m_utm19_w_5_61")) %>% mapview()
charlevoix <- data %>%
  filter(Project == "600011-10_Charlevoix", 
         Tile_name %in% c(	"2m_utm19_w_6_61", "2m_utm19_w_5_61", "2m_utm19_w_6_62" , "2m_utm19_w_5_62"))  

mapview(charlevoix)
map(charlevoix$Ftp_dsmCHS,~ download.file(.x, destfile = here("2021_04_Canada_HRDEM/downloads/", basename(.x)) ))


raster1 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_5_61.tif"))
raster2 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_5_62.tif"))
raster3 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_6_61.tif"))
raster4 <- raster::brick(here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_6_62.tif"))

raster::plot(raster1)
raster::plotRGB(raster3)

dim(raster3)

install.packages("terrainr")
library(terrainr)
ggplot() + 
  geom_spatial_rgb(data = here("2021_04_Canada_HRDEM/downloads/colorhillshade_dsm_2m_utm19_w_6_61.tif"),
                   aes(x = x, y = y, r = red, g = green, b = blue))  
