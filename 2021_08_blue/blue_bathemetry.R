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
if(!require("gdalUtils")) install.packages("gdalUtils")
if(!require("cmocean")) install.packages("cmocean")
if(!require("scico")) install.packages("scico")
library(ggtext) # for element markdown

library(conflicted)
conflict_prefer("plotRGB", "terra")
conflict_prefer("filter", "dplyr")



#data source : https://visibleearth.nasa.gov/images/73963/bathymetry/83058l

#how to merge the rasters : #https://stackoverflow.com/questions/50234139/using-mosaic-in-r-for-merge-multiple-geotiff
if(FALSE){
  my_tif_files <- list.files(here("2021_08_blue/data/"), full.names = TRUE)
  
  
  
  gdalbuildvrt(gdalfile = my_tif_files, # uses all tiffs in the current folder
               output.vrt = "dem.vrt")
  
  gdal_translate(src_dataset = "dem.vrt", 
                 dst_dataset = "dem.tif", 
                 output_Raster = TRUE , # returns the raster as Raster*Object
                 # if TRUE, you should consider to assign 
                 # the whole function to an object like dem <- gddal_tr..
                 options = c("BIGTIFF=YES", "COMPRESSION=LZW"))
}
rast <- terra::rast(here("2021_08_blue/data/dem.tif"))


provinces <- ne_states(returnclass = "sf", country = "canada") 

nfl <- provinces %>% filter(name == "Newfoundland and Labrador")
## I toyed about adding a 100 km buffer around the provinces, but didnt do it.
add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}
nfl_100km_buffer <- add_buffer_in_meters_and_return_to_original_crs(nfl,100000)


rast_nfl <- terra::crop(rast, nfl_100km_buffer)
plot(rast_nfl)
# ouin c'est weird.


if (FALSE){
  small_rast <- terra::disagg(rast, fact= 4)
  terra::writeRaster(small_rast, "small_rast.tif")
  
}


