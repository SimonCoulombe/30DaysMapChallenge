remotes::install_github("dickoa/rhdx")
remotes::install_github("anthonynorth/snapbox")

library(tidyverse)
library(sf)
library(stars)
library(sen2r)
library(rhdx)
library(mapview)
library(units)
library(snapbox)
library(ggthemes)

library(tidygeocoder)

niamey <- tibble(address ="Niamey, Niger") %>% geocode(address = address) %>% st_as_sf(coords = c("long", "lat"), crs = 4326)



##bbox manic5
# bbox = tibble("long" = c(-69.232372 , -68.164887 ), "lat" = c(51.073, 51.737316)) %>% 
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   st_bbox()


#bbox niamey
bbox = tibble("long" = c(1.95 , 2.35 ), "lat" = c(13.3, 13.65)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_bbox()


# bbox_url <- "https://tinyurl.com/yyobbc58"
# read_sf(bbox_url)
# #bbox <- read_sf("2021_09_satellite/bbox.fgb")
ggplot() +
  layer_mapbox(bbox,
               map_style = mapbox_satellite_streets())


# global surface water https://global-surface-water.appspot.com/download
# 10 x 10 degree tile around niamey
download.file("https://storage.googleapis.com/global-surface-water/downloads2020/occurrence/occurrence_0E_20Nv1_3_2020.tif",
              destfile = here::here("2021_09_satellite/occurrence.tif"))


download.file("https://storage.googleapis.com/global-surface-water/downloads2020/occurrence/occurrence_70W_60Nv1_3_2020.tif",
              destfile = here::here("2021_09_satellite/occurrence_quebec.tif"))


# occurent_quebec

if (FALSE){
  occurence <- here::here("2021_09_satellite/occurrence_quebec.tif")  
  occ <- read_stars(occurence) %>%
    st_crop(bbox) %>%
    st_as_stars()
  
  mat <- occ[[1]]
  str(mat)
  
  #The data is of factor type, to easily subset the data, we need to convert the type to numeric. Particularly, selecting pixels with an occurence of 80 will be achieved more efficiently with numeric vectors.
  mat <- as.numeric(as.character(mat))
  mat <- matrix(mat, nrow = dim(occ)[1])
  mat[mat < 80] <- NA
  mat[!is.na(mat)] <- 1L
  occ$water_perm <- mat
  occ
  
  permanent_water <- st_as_sf(occ["water_perm"], merge = TRUE) %>%
    filter(water_perm == 1) %>%
    st_make_valid() %>% 
    st_union()
  
  write_rds(permanent_water, "permanent_water_quebec.rds")
}


# occurence niamey
if(FALSE){
 occ <- read_stars(here::here("2021_09_satellite/occurrence.tif")) %>%
   st_crop(bbox) %>%
   st_as_stars()

mat <- occ[[1]]
str(mat)

#The data is of factor type, to easily subset the data, we need to convert the type to numeric. Particularly, selecting pixels with an occurence of 80 will be achieved more efficiently with numeric vectors.
mat <- as.numeric(as.character(mat))
mat <- matrix(mat, nrow = dim(occ)[1])
mat[mat < 80] <- NA
mat[!is.na(mat)] <- 1L
occ$water_perm <- mat
occ

permanent_water <- st_as_sf(occ["water_perm"], merge = TRUE) %>%
  filter(water_perm == 1) %>%
  st_make_valid() %>% 
  st_union()

write_rds(permanent_water, "permanent_water.rds")
} else {permanent_water <- read_rds("permanent_water.rds")}

# get elevation 

dem.raster <- elevatr::get_elev_raster(bbox %>% st_as_sfc(),z= 9, clip = "bbox")

ggplot() +
  layer_mapbox(bbox,
               map_style = mapbox_satellite_streets())  +
  geom_sf(data = permanent_water, fill = "blue", size = 0.2)


write_scihub_login(username = Sys.getenv("scihub_username"),
                   password = Sys.getenv("scihub_password"))


out_dir <- "./data/raster/s2out"
safe_dir <- "./data/raster/s2safe"

if (!dir.exists(out_dir)){
  dir.create(out_dir, recursive = TRUE)
}

if (!dir.exists(safe_dir)){
  dir.create(safe_dir, recursive = TRUE)
}



out_path <- sen2r(
  gui = FALSE,
  step_atmcorr = "l2a",
  extent = tibble("long" = c(1.95 , 2.35 ), "lat" = c(13.3, 13.65)) %>% 
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    st_bbox() %>% 
    st_as_sfc() ,
  extent_name = "Niamey",
  timewindow = c(as.Date("2020-09-10"), as.Date("2020-09-15")),
  list_prods = "BOA",
  list_indices = "NDWI2",
  list_rgb = "RGB432B",
  path_l1c = safe_dir,
  path_l2a = safe_dir,
  path_out = out_dir,
  parallel = 8,
  overwrite = TRUE
)
