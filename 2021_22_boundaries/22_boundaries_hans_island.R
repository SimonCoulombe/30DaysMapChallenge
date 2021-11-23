library(mapview)
library(osmdata)

library(stars) # not useds
library(ggspatial)
library(snapbox)

library(rnaturalearth)
library(raytrix)
library(rayshader)
library(crsuggest)
#devtools::install_github("h-a-graham/raytrix")
library(raytrix)
library(here)
library(terra)
library(raster)
library(sf)
library(tidyverse)
library(elevatr)
library(ggtext)
hans_island <- osmdata::getbb("Hans Island", format_out = "sf_polygon")  %>% .[2,]

# recommended crs : 5922, mais le DEM tif vient en https://epsg.io/3413 et çaa l'air cool
#suggest_crs(hans_island, type= "projected", gcs = 4326, units = "m" )
my_crs <- 3413

hans_island <- hans_island %>% st_transform(my_crs)

add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, type = "projected", units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>%
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}




if(FALSE){ # get Lidar HRDEM  NEVER MIND THEY DONT HAVE HANS ISLAND
  
  ## first get the dataset footprints
  # https://open.canada.ca/data/en/dataset/957782bf-847c-4644-a757-e383c0057995/resource/1f54365a-cdc2-42a2-b22c-b1aae563a5f9
  
  footprints <- read_sf(here::here("2021_11_3d_parc_national_de_la_gaspesie/data/Datasets_Footprints/Datasets_Footprints.shp")) %>%
    st_transform(4326)
  mapview(footprints %>% st_crop(st_bbox(hans_island %>% add_buffer_in_meters_and_return_to_original_crs(., 10000))))
  
  
  # on veut Tile_name 	1m_utm19_w_11_130  et 	1m_utm19_w_10_130 
  footprints_hans <- footprints %>% filter(Project == "31_37", Tile_name %in% c("2m_polarstereo_31_37_1_2"))
  
  
  # download tiles 
  map(footprints_hans$Ftp_dsm,~ download.file(.x, destfile = here::here("2021_22_boundaries/", basename(.x)) ))
  
  
  
}

if (FALSE){ # exemple acropole
  # acropole marche encore ..
  acropole <- read_sf(here("2021_11_3d_parc_national_de_la_gaspesie/data/Acropole des draveurs.gpx"), layer = "track_points")  %>%
    select(time, rownum = track_seg_point_id, ele) %>%
    dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                  lon = sf::st_coordinates(.)[,2]) %>%
    mutate(track ="acropole") 
  
  
  
  aller_acropole <- acropole[1:which.max(acropole$lat),]
  
  trail <-aller_acropole %>% 
    group_by(track) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    st_set_crs(4326)
  
  
  raster1 <- terra::rast(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_10_130.tif")) 
  raster2 <- terra::rast(here("2021_11_3d_parc_national_de_la_gaspesie/data/dtm_1m_utm19_w_11_130.tif")) 
  
  
  both_rasters <- terra::merge(raster1, raster2)
  both_rasters_crop <-  terra::crop(both_rasters, trail %>% add_buffer_in_meters_and_return_to_original_crs(., 50) %>% st_transform(st_crs(both_rasters)))
  
  
  
  bryce <- as(both_rasters_crop, "Raster")
  bryce_mat = raster_to_matrix(bryce)
  
  bryce_small <- resize_matrix(bryce_mat, 0.25)
  
  
  bryce_zoom_mat <- bryce_small
  
  base_map = bryce_zoom_mat %>% 
    height_shade() %>%
    add_overlay(sphere_shade(bryce_zoom_mat, texture = "unicorn", colorintensity = 5), alphalayer=0.5)  %>%
    add_shadow(lamb_shade(bryce_zoom_mat), 0) %>%
    add_shadow(ambient_shade(bryce_zoom_mat),0) %>% 
    add_shadow(texture_shade(bryce_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)
  
  plot_map(base_map)
}


if (FALSE){ # essaie avec HRDEM CANADA.. masi c'EsT NA bordel.
  
}

if (FALSE) { #essaie avec elevatr package, mais c'est bizarre
  
  rasterA <- get_elev_raster( hans_island %>%
                                add_buffer_in_meters_and_return_to_original_crs(., 100), 
                              z = 10)
  
  
  hans_mat = raster_to_matrix(rasterA)
  hans_small <- resize_matrix(hans_mat, 0.25)
  hans_zoom_mat <- hans_small
  
  base_map = hans_zoom_mat %>% 
    height_shade() %>%
    add_overlay(sphere_shade(hans_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5)  %>%
    add_shadow(lamb_shade(hans_zoom_mat), 0) %>%
    add_shadow(ambient_shade(hans_zoom_mat),0) %>% 
    add_shadow(texture_shade(hans_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)
  plot_map(base_map)
  
  plot_3d(base_map, heightmap =  hans_zoom_mat, windowsize=c(2400,1600))
}

if(FALSE){ # essaie avec arcticdem 
  
  #https://www.pgc.umn.edu/data/arcticdem/
  arcticdem_footprint <- read_sf("2021_22_boundaries/ArcticDEM_Tile_Index_Rel7.shp")
  mapview(arcticdem_footprint)
  
  arcticdem <- terra::rast(here("2021_22_boundaries/31_37_1_2_2m_v3.0_reg_dem_browse.tif")) 
  both_rasters_crop <-  terra::crop(arcticdem, hans_island %>% add_buffer_in_meters_and_return_to_original_crs(., 40000) %>% st_transform(st_crs(arcticdem)))
  
  
  bryce <- as(both_rasters_crop, "Raster")
  bryce_mat = raster_to_matrix(bryce)
  
  bryce_small <- resize_matrix(bryce_mat, 0.1)
  
  
  bryce_zoom_mat <- bryce_small
  
  base_map = bryce_zoom_mat %>% 
    height_shade() %>%
    add_overlay(sphere_shade(bryce_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5)  %>%
    add_shadow(lamb_shade(bryce_zoom_mat), 0) %>%
    add_shadow(ambient_shade(bryce_zoom_mat),0) %>% 
    add_shadow(texture_shade(bryce_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1) 
  
  plot_map(base_map)
}

## hey pis fuck it je fais mon plot pareil en stars en plus (sauf qu'on commence en TErra pour aggregate)

if(FALSE){
  ## globe
  # https://rdrr.io/github/EcoDynIZW/d6berlin/src/R/globe.R
  #' Plot Globe with Locator Pin for Berlin (while preserving polygons in orthographic view)
  #'
  #' @param bg A Boolean. Should a background be added to the globe?
  #' @param col_water A hex code. Color used for oceans.
  #' @param col_earth A hex code. Color used for continents.
  #'
  #' @return A ggplot object containing a locator globe with pin.
  #'
  #' @examples
  #' \dontrun{
  #' globe()
  #' }
  #'
  #' @importFrom magrittr %>%
  #'
  #' @export
  globe <- function(lon = 13, lat= 32, col_earth = "#a5bf8b", col_water = "#96b6d8", bg = FALSE, alpha =0.5) {
    ## code to preserve orthpgraphic view from this gist:
    ## https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1
    ## via this issue: https://github.com/r-spatial/sf/issues/1050
    
    ## Load country data
    mini_world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
    
    ## Define the orthographic projection ........................................
    lat <- lat
    lon <- lon
    ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                    ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
    
    ## Define the polygon to split what lies within and without your projection ..
    circle <-
      suppressMessages(
        sf::st_point(x = c(0, 0)) %>%
          sf::st_buffer(dist = 6371000) %>%
          sf::st_sfc(crs = ortho)
      )
    ## Project this polygon in lat-lon ...........................................
    circle_longlat <-
      circle %>%
      sf::st_transform(crs = 4326)
    
    ## You must decompose it into a string with ordered longitudes
    ## Then complete the polygon definition to cover the hemisphere ..............
    if(lat != 0) {
      circle_longlat <- sf::st_boundary(circle_longlat)
      
      circle_coords <- sf::st_coordinates(circle_longlat)[, c(1,2)]
      circle_coords <- circle_coords[order(circle_coords[, 1]),]
      circle_coords <- circle_coords[!duplicated(circle_coords),]
      
      ## Rebuild line ............................................................
      circle_longlat <-
        sf::st_linestring(circle_coords) %>%
        sf::st_sfc(crs = 4326)
      
      if(lat > 0) {
        rectangle <- list(rbind(circle_coords,
                                c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                                c(X = 180, Y = 90),
                                c(X = -180, Y = 90),
                                c(X = -180, circle_coords[1, 'Y']),
                                circle_coords[1, c('X','Y')])) %>%
          sf::st_polygon() %>%
          sf::st_sfc(crs = 4326)
      } else {
        rectangle <- list(rbind(circle_coords,
                                c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                                c(X = 180, Y = -90),
                                c(X = -180, Y = -90),
                                c(X = -180, circle_coords[1, 'Y']),
                                circle_coords[1, c('X','Y')])) %>%
          sf::st_polygon() %>%
          sf::st_sfc(crs = 4326)
      }
      
      circle_longlat <- suppressMessages(sf::st_union(
        sf::st_make_valid(circle_longlat),
        sf::st_make_valid(rectangle))
      )
    }
    
    ## A small negative buffer is necessary to avoid polygons still disappearing
    ## in a few pathological cases ...............................................
    ## Comment Cédric: Doesn't work with -.09 anymore, returns empty object.
    ##                 But works also without the buffer, so using 0 here to
    ##                 return the same type of object.
    visible <- suppressMessages(suppressWarnings(
      sf::st_intersection(sf::st_make_valid(mini_world),
                          sf::st_buffer(circle_longlat, 0)) %>%
        sf::st_transform(crs = ortho)
    ))
    
    ## Get reason why polygons are broken ........................................
    broken_reason <- sf::st_is_valid(visible, reason = TRUE)
    
    ## First fix NA's by decomposing them ........................................
    na_visible <- visible[is.na(broken_reason),]
    visible <- visible[!is.na(broken_reason),]
    
    ## Open and close polygons ...................................................
    na_visible <- sf::st_cast(na_visible, 'MULTILINESTRING') %>%
      sf::st_cast('LINESTRING', do_split=TRUE)
    na_visible <- na_visible %>%
      dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE))
    
    ## Exclude polygons with less than 4 points ..................................
    na_visible <- na_visible %>%
      dplyr::filter(npts >= 4) %>%
      dplyr::select(-npts) %>%
      sf::st_cast('POLYGON')
    
    ## Fix other broken polygons .................................................
    broken <- which(!sf::st_is_valid(visible))
    for(land in broken) {
      result = suppressWarnings(tryCatch({
        visible[land,] <-
          sf::st_make_valid(visible[land,]) %>%
          sf::st_collection_extract()
      }, error = function(e) {
        visible[land,] <<- sf::st_buffer(visible[land,], 0)
      }))
    }
    
    ## Bind together the two tables ..............................................
    visible <- suppressMessages(rbind(visible, na_visible))
    
    
    ## Create globe as ggplot ....................................................
    globe <-
      ggplot2::ggplot()
    
    if (isTRUE(bg)) {
      globe <- globe +
        ggplot2::geom_sf(data = circle, fill = "white", color = "transparent")
    }
    
    globe <- globe +
      ggplot2::geom_sf(data = circle, fill = col_water, alpha = alpha) +
      ggplot2::geom_sf(data = sf::st_collection_extract(visible),
                       fill = col_earth, color = NA) +
      
      ggplot2::geom_sf(data = circle, color = "grey60", fill = NA, size = .5) +
      ggplot2::coord_sf(crs = ortho) +
      ggplot2::theme_void() +
      ggplot2::theme(panel.grid = ggplot2::element_line("grey60", size = .3))
    
    return(globe)
  }
  
  
  lat <- 80.82646
  lon <- -66.45409
  ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                  ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
  
  globe_blue <- "#12608D"
  globe_gray <- "#e5e3df"
  
  
  plot_globe <- 
    globe(lon = lon, lat = lat, bg = FALSE, col_earth = globe_gray, col_water = globe_blue, alpha = 1) +
    geom_sf(data = elev_stars %>% st_bbox() %>% st_as_sfc() %>% st_transform(ortho), fill = "red" ) +
    coord_sf(xlim = c(-2e6, 2e6) , ylim = c(-2e6, 2e6))  
    
  
  
  
  
  
  
  
  elev_browse  <- terra::rast(here("2021_22_boundaries/31_37_1_2_2m_v3.0_reg_dem_browse.tif")) # 10m resolution
  elev  <- terra::rast(here("2021_22_boundaries/31_37_1_2_2m_v3.0_reg_dem.tif")) # 2 meter resolution     
  
  elev_small <- aggregate(elev_browse, fact=5, fun=mean)
  elev_stars <- st_as_stars(elev_small) %>% mutate(type = "elev", density = X31_37_1_2_2m_v3.0_reg_dem_browse) %>% select(-X31_37_1_2_2m_v3.0_reg_dem_browse)
  
  elev_stars4326 <- elev_stars %>% st_transform(4326)
  
  z2 <-  st_bbox(elev_stars4326) %>% 
    opq(timeout = 120) %>%
    add_osm_feature("border_type" , "territorial") %>%
    osmdata_sf()
  
  boundary <- z2$osm_lines %>% 
    filter(source == "geobase" | border_status == "dispute") %>%
    st_transform(st_crs(elev_stars)) %>%
    st_crop(elev_stars)
  
  annotations <- tibble(x= c(-67.5, -66), y=c(80.7, 80.6), labels = c("Canada", "Denmark")) %>%
    st_as_sf(coords= c(x= "x", y = "y"), crs = 4326) %>%
    st_transform(st_crs(elev_stars)) %>% 
    dplyr::mutate(x = sf::st_coordinates(.)[,1],
                  y = sf::st_coordinates(.)[,2])
  
  annotate_hans_island <-  hans_island %>% st_transform(st_crs(occ)) %>%
    st_centroid %>% 
    dplyr::mutate(x = sf::st_coordinates(.)[,1],
                  y = sf::st_coordinates(.)[,2]) %>%
    mutate(labels = "Hans Island, Canada")
  
  
  plot_local <- ggplot() + 
    geom_stars(data = elev_stars,aes(fill = density, x= x , y =y )) + 
    scale_fill_gradientn(colours = scico::scico(100, palette = "oslo"), 
                         na.value = "black") +
    #geom_sf(data = hans_island %>% st_transform(st_crs(occ)), alpha = 0.3, color = "white", size =1.2 , fill = NA)+
    #geom_sf(data = hans_island  %>% st_transform(st_crs(occ)), alpha = 0.3, color = "red", size =1,  fill = NA)+
    geom_sf(data = boundary, alpha = 0.3, color = "red", size =1,  fill = NA)+ 
    coord_sf(expand = FALSE) + 
      labs(fill = "altitude", x= NULL, y =NULL)+
    annotate("richtext",
             label = annotations$labels,
             x = annotations$x - 5000 , y = annotations$y + 8000,
             
             family = "Roboto",
             size = 12, col = "grey15",
             label.color = NA, fill = NA,
             hjust = 0,
    ) + 
    annotate("richtext",
             label = annotate_hans_island$labels, alpha = 0.3,
             x = annotate_hans_island$x - 15000 , y = annotate_hans_island$y + 2000,
             
             family = "Roboto",
             size = 12, col = "red",
             label.color = NA, fill = NA,
             hjust = 0,
    )
    
  plot_local + 
      patchwork::inset_element(plot_globe, bottom = .75, left = 0.75, top = 1, right = 1) 
  
  
}


dem_raster <- as(arcticdem, "Raster")
dem_spdf <- as(dem_raster, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("elev", "lon", "lat")

z <- raster::aggregate(dem_raster, 5)
#BlueMarbleNG-TB_2004-12-01_rgb_3600x1800.TIFF

my_bbox <-  dem_raster %>% st_bbox() %>% st_transform(4326)



ggplot() + 
  layer_spatial(data = z) + 
  geom_sf(data = hans_island, alpha = 0.3, color = "white", size =1.2 , fill = NA)+
  geom_sf(data = hans_island, alpha = 0.3, color = "red", size =1,  fill = NA)+
  geom_sf(data = boundary, alpha = 0.3, color = "red", size =1,  fill = NA)+ 
  coord_sf(expand = FALSE) +
  
  
  
  # blue marble https://neo.gsfc.nasa.gov/view.php?datasetId=BlueMarbleNG-TB
  
  if(FALSE){ # essaie arcticdem 32m
    
    arcticdem <- terra::rast(here("2021_22_boundaries/31_37_32m_v3.0_reg_dem.tif")) 
    plot(arcticdem) ## ah non, il est aussi flushé.
  }

ov <- map_drape(res=5, src="wms_arcgis_mapserver_ESRI.WorldImagery_tms",
                alpha = 0.7)


if (FALSE){
  #
  #Attend .. rayshader to tmap se peut.. J’imagine que to ggplot2 aussi alors?
  #https://github.com/h-a-graham/Hugh-30DayMapChallenge
  #https://twitter.com/hughagraham/status/1460610022740746240
  texture_to_brick <- function(.texture, .raster){
    raster::brick(scales::rescale(.texture, to = c(0, 255)),
                  xmn = raster::extent(.raster)[1],
                  xmx = raster::extent(.raster)[2],
                  ymn = raster::extent(.raster)[3],
                  ymx = raster::extent(.raster)[4],
                  crs = raster::crs(raster)
    )
    
  }
}
z <- texture_to_brick(base_map)

if(FALSE){
  # on peut aussi avoir Hans Island dans les disputed areas de Natural earth  
  download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_disputed_areas.zip",
                destfile = here::here("2021_22_boundaries/ne_10m_admin_0_disputed_areas.zip"))
  
  unzip(here::here("2021_22_boundaries/ne_10m_admin_0_disputed_areas.zip"), exdir = "2021_22_boundaries/")
  
  boundaries <- read_sf("2021_22_boundaries/ne_10m_admin_0_disputed_areas.shp")
  
  hans_island2 <- boundaries %>% filter(NAME_SORT == "Hans Island")
}      
## ça marche mais ça retourne trop, mais ça a permis de voir que je veux boundary = "administrative" et border_Type = "territorial
# z <-  hans_island %>%add_buffer_in_meters_and_return_to_original_crs(10000) %>% 
# opq(timeout = 20) %>%
#   add_osm_feature(key = "boundary") %>%
#   osmdata_sf()

pouet <- ne_countries()


my_bbox <-  hans_island %>%add_buffer_in_meters_and_return_to_original_crs(100000)  %>% st_bbox()

z2 <-  hans_island %>%add_buffer_in_meters_and_return_to_original_crs(100000) %>% 
  opq(timeout = 120) %>%
  add_osm_feature("border_type" , "territorial") %>%
  osmdata_sf()

boundary <- z2$osm_lines %>% 
  filter(source == "geobase" | border_status == "dispute") %>% 
  st_crop(hans_island %>%add_buffer_in_meters_and_return_to_original_crs(95000) )

bm <- rast(here("2021_22_boundaries/snapshot-2021-11-19T00_00_00Z.tiff"))
#bm_projected <-terra::project(bm, espg:4326)  # if we specify a proj4, we pass it as a text string to terra::project
#bm_projected <-terra::project(bm, paste0("epsg:", my_crs)) # if you specify a crs number, it has be be passed as espg:3151


# crop  the rater to the projected provinces
#bm_projected_crop <- terra::crop(bm, boundary)



ggplot()+ 
  # layer_mapbox(hans_island %>%add_buffer_in_meters_and_return_to_original_crs(100000), scale_ratio = 0.5,
  #              #mapbox_api_access_token = "pk.eyJ1IjoibW9yZ2x1bSIsImEiOiJjaWVvOHQzamQwaHMwc21rbThkN3Z5OTVxIn0.QgvzvOHp46vl5Ht3JwGNRg",
  #              mapbox_light()
  #              #mapbox_satellite()
  # )  +
  layer_spatial(data = stack(bm)) + # blue marble background map
  geom_sf(data = boundary %>% st_crop(bm) )+ 
  coord_sf(expand = FALSE)


## experience avec le programme de raytrix pour aller chercher image aérienne.

rasterA <- get_elev_raster( hans_island %>%
                              add_buffer_in_meters_and_return_to_original_crs(., 5000), 
                            z = 10)
set_canvas_raster(rasterA)
#download overlay with raytrix
ov <- map_drape(res=5, src="wms_arcgis_mapserver_ESRI.WorldImagery_tms",
                alpha = 0.7)



IoS_mat <- raster_to_matrix(rasterA) %>%
  resize_matrix(., 1)


#  ----- rayshade ------
IoS_Map <- IoS_mat %>%
  sphere_shade(texture='imhof4')%>%
  add_overlay(., ov,rescale_original=T) %>%
  add_shadow(texture_shade(IoS_mat, detail=0.6, contrast = 3, brightness = 6),0.2) %>%
  add_shadow(ray_shade(IoS_mat, sunaltitude = 35, zscale = 3, multicore = T),0.1) %>%
  add_overlay(generate_waterline_overlay(IoS_mat, min = 0.0002, max = 0.1, smooth=2))
