
library("tidyverse") #for data wrangling and ggplot
library("sf") # for spatial data wrangling
library("terra") # new raster package
library("raster") # old raster package, more supported by other packages
library("mapview") # for quick map diagnosis
library("rnaturalearth") # for country limits
library(tidygeocoder) # to geocode address and city nams
library("osmdata")  # to get river and lakes polygons
library("conflicted") # to specify which function to prefer
library("here") 
library(ggspatial) # for layer_spatial ,ggspatial::north_arrow(_nautical) and ggspatial::annotation_scale()
library(colorspace) #  for lightern()
library(plotwidgets) # for rgb2hsl
library(elevatr) # for elevation rasters   
library(ggtext)
library(patchwork) # to combine globe and map using inset_element()
conflict_prefer("plotRGB", "terra")
conflict_prefer("filter", "dplyr")

water_fill <- "#FAFDFF"
water_outline <- "#89A4B2"


globe_blue <- "#12608D"
globe_gray <- "#e5e3df"
# get country limits, province limites, etc..
countries <- ne_countries(scale = 50, returnclass = "sf")
canada <- ne_countries(scale = 50, returnclass = "sf", country = "canada")
provinces <- ne_states(returnclass = "sf", country = "canada")
quebec <- provinces %>% filter(name == "Québec")


add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}


## geocode some cities using tidygeocoder  


point_Mashteuiatsh =   tibble(villes = c("Mashteuiatsh")) %>%
  mutate(municipalite = paste0(villes, " Québec, Canada")) %>%
  geocode (address = municipalite, method = "osm", lat = latitude, long = longitude) %>%
  st_as_sf(coords = c("longitude", "latitude" ), crs = 4326, remove= FALSE)  

point_fort_george <- tibble(villes  = "Fort George", latitude = 53.83, longitude = -78.99) %>%
  st_as_sf(coords = c("longitude", "latitude" ), crs = 4326, remove= FALSE)  

villes <- bind_rows(point_Mashteuiatsh, point_fort_george)



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
globe <- function(lon = 13, lat= 32, col_earth = "#a5bf8b", col_water = "#96b6d8", bg = FALSE) {
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
    ggplot2::geom_sf(data = circle, fill = col_water, alpha = .5) +
    ggplot2::geom_sf(data = sf::st_collection_extract(visible),
                     fill = col_earth, color = NA) +
    
    ggplot2::geom_sf(data = circle, color = "grey60", fill = NA, size = .5) +
    ggplot2::coord_sf(crs = ortho) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid = ggplot2::element_line("grey60", size = .3))
  
  return(globe)
}


if(FALSE){
  
  bb <- st_bbox(quebec)
  
  riviere_peribonka <-  bb %>% opq(timeout = 120) %>%
    add_osm_feature("name" , "Rivière Péribonka") %>%
    osmdata_sf()
  riviere_peribonka.sf <- riviere_peribonka$osm_multipolygons
  
  lac_peribonka <-  bb %>% opq(timeout = 120) %>%
    add_osm_feature("name" , "Lac Péribonka") %>%
    osmdata_sf()
  
  lac_peribonka.sf <- lac_peribonka$osm_multipolygons
  
  lac_st_jean <-  bb %>% opq() %>%
    add_osm_feature("name" , "Lac Saint-Jean") %>%
    osmdata_sf() 
  
  lac_st_jean.sf <- lac_st_jean$osm_multipolygons %>%
    filter(osm_id != 	1596407)
  
  
  passes_dangereuses <- bb %>% opq(timeout = 120) %>%
    add_osm_feature("name", "Passes Dangereuses") %>%
    osmdata_sf()
  
  
  
  passes_dangereuses.sf <- passes_dangereuses$osm_points[1,] %>% 
    mutate(
      longitude= map_dbl( geometry, ~st_coordinates(.x)[1]),
      latitude= map_dbl( geometry, ~st_coordinates(.x)[2])
    ) 
  write_rds(passes_dangereuses.sf, here("2021_08_blue/data/processed/passes_dangereuses.sf.rds"))
  
  area_of_interest <-bind_rows(lac_st_jean.sf, lac_peribonka.sf, riviere_peribonka.sf) 
  write_rds(area_of_interest, here("2021_08_blue/data/processed/area_of_interest.rds"))
  
  
  add_buffer_in_meters_and_return_to_original_crs(.,30000)
  
  area_of_interest_buffered_bbox <- area_of_interest %>% add_buffer_in_meters_and_return_to_original_crs(.,30000) %>% st_bbox()
  
  ## get all  rivers and lakes that fit in my area of interest
  osm_lakes.sf <- 
    opq(bbox = area_of_interest_buffered_bbox) %>%
    add_osm_feature(key = 'water', value = 'lake') %>%
    osmdata_sf()
  osm_lakes.sf <- osm_lakes.sf$osm_multipolygons
  write_rds(osm_lakes.sf, here("2021_08_blue/data/processed/osm_lakes.sf.rds"))
  
  
  osm_rivers.sf <- 
    opq(bbox = area_of_interest_buffered_bbox) %>%
    add_osm_feature(key = 'waterway', value = 'river') %>%
    osmdata_sf()
  osm_rivers.sf <- osm_rivers.sf$osm_lines
  write_rds(osm_rivers.sf, here("2021_08_blue/data/processed/osm_rivers.sf.rds"))
  
  osm_rivers.sf2 <- 
    opq(bbox = area_of_interest_buffered_bbox) %>%
    add_osm_feature(key = 'water', value = 'river') %>%
    osmdata_sf()
  osm_rivers.sf2 <- osm_rivers.sf2$osm_multipolygons
  write_rds(osm_rivers.sf2, here("2021_08_blue/data/processed/osm_rivers.sf2.rds"))
} else {
  area_of_interest <- read_rds(here("2021_08_blue/data/processed/area_of_interest.rds"))
  osm_lakes.sf <- read_rds(here("2021_08_blue/data/processed/osm_lakes.sf.rds"))
  osm_rivers.sf <- read_rds(here("2021_08_blue/data/processed/osm_rivers.sf.rds"))
  osm_rivers.sf2 <- read_rds(here("2021_08_blue/data/processed/osm_rivers.sf2.rds"))
  area_of_interest_buffered_bbox <- area_of_interest %>% add_buffer_in_meters_and_return_to_original_crs(.,30000) %>% st_bbox()
  passes_dangereuses.sf <- read_rds( here("2021_08_blue/data/processed/passes_dangereuses.sf.rds"))
}

extra_water <- bind_rows(osm_lakes.sf, osm_rivers.sf, osm_rivers.sf2)


# pick a projection and project everything ----

#crsuggest::suggest_crs(area_of_interest, gcs =  "4326")
# A tibble: 10 x 6
# crs_code crs_name                        crs_type  crs_gcs crs_units crs_proj4                                                        
# <chr>    <chr>                           <chr>       <dbl> <chr>     <chr>                                                            
#   1 32619    WGS 84 / UTM zone 19N           projected    4326 m         +proj=utm +zone=19 +datum=WGS84 +units=m +no_defs   


area_of_interest_buffered_projected <- area_of_interest %>%
  add_buffer_in_meters_and_return_to_original_crs(.,30000)  %>% 
  st_transform(32619) 

area_of_interest_buffered_bbox_projected <- area_of_interest %>%
  add_buffer_in_meters_and_return_to_original_crs(.,30000)  %>% 
  st_transform(32619) %>%
  st_bbox()

area_of_interest_projected <- area_of_interest %>% st_transform(32619)

extra_water_projected <- extra_water %>% st_transform(32619)


## get elevation for contour lines ----    
dem.raster <- elevatr::get_elev_raster(area_of_interest %>%
                                         add_buffer_in_meters_and_return_to_original_crs(.,30000) %>%
                                         st_transform(32619) %>%
                                         st_bbox() %>%
                                         st_as_sfc() %>%
                                         st_transform(4326),
                                       z= 9, clip = "bbox")

# create slope and hillshade
slope.raster = raster::terrain(dem.raster, opt='slope')
aspect.raster = raster::terrain(dem.raster, opt='aspect')
hill.raster = raster::hillShade(slope.raster, aspect.raster, 40, 270)

dem_spdf <- as(dem.raster, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("elev", "lon", "lat")



hill.raster_projected <- raster::projectRaster(hill.raster, crs = 32619)
hill_spdf <- as(hill.raster_projected, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("hill", "lon", "lat")



## add water lines to lac st-jean  (NOT USED) ----
# here is  my add_water_line() function and add_water_lines() wrapper
add_water_line <- function(data, buffer, lighten=0.1){
  geom_sf(data = data %>% st_buffer(buffer), fill = NA, 
          color = lighten(water_outline, lighten), lwd = 0.35
  ) 
}

add_default_water_lines <- function(data){
  distance_between_pole_of_inacessibility_and_polygon <- polylabelr::poi(data)[[1]]$dist
  
  z <- tibble(data = list(data), 
              buffers = distance_between_pole_of_inacessibility_and_polygon / 30  * seq(from=-1, to = -5),
              lightens = c(0.1, 0.3, 0.5, 0.7, 0.9)
  )
  purrr::pmap(list(data = z$data, buffer = z$buffers, lighten = z$lightens), add_water_line)
}
## load basemap raster (not used looks ugly) ----
# source: download greyscale shaded relief  geotiff base layer by going to  worldview.earthdata.nasa.gov 
# add grayscale shaded relief  to map
# "Take a snapshot" , export to geotiff, 60m/ pixel

#bm <- rast(here("2021_08_blue/data/snapshot-2021-11-08T00_00_00Z.tiff")) # high resolution
# bm <- rast(here("2021_08_blue/data/snapshot-2021-01-15T00_00_00Z.tiff"))  # low res
# bm_projected <-terra::project(bm, "epsg:32619") 
# bm_projected_crop <- terra::crop(bm_projected, 
#                                  area_of_interest %>%
#                                    add_buffer_in_meters_and_return_to_original_crs(.,30000)  %>% 
#                                    st_transform(32619))

# # function by Dr Royé to change saturation from RGB
# saturation <- function(rgb, s = .5){
#   
#   hsl <- plotwidgets::rgb2hsl(as.matrix(rgb))
#   hsl[2, ] <- s
#   
#   rgb_new <- as.vector(t(hsl2rgb(hsl)))
#   
#   return(rgb_new)
#   
# }
# # apply the function to unsaturate with 5%
# bm_projected_crop_desat <- app(bm_projected_crop, saturation, s = .05)



### ggplot eau   

villes_projected_32619 <- villes %>%  
  st_transform(32619)  %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])


passes_dangereuses.sf_projected_32619 <- passes_dangereuses.sf %>%
  st_transform(32619)  %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])


title_location <- tibble(lat = 50.5, lon = -72.5) %>% 
  st_as_sf(coords= c("lon", "lat"), crs = 4326) %>% 
  st_transform(32619)  %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])


caption_location <- tibble(lat = 48.2, lon = -71.1) %>% 
  st_as_sf(coords= c("lon", "lat"), crs = 4326) %>% 
  st_transform(32619)  %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])




plot_peribonka <- ggplot() +
  geom_raster(data = hill_spdf, aes(lon, lat, fill = hill), alpha = 0.45) +
  scale_fill_gradientn(colours = scico::scico(11, palette = "oslo"), guide = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = extra_water_projected, fill = water_fill, colour = water_outline)+
  geom_sf(data = area_of_interest_projected ,fill =  water_fill, color =  water_outline  ) +
  add_default_water_lines(area_of_interest %>% filter(name == "Lac Saint-Jean") %>% st_transform(32619))+ 
  geom_sf(data = villes_projected_32619 %>% filter(villes == "Mashteuiatsh")) +
  ggrepel::geom_text_repel(data = villes_projected_32619  %>% filter(villes == "Mashteuiatsh"),
                           aes(x = x, y = y, label = villes),  #
                           fontface = "bold", size = 6, color = "grey15", family = "Roboto"
  ) +
  geom_sf(data = passes_dangereuses.sf_projected_32619)+ 
  ggrepel::geom_text_repel(data = passes_dangereuses.sf_projected_32619 ,
                           aes(x = x, y = y, label = name),  #
                           fontface = "bold", color = "grey15", family = "Roboto",
                           size = 6)  +
  coord_sf( xlim = c(area_of_interest_buffered_bbox_projected$xmin,area_of_interest_buffered_bbox_projected$xmax),
            ylim = c(area_of_interest_buffered_bbox_projected$ymin,area_of_interest_buffered_bbox_projected$ymax),
            crs = 32619,
            expand = FALSE
  )  + 
  annotate("richtext",
           label = "KUKUM",
           x = title_location$x , y = title_location$y,
           family = "Roboto",
           size = 24, col = "grey15",
           label.color = NA, fill = NA,
           hjust = 0,
  )+
  annotate("richtext",
           label = "@coulsim",
           x = caption_location$x , y = caption_location$y,
           family = "Roboto",
           size = 6, col = "grey15",
           label.color = NA, fill = NA,
           hjust = 0,
  )+  
  labs(
    #title = "Kukum",
    x = NULL, y = NULL#,
    # caption = "**Map by @coulsim**. polygons: {osmdata} (rivers and lakes), elevation: {elevatr}"
  ) + 
  
  theme_minimal(base_family = "Roboto") + 
  theme(plot.background = element_rect(color = NA, fill = "white"),
        panel.background = element_rect(color = NA, fill = "white"),
        panel.grid = element_blank(),
        plot.margin = grid::unit(c(t = 10,r = 5, b = 3, l = 5), "mm"),
        
        plot.caption = element_markdown(family = "Roboto", size = 11,
                                        hjust = 0.5, color = "grey85",
                                        margin = margin(t = 20, b = 6)),
        legend.title = element_text(color= "grey85"),
        legend.text = element_text(color= "grey85"),
        legend.position = c(0.92, 0.8)
        
  )

#layer_spatial(data = stack(bm_projected_crop)) + # blue marble background map




## globe 
lat <- 48
lon <- -72
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


villes_projected_ortho <- villes %>%  
  st_transform(ortho)  %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])
z <- globe(lon = -72, lat = 48, bg = FALSE, col_earth = globe_gray, col_water = globe_blue)

plot_globe <- z + 
  #geom_sf(data = area_of_interest_buffered_bbox_projected %>% st_as_sfc(), fill = "white", color = "black") + 
  geom_sf(data = villes_projected_32619 ) + 
  ggrepel::geom_text_repel(data =villes_projected_ortho,
                           aes(x = x, y = y, label = villes
                           ),
                           family = "Roboto"
  ) +
  coord_sf(xlim = c(-3e6, 6e6)) # trouvé au pif..




## combine both 


# https://stackoverflow.com/questions/61809382/how-can-i-put-a-scalebar-and-a-north-arrow-on-the-map-ggplot

fail <-     plot_peribonka + 
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Roboto"
    )
  )+ 
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "Roboto"
  ) +
  inset_element(plot_globe, bottom = 0.5, left = 0, top = 0.9, right = 0.4) 

fail

fail$patches$layout$widths  <- 1
fail$patches$layout$heights <- 1

fail


png_file <- here::here("2021_08_blue/day08_kukum.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)

