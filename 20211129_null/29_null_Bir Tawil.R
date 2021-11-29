library(tidyverse)
library(sf)
library(rnaturalearth)
library(osmdata)
library(mapview)
library(elevatr)
library(snapbox)
library(ggtext)
boundaries <- read_sf("2021_22_boundaries/ne_10m_admin_0_disputed_areas.shp")
mapview(boundaries)
countries <- ne_countries(returnclass = "sf", scale = 110)
egypt_sudan <- countries %>% filter(name %in% c( "Sudan", "Egypt"))





# id de bir tawil trouvé là
bir_tawil <- opq_osm_id(type ="relation", id = 3335661) %>%
  opq_string() %>%
  osmdata_sf()
bir_tawil <- bir_tawil$osm_multipolygons
# alternativement ça ça marchait 

# z2 <-  st_bbox(egypt_sudan) %>% 
#   opq(timeout = 120) %>%
#   add_osm_feature("boundary" , "political") %>%
#   osmdata_sf() 
# bir_tawil2 <- z2$osm_multipolygons %>% .[1,]





add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}

# 
# dem.raster <- elevatr::get_elev_raster(bir_tawil %>%
#                                          add_buffer_in_meters_and_return_to_original_crs(.,100000),
#                                        z= 9, clip = "bbox")


bir_tawil_32636 <- bir_tawil %>% st_transform(32636)

annotations 

centroid_location <- st_centroid(bir_tawil_32636)  %>% 
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry %>% 
  select(x,y ) 

label_egypt <- centroid_location %>% mutate(label = "Egypt", y = y + 50000)
label_sudan <- centroid_location %>% mutate(label = "Sudan", y = y - 50000)
label_bir_tawil <- centroid_location %>% mutate(label = "Bir Tawil")
my_labels <- bind_rows(label_egypt, label_sudan, label_bir_tawil)

area_of_interest <- bir_tawil %>%add_buffer_in_meters_and_return_to_original_crs(.,150000) %>% st_transform(32636)
bbox_area_of_interest = st_bbox(area_of_interest)
main_plot <-ggplot() +
  layer_mapbox(area_of_interest, 
               scale_ratio = 0.5, 
               map_style = stylebox::mapbox_satellite()) +
  geom_sf(data = countries, color = "white", fill = NA) +
  geom_sf(data = bir_tawil_32636, color = "white", fill = NA) + 
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
  coord_sf(crs = 32636, expand = FALSE, xlim = c(bbox_area_of_interest$xmin, bbox_area_of_interest$xmax), ylim = c(bbox_area_of_interest$ymin, bbox_area_of_interest$ymax) )+ 
  annotate("richtext",
           label = my_labels$label,
           x = my_labels$x , y = my_labels$y,
           family = "Roboto",
           size = 10, col = "white",
           label.color = NA, fill = NA,
           hjust = 0.5,
  )+  
  labs(
    Title = "Day 29: Null  (Terra Nullius)",
    subtitle= "Bir Tawil is a 2,060 km² area of land along the border between Egypt and Sudan, which is uninhabited and claimed by neither country",
    x = NULL, y = NULL#,
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


plot_globe <- globe(lon = 20, lat = 20, bg = TRUE, col_earth = "white", col_water = "gray95") +
  geom_sf(data = countries,  fill =NA, color= "gray90") +
  geom_sf(data = bir_tawil, fill ="red", color = "red")


full_plot <- main_plot +
  patchwork::inset_element(plot_globe, bottom = 0.03, left = 0.6, top = 0.4, right = 0.97) 

full_plot

png_file <- here::here("20211129_null//day29_null.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)



