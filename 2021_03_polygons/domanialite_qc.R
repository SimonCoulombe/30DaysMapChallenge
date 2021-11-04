library(dplyr) # data wrangling
library(magrittr) # pipe
library(sf) # spatial wrangling
library(ggplot2) # plots
library(mapview) # quick maps
library(rmapshaper) # for ms_simplify
library(purrr)
library(readr)
library(ggtext) # for element markdown
library(rcartocolor) # for CARTO palettes
library(wesanderson) # for wesanderson palettes
library(rnaturalearth ) # for lakes and rivers
library(colorspace) #  for lightern()
library(polylabelr) # to get pole of inacessibility (point furthest from any edge, to give us an idea of the distance between water lines)

quebec_lambert <- "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


water_fill <- "#FAFDFF"
water_outline <- "#89A4B2"



## TODO : add water lines using Katie Joly code.
# here is  my add_water_line() function and add_water_lines() wrapper
add_water_line <- function(data, buffer, lighten=0.1){
  geom_sf(data = data %>% st_buffer(buffer), fill = NA, 
          color = lighten(water_outline, lighten), lwd = 0.35
  ) 
}

## let's try to add the buffers automatically  
# to do this, i figure out where the pole of inaccessibility is (point furthest away from the polygon)
# then we look at how far the point is from the polygon (here: 8000 meters)
# and add buffer at  1/30, 2/30, 3/30, 4/50 and 5/30 of that distance

add_default_water_lines <- function(data){
  distance_between_pole_of_inacessibility_and_polygon <- polylabelr::poi(data)[[1]]$dist
  
  z <- tibble(data = list(data), 
              buffers = distance_between_pole_of_inacessibility_and_polygon / 30  * seq(from=-1, to = -5),
              lightens = c(0.1, 0.3, 0.5, 0.7, 0.9)
  )
  purrr::pmap(list(data = z$data, buffer = z$buffers, lighten = z$lightens), add_water_line)
}




# data prep from geojson dump (this is ugly but it ends up working..)  
if (FALSE){
  domanialite <- read_sf("DOMANIALITE.geojson") # geojson not included
  
  #Rendre polygones valides après avoir jetés les pires
  domanialite2 <- domanialite[!is.na(st_is_valid(domanialite, reason= TRUE)),]  %>%
    sf::st_make_valid()
  
  liste_type_organ <- domanialite2 %>% st_drop_geometry() %>% count(DE_TYPE_TENR, NM_ORGAN) %>% arrange(n)
  
  
  z <- domanialite2  %>% arrange(DE_TYPE_TENR, NM_ORGAN)
  zzz <- z %>% mutate(temp = purrr::map_chr(geometry, ~class(.x)[2]))
  zzzz <- zzz %>% 
    filter(temp %in% c("POLYGON", "MULTIPOLYGON")) %>%
    st_cast("POLYGON") %>%
    mutate(group = ceiling(row_number() / 1e4))
  
  z5 <- zzzz %>% group_nest(group)
  z6 <- purrr::map(z5$data, ~ rmapshaper::ms_simplify(.x, keep =0.01)) 
  z7 <- purrr::map(z6,~ .x %>%  st_cast("POLYGON"))
  z8 <- sf::st_as_sf(data.table::rbindlist(z7))
  z9 <- z8 %>% st_make_valid()
  z10 <- z9 %>% st_transform(crs = quebec_lambert)
  
  
  z11_rncan <- z10 %>% filter(NM_ORGAN == "Ministère de l'Énergie et des Ressources naturelles",  DE_TYPE_TENR == "Publique") %>% st_buffer(200)  %>% st_union()
  z11_prive <- z10 %>% filter(is.na(NM_ORGAN) ,  DE_TYPE_TENR == "Privée") %>% st_buffer(200)   %>% st_union()
  z11_indeterminee <- z10 %>% filter(is.na(NM_ORGAN) ,  DE_TYPE_TENR == "Indéterminée") %>% st_buffer(200)   %>% st_union()
  z11_everything_else <- z10 %>% 
    filter(
      !( NM_ORGAN == "Ministère de l'Énergie et des Ressources naturelles" &  DE_TYPE_TENR == "Publique") &
        !(is.na(NM_ORGAN)  &  DE_TYPE_TENR == "Privée") &
        !(is.na(NM_ORGAN) &  DE_TYPE_TENR == "Indéterminée")
    ) %>% 
    st_buffer(200) %>%
    group_nest(NM_ORGAN, DE_TYPE_TENR) 
  z12 <- z11_everything_else %>% mutate(temp = map(data, st_union)    )
  
  z13_rncan <- tibble( NM_ORGAN ="Ministère de l'Énergie et des Ressources naturelles",  DE_TYPE_TENR = "Publique", geometry = z11_rncan) %>%  # take a tibble, add geometry column, convert to sf
    st_as_sf(sf_column_name = "geometry") %>%
    st_cast("MULTIPOLYGON")
  
  z13_prive <- tibble(NM_ORGAN = NA_character_ ,  DE_TYPE_TENR = "Privée", geometry = z11_prive) %>%  # take a tibble, add geometry column, convert to sf
    st_as_sf(sf_column_name = "geometry") %>%
    st_cast("MULTIPOLYGON")
  
  z13_indetermine <- tibble(NM_ORGAN = NA_character_ ,  DE_TYPE_TENR = "Indéterminée", geometry = z11_indeterminee) %>%  # take a tibble, add geometry column, convert to sf
    st_as_sf(sf_column_name = "geometry") %>%
    st_cast("MULTIPOLYGON")
  
  z13_autres <-   # take a tibble, add geometry column, convert to sf
    pmap(list(z12$NM_ORGAN, z12$DE_TYPE_TENR, z12$temp),
         function(NM_ORGAN, DE_TYPE_TENR, temp){
           tibble(NM_ORGAN = NM_ORGAN ,  DE_TYPE_TENR = DE_TYPE_TENR, geometry = temp) %>% 
             st_as_sf(sf_column_name = "geometry") %>%
             st_cast("MULTIPOLYGON")
         }                          
    )
  
  
  z14 <- sf::st_as_sf(data.table::rbindlist( c(z13_autres, list(z13_indetermine,z13_prive,z13_rncan)))) #  append everythinh 
  write_rds(z14, here::here("2021_03_polygons/data/z14.rds")) # this is the final data.. ugh..
}

z14 <- read_rds(here::here("2021_03_polygons/data/z14.rds"))

bbox_quebec_lambert_wrong <- st_bbox(z14) ## hey that bounding box isnt right..




z14 <-  z14 %>% head(100) ## but somehow passing it through the HEAD function fixes it.
bbox_quebec_lambert <- st_bbox(z14) 

# quebec silhouette (used for cropping)
quebec_shape <- osmdata::getbb("Quebec, Canada", format_out = "sf_polygon") 
quebec_shape <-  bb[1,] # garder le polygone de la province de québec


#lakes
lakes10 <- ne_download(scale = 10, type = 'lakes', category = 'physical', returnclass = "sf") %>% st_make_valid()


#rivers
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")






# coastlines 
coastline10 <- ne_download(scale = 10, type = 'coastline', category = 'physical', returnclass = "sf")

# ocean 
ocean10 <- ne_download(scale = 110, type = 'ocean', category = 'physical', returnclass = "sf") 


# 
# # rivers osmdata (maybe more rivers? )
# osm_rivers.sf <- 
#   opq(bbox = st_bbox(quebec_shape), timeout = 120) %>%
#   add_osm_feature(key = 'waterway', value = 'river') %>%
#   osmdata_sf()
# osm_rivers.sf <- osm_rivers.sf$osm_lines ## turns out que c'est bcp trop.. :|
# 

z15 <- z14 %>% st_difference(ocean10 %>% st_transform(quebec_lambert) %>% st_make_valid()) ## remove ocean from the map.


# get location of titlein the top right for title annotation
tibble(lon = -70.66, lat = 61.86) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(quebec_lambert)
#(-114240.1 1982448)

tibble(lon = -63, lat = 60) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(quebec_lambert)
# 306597.9 1784771)

tibble(lon = -68.6, lat = 46.8) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(quebec_lambert)
#(-7622.156 311619)
ggplot() +
  geom_sf(data = z15 %>% 
            mutate(domaine = factor(DE_TYPE_TENR, levels = c("Publique", "Mixte", "Privée", "Indéterminée", "Non illustrée")) %>% 
                     forcats::fct_collapse(Autre = c("Indéterminée", "Non illustrée") )
            ),
          aes(fill = domaine), color = NA)+ 
  geom_sf(data = rivers10 %>%  st_intersection(quebec_shape)  %>% st_transform(crs = quebec_lambert), inherit.aes = FALSE, color = darken("#89A4B2", 0.3) )+
  geom_sf(data = lakes10 %>% st_intersection(quebec_shape)  %>% st_transform(crs = quebec_lambert),
          inherit.aes = FALSE, color = darken("#89A4B2", 0.3), fill = water_fill ) +
 # geom_sf(data = ocean10 %>% st_intersection(quebec_shape)  %>% st_transform(crs = quebec_lambert), inherit.aes = FALSE, color = darken("#89A4B2", 0.3) )+ 
  coord_sf(
    crs = quebec_lambert,
    xlim = c(bbox_quebec_lambert["xmin"], bbox_quebec_lambert["xmax"]),
    ylim = c(bbox_quebec_lambert["ymin"], bbox_quebec_lambert["ymax"])
  ) + 
  
  labs(
    fill = "Domanialité",
    caption = "Data: Gouvernement du Québec " ,
    x = NULL, y = NULL)  +
  # TITLE + key facts
  # TITLE + key facts
  # annotate code by _ansgar https://github.com/bydata/30DayMapChallenge-2021/blob/main/R/day02-lines.R
  annotate("richtext",
           label = "You can camp for free on most public<br>land, but there is no map of<br>where it is allowed",
           x = -114240.1 , y = 1982448,
           family = "Roboto",
           size = 7, col = "grey35",
           label.color = NA, fill = NA,
           hjust = 0,
  )  +
  # annotate("richtext",
  #          label = "https://www.quebec.ca/tourisme-et-loisirs/activites-sportives-et-de-plein-air/activites-permises-territoire-public",
  #          x = -7622.156 , y = 311619,
  #          family = "Roboto",
  #          size = 6, col = "grey35",
  #          label.color = NA, fill = NA,
  #          hjust = 0,
  # )  +
  # rcartocolor::scale_fill_carto_d()+
  scale_fill_manual(values = wes_palette("Royal1"))+
  theme_minimal(base_family = "Roboto") + 
  theme(plot.background = element_rect(color = NA, fill = "black"),
        panel.background = element_rect(color = NA, fill = "black"),
        panel.grid = element_blank(),
        plot.margin = grid::unit(c(t = 10,r = 5, b = 3, l = 5), "mm"),
        
        plot.caption = element_markdown(family = "Roboto", size = 11,
                                        hjust = 0.5, color = "grey35",
                                        margin = margin(t = 20, b = 6)),
        legend.title = element_text(color= "grey35"),
        legend.text = element_text(color= "grey35"),
        legend.position = c(0.82, 0.6)
  )
# site web qui dit qu'Aucune carte existe:https://www.quebec.ca/tourisme-et-loisirs/activites-sportives-et-de-plein-air/activites-permises-territoire-public<br> 

# use knitr to remove white borders around plots..
# https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/
png_file <- here::here("2021_03_polygons/day03_polygons_domanialite.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)


