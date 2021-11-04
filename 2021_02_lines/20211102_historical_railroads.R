library(here) # to make navigating file structure easier
library(magrittr)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) # for canada shapefile
library(ggmap) # for ggmap()
library(ggtext) # for element markdown


#  required for base map downloads
ggmap::register_google(key = Sys.getenv("googlemap_api_key"),
                       account_type="premium")



# get a shapefile of canada because we want to get bounding box 
canada <-  ne_countries(returnclass = "sf", scale = "medium", country = c("canada"))
as.numeric(st_bbox(canada))
#[1] -141.00215   41.67485  -52.65366   83.11611

#let's cut the northern third to make the map more readable'
mybox <- ggmap::make_bbox(lon = c(-141, -53), lat = c(41,63))


# get canada basemap from google satellite

# https://slcladal.github.io/maps.html
# load library
# define box
# sbbox <- make_bbox(lon = c(115, 155), lat = c(-12.5, -42), f = .1)
# # get map
# ausbg = get_map(location=sbbox, zoom=4,
#                 
#                 # possible sources
#                 source = "osm",
#                 #source = "google",
#                 #source = "stamen",
#                 
#                 # possible coloring
#                 #color = "bw",
#                 color = "color",
#                 
#                 # possible maptypes
#                 maptype="satellite")
# #maptype="terrain")
# #maptype="terrain-background")
# #maptype="hybrid")
# #maptype="toner")
# #maptype="hybrid")
# #maptype="terrain-labels")
# #maptype="roadmap")
# # create map
# ausbg = ggmap(ausbg)



# get the basemap

raw_map <- get_map(
  location = mybox,
  #location = as.numeric(st_bbox(canada)),
  zoom=5, 
  color = "bw",
  maptype = 'satellite', 
  source = 'google'
)



# there are issues with ggmap +geom_sf because there is some issue with the CRS of the bbox
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster/50844502#50844502
# Applying this function fixes it: 
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(raw_map)

# historical rails 
#https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/UCCFVQ

#
#EPSG:2138
#NAD27(CGQ77) / Quebec Lambert
rail <- read_sf(here::here("2021_02_lines/data/HR_rails_new/HR_rails_NEW.shp")) %>% 
  st_transform(crs = 3857) ## 3857 to match ggmap


# function based on ggmap_bbox() I built to get the bbox of the ggmap map to use it to create a big black rectangle to hide the area outside Canada..
get_ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  st_bbox(map_bbox)
  
} 


# create big black square, but cut out canada using st_difference()
my_big_square <- st_as_sfc(get_ggmap_bbox(map))  %>% st_set_crs(3857)
not_canada <- my_big_square %>% st_difference(canada %>% st_transform(3857))


# create year_built to have 5 groups of year built
rails_plus <- rail %>% 
  filter(CNSTRCTD >= 1500) %>%
  mutate(year_built =
           factor(case_when(CNSTRCTD < 1881 ~ "Before 1881",
                            CNSTRCTD %in% c(1881, 1882, 1883, 1884, 1885) ~ "1881-1885",
                            CNSTRCTD <= 1919 ~ "1886-1919",
                            CNSTRCTD <= 1949 ~ "1920-1949",
                            CNSTRCTD > 1949 ~ "1950 - Today",
                            
           ),
           levels = c("Before 1881", "1881-1885", "1886-1919", "1920-1949", "1950 - Today")
           )
  )


# get location of title in the USA (bottom left) in crs 3857 for title annotation
tibble(lon = -140, lat = 45) %>% st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4236) %>% st_transform(3857)

#(-15584713 5620347)


ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = not_canada, fill = "black", inherit.aes = FALSE)+
  #   I decided to add each color in order because it allows the  values added last (older) to look better.
  # geom_sf(data =rails_plus  ,
  #         aes(color = year_built, fill = year_built), size = 0.6,
  #         inherit.aes = FALSE) +
  
  geom_sf(data =rails_plus %>% filter(year_built == "1950 - Today") ,
          aes(color = year_built, fill = year_built), size = 0.6,
          inherit.aes = FALSE) +
  geom_sf(data =rails_plus %>% filter(year_built == "1920-1949") ,
          aes(color = year_built, fill = year_built), size = 0.6,
          inherit.aes = FALSE) +
  geom_sf(data =rails_plus %>% filter(year_built == "1886-1919") ,
          aes(color = year_built, fill = year_built), size = 0.6,
          inherit.aes = FALSE) +
  geom_sf(data =rails_plus %>% filter(year_built == "1881-1885") ,
          aes(color = year_built, fill = year_built), size = 1.2,
          inherit.aes = FALSE) +
  geom_sf(data =rails_plus %>% filter(year_built == "Before 1881") ,
          aes(color = year_built, fill = year_built), size = 0.6,
          inherit.aes = FALSE) +
  scale_color_viridis_d(drop = FALSE) +  # drop = FALSE forces legend to retain all values even when not present
  scale_fill_viridis_d(drop = FALSE) + 
  
  labs(
    fill = "Year built",
    color = "Year built",
    caption = "Data: **Historical Canadian Railroads (University of Toronto)**, **Basemap: google satellite** <br> Map by @coulsim, lots of theme code reused from @_ansgar line map" ,
    x = NULL, y = NULL)  +
  # TITLE + key facts
  # TITLE + key facts
  # annotate code by _ansgar https://github.com/bydata/30DayMapChallenge-2021/blob/main/R/day02-lines.R
  annotate("richtext",
           label = "The Canadian Pacific Railway connecting <br>British-Columbia to Eastern Canada <br>was built between 1881 and 1885",
           x = -15584713 , y = 5620347,
           family = "Roboto",
           size = 8, col = "grey35",
           label.color = NA, fill = NA,
           hjust = 0,
  )  + 
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
        legend.position = c(0.92, 0.8)

  )




# use knitr to remove white borders around plots..
# https://www.pmassicotte.com/post/removing-borders-around-ggplot2-graphs/
png_file <- here::here("2021_02_lines/day02_lines.png")

ggsave(
  png_file,
#  device = cairo_pdf,
  width = 13,
  height = 6
)

knitr::plot_crop(png_file)
