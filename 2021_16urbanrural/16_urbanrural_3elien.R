library(tidyverse)
library(sf)
library(mapview)
library(rdeck)
library(RcppSimdJson)
library(snapbox)

install.packages("anthonynorth/rdeck")
zonage <- read_sf("2021_16urbanrural/data/zonage/zonage_l.shp")
zone_agricole_transpoee <- read_sf("2021_16urbanrural/data/zone_agricole_l.shp")
inclusions <- read_sf("2021_16urbanrural/data/incl_excl.shp")

#La Base de données des parcelles et productions agricoles déclarées (BDPPAD) e
#https://www.fadq.qc.ca/documents/donnees/base-de-donnees-des-parcelles-et-productions-agricoles-declarees/
bdppad <- read_sf("2021_16urbanrural/data/BDPPAD_v03_AN_2021_s_20211021.shp")
bdppad4326 <- bdppad %>% st_transform(4326)

#bdppad_simple <- bdppad %>% rmapshaper::ms_simplify() %>% st_transform()
sortie <- tibble(x= -71.14080, y= 46.79436) %>%
  st_as_sf(coords = c("x", "y"),crs = 4326)


add_buffer_in_meters_and_return_to_original_crs <- function(data, distance){
  current_crs <- sf::st_crs(data)
  meter_crs <- crsuggest::suggest_crs(data, units = "m",  gcs = 4326)  %>% pull(crs_code) %>% head(1) %>% as.numeric()
  data %>% 
    st_transform(crs = meter_crs) %>%
    st_buffer(dist = distance) %>%
    st_transform(crs = current_crs)
}

centre_decale <- tibble(x= -71.111, y= 46.771) %>%
  st_as_sf(coords = c("x", "y"),crs = 4326)


zone_d_interet <- centre_decale %>% add_buffer_in_meters_and_return_to_original_crs(distance=10000)

bppad_chaudiere <-  bdppad4326 
mapview(zone_d_interet)
bdppad2020_4326 <- read_sf("2021_16urbanrural/data/BDPPAD_v03_2020_s_20210429.shp") %>% st_transform(4326)%>% st_crop(zone_d_interet)
ggplot() +
  layer_mapbox(zone_d_interet,
               map_style = mapbox_satellite_streets()) +
  #geom_sf(data = bppad_chaudiere, alpha = 0.3) + 
  geom_sf(data =sortie, size = 10, color = "red")+ 
  labs(
    title = "#30DayMapChallenge Day 16 -Urban/Rural: Proposed exit location of a $10 billion tunnel connecting Lévis to Québec City",
    subtitle ="Proponents claim it won't lead to urban sprawl",
    caption = "base map:mapbox satellite ",
    x = NULL, y = NULL#,
    
  ) + 
  cowplot::theme_map()



png_file <- here::here("2021_16urbanrural/day16_urban_rural.png")

ggsave(
  png_file,
  #  device = cairo_pdf,
  width = 13,
  height = 13
)

knitr::plot_crop(png_file)




simple  <- zone_agricole_transpoee %>% rmapshaper::ms_simplify()
simple4326 <- simple %>% st_transform(4326)
intersected <- simple4326 %>% st_intersection(zone_d_interet)
url <- file.path(
  "https://raw.githubusercontent.com/visgl/deck.gl-data/master",
  "examples/scatterplot/manhattan.json",
  fsep = "/"
)
manhattan_data <- fload(url) %>%
  as_tibble(.name_repair = ~ c("lon", "lat", "species")) %>%
  mutate(
    position = sfc_point(lon, lat),
    species_name = if_else(species == 1, "dog", "cat")
  )

manhattan_data

rdeck(
  map_style = mapbox_dark(),
  # set the bounds of the map to include all of the manhattan data
  initial_bounds = st_bbox(manhattan_data$position),
  # add a 2 pixel buffer to each point, making it easier to hover
  picking_radius = 2
) %>%
  add_scatterplot_layer(
    name = "manhattan_animals",
    data = manhattan_data,
    # the coloumn in manhattan_data which contains the location of each point
    get_position = position,
    # a categorical colour scale, using the species column and a cividis colour palette
    get_fill_color = scale_color_category(
      col = species,
      palette = cividis(2)
    ),
    # the radius of each point (default 1 metre) is scaled by 30
    radius_scale = 30,
    radius_min_pixels = 0.5,
    # highlight dot density
    blending_mode = "additive",
    # interactivity
    pickable = TRUE,
    auto_highlight = TRUE,
    # per-species highlight colour
    highlight_color = scale_color_category(
      col = species,
      palette = c("#0060e6", "#fff399"),
      legend = FALSE
    ),
    tooltip = c(species, species_name)
  )


manhattan_data_grouped <- manhattan_data %>%
  group_by(species_name) %>%
  summarise(
    position = st_union(position),
    count = n(),
    .groups = "drop"
  )


rdeck(
  map_style = mapbox_dark(),
  # set the bounds of the map to include all of the manhattan data
  initial_bounds = st_bbox(manhattan_data_grouped$position),
  # add a 2 pixel buffer to each point, making it easier to hover
  picking_radius = 2
) %>%
  add_scatterplot_layer(
    name = "manhattan_animals",
    data = manhattan_data_grouped,
    # the coloumn in manhattan_data which contains the location of each point
    get_position = position,
    # a categorical colour scale, using the species column and a cividis colour palette
    get_fill_color = scale_color_category(
      col = species_name,
      palette = cividis(2)
    ),
    # the radius of each point (default 1 metre) is scaled by 30
    radius_scale = 30,
    radius_min_pixels = 0.5,
    # highlight dot density
    blending_mode = "additive",
    # interactivity
    pickable = TRUE,
    auto_highlight = TRUE,
    # per-species highlight colour
    highlight_color = scale_color_category(
      col = species_name,
      palette = c("#0060e6", "#fff399"),
      legend = FALSE
    ),
    tooltip = everything()
  )






rdeck(
  map_style = mapbox_dark(),
  # set the bounds of the map to include all of the manhattan data
  initial_bounds = st_bbox(zone_agricole_transpoee),
  # add a 2 pixel buffer to each point, making it easier to hover
  picking_radius = 2
) %>%
  add_line_layer(
    name = "manhattan_animals",
    data = zone_agricole_transpoee,
    # the coloumn in manhattan_data which contains the location of each point
    get_position = geometry,
    # a categorical colour scale, using the species column and a cividis colour palette
    get_fill_color = scale_color_category(
      col = species_name,
      palette = cividis(2)
    ),
    # the radius of each point (default 1 metre) is scaled by 30
    radius_scale = 30,
    radius_min_pixels = 0.5,
    # highlight dot density
    blending_mode = "additive",
    # interactivity
    pickable = TRUE,
    auto_highlight = TRUE,
    # per-species highlight colour
    highlight_color = scale_color_category(
      col = date_maj,
      #palette = c("#0060e6", "#fff399"),
      legend = FALSE
    ),
    tooltip = everything()
  )
