library(stringr)
library(tidyverse)
library(sf)
library(mapview)
results <- read_csv("2021_26_choropleth/resultats-detailles-2021.csv")
bureaux  <- read_sf("2021_26_choropleth/bureaux-vote-2021.shp")
sections <- read_sf("2021_26_choropleth/section-vote-2021.shp")
bassins <- read_sf("2021_26_choropleth/bassins-electoraux-2021.shp")
districts <- read_sf("2021_26_choropleth/districts-electoraux-2021.shp")
z <- results %>% mutate(
  NOM_SECTIO = paste0(stringr::str_pad(DistrictID, side = "left", pad = "0", width = 3) ,
                      "-",
                      stringr::str_pad(Bureau, side = "left", pad = "0", width = 3) )
) %>%
  filter(Poste ==0) %>% # mairie
  filter(Bureau <= 108)  %>% # vote en bureau
  mutate(pct = 100* Votes / TotalValidVotes) 

poll_winners <- z %>% 
  group_by(NOM_SECTIO ) %>%
  slice_max(pct) %>%
  select(NOM_SECTIO, winner= Candidat, pct_winner = pct )


zz <- z %>% 
  select(NOM_SECTIO, Candidat,  TotalValidVotes, TotalRejectedVotes, TotalVotes, pct ) %>%
  pivot_wider(names_from=Candidat, values_from = pct, names_prefix = "pct_") %>%
  left_join(poll_winners) %>% 
  janitor::clean_names() 


map_data <- sections %>%  
  janitor::clean_names() %>%
  inner_join(zz)   %>% 
  mutate(fct_winner = factor(winner),
         winner2 = case_when(
           winner == "PLANTE Valérie" ~ "Valérie Plante",
           winner == "CODERRE Denis" ~ "Denis Coderre",
           winner == "HOLNESS Balarama" ~ "Balarama Holness",
           is.na(winner)  ~ "No vote",
           TRUE ~ "Autre"
           
         ),
         fct_winner2 = factor(winner2, levels = c("Valérie Plante", "Denis Coderre", "Balarama Holness", "Autre", "No vote")),
         fct_winner = fct_explicit_na(fct_winner, na_level = "No winner")
  )%>% 
  
  st_cast( "MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(my_opacity = floor(pmin(pct_winner *3, 200))) %>%
  mutate(my_opacity = if_else(total_valid_votes ==0, 200, my_opacity))


library(mapdeck)
MAPBOX <- Sys.getenv("mapbox")



colors <- tibble(
  winner2 = c("Valérie Plante", "Denis Coderre", "Balarama Holness", "Autre", "No vote"),
  colour = c("#FFC0CB", "#684c2a",  "#003049", "#FFA500", "#00FF00")
)

l1 <- legend_element(
  variables = colors$winner2,
  colours = colors$colour,
  colour_type = "fill",
  variable_type = "category",
  title = "Gagnant"
)

js <- mapdeck_legend(l1)

map_data_plus <- map_data %>% 
  left_join(colors) %>%
  mutate(colour = paste0(colour,as.hexmode(my_opacity))) %>%
  mutate(my_tooltip = paste0(nom_sectio, ": ",winner, " (", round(pct_winner,1), " % de ", total_valid_votes, " votes)"))  %>%
  st_transform(4326)

mymap <- mapdeck(token = MAPBOX, 
                 style = mapdeck_style('dark'))  %>% #  # 
  add_polygon(data = map_data_plus   ,
              fill_colour = "colour",
              legend = js,
              auto_highlight = TRUE,
              highlight_colour = "#FFFFFFAA",
              tooltip = "my_tooltip"
              
              
  )

widgetframe::frameWidget(mymap, height =1000)
