library(tidyverse)
library(sf)
library(glue)

week_numbers <- read_csv("C:/Users/neild/Documents/dev/covid19_mapping/lsoa_csv/week_numbers.csv")

cases <- read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv', na = '-99', col_types = cols(.default = "c")) %>% 
  pivot_longer(cols = c(contains('wk'), contains('latest')), names_to = 'week_number', values_to = 'cases_count') %>% 
  mutate(cases_count = as.integer(cases_count)) %>% 
  rename(msoa11cd = msoa11_cd) %>% 
  filter(msoa11cd != 'xxxxxxxxx')

max_week <- max(as.integer(substr(cases$week_number, nchar(cases$week_number)-1, nchar(cases$week_number))), na.rm = TRUE)

cases <- cases %>% 
  mutate(week_number = ifelse(str_detect(week_number, 'wk'), week_number, glue('wk_{max_week+1}'))) %>% 
  left_join(week_numbers)

write_rds(cases, 'msoa_processed_cases.RDS')

msoa <-
  sf::st_read("geo/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp") %>%
  sf::st_transform(4326)

msoa_shapes <- msoa %>%
  left_join(cases) %>% 
  replace_na(list(cases_count = 0))
#  filter(cases_count > 0)

msoa_coords_sampled <- msoa_shapes %>%
  filter(cases_count > 0) %>% 
  #filter(week_ending >= '2020-08-01') %>% 
  mutate(
    sample_points = map(
      .x = geometry,
      .y = cases_count,
      .f = ~
        sf::st_sample(.x, .y) %>%
        sf::st_coordinates()
    )
  ) %>% 
  sf::st_drop_geometry() %>% 
  select(msoa11cd, sample_points, week_ending) %>% 
  unnest(cols = sample_points)

msoa_coords_sampled$lon <- msoa_coords_sampled$sample_points[,1]
msoa_coords_sampled$lat <- msoa_coords_sampled$sample_points[,2]

write_rds(msoa_coords_sampled, "msoa_sampled.RDS")


#-------------------------------------------------------------------------------

library(leaflet)
library(leaflet.extras)

pal_fun <- leaflet::colorNumeric("YlOrRd", c(0, max(msoa_shapes$cases_count)), n = 5)

map <- msoa_coords_sampled %>%
  leaflet() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom = 6, maxZoom = 11)) %>% 
                   setView(lat = 53.6, lng = -1.98, zoom = 10) %>% 
    addHeatmap(layerId = 'heatmap', minOpacity = 0.25, max = 100, radius = 20)

map

htmlwidgets::saveWidget(map, "lsoa_map.html")
