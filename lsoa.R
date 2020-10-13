library(tidyverse)
library(sf)

week_numbers <- read_csv("C:/Users/neild/Documents/dev/covid19_mapping/lsoa_csv/week_numbers.csv")

cases <- read_csv('https://coronavirus.data.gov.uk/downloads/lsoa_data/LSOAs_latest.csv', na = '-99', col_types = cols(.default = "c")) %>% 
  pivot_longer(cols = contains('wk'), names_to = 'week_number', values_to = 'cases_count') %>% 
  mutate(cases_count = as.integer(cases_count)) %>% 
  left_join(week_numbers) %>% 
  rename(LSOA11CD = lsoa11_cd)

lsoa <-
  sf::st_read("geo/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BSC_v2.shp") %>%
  sf::st_transform(4326)

lsoa_shapes <- lsoa %>%
  left_join(cases) %>% 
  replace_na(list(cases_count = 0))
#  filter(cases_count > 0)

lsoa_centroids <- lsoa_shapes %>%
  sf::st_centroid(msoa)

lsoa_coords_sampled <- lsoa_shapes %>%
  filter(cases_count > 0) %>% 
  filter(week_ending >= '2020-08-01') %>% 
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
  select(LSOA11CD, sample_points, week_ending) %>% 
  unnest(cols = sample_points)

lsoa_coords_sampled$lon <- lsoa_coords_sampled$sample_points[,1]
lsoa_coords_sampled$lat <- lsoa_coords_sampled$sample_points[,2]

write_rds(lsoa_coords_sampled, "lsoa_sampled.RDS")


#-------------------------------------------------------------------------------

library(leaflet)
library(leaflet.extras)

pal_fun <- leaflet::colorNumeric("YlOrRd", c(0, max(lsoa_shapes$cases_count)), n = 5)

map <- lsoa_coords_sampled %>%
  leaflet() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>% 
                   setView(lat = 53.6, lng = -1.98, zoom = 10) %>% 
    addHeatmap(layerId = 'heatmap', minOpacity = 0.25, max = 100, radius = 20)

map

htmlwidgets::saveWidget(map, "lsoa_map.html")
