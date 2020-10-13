library(tidyverse)
library(ggmap)

msoa <-
  sf::st_read("geo/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp") %>%
  sf::st_transform(4326)

msoa_coords <- sf::st_centroid(msoa) %>%
  sf::st_coordinates()

msoa_coords <- tibble(msoa11cd = msoa$msoa11cd,
                      lat = msoa_coords[, 2],
                      lon = msoa_coords[, 1])

msoa_coords_sampled <- msoa %>%
  left_join(covid_cases) %>%
  filter(cases_count > 0) %>%
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
  select(msoa11cd, sample_points) %>% 
  unnest(cols = sample_points)

msoa_coords_sampled$lon <- msoa_coords_sampled$sample_points[,1]
msoa_coords_sampled$lat <- msoa_coords_sampled$sample_points[,2]

register_google(Sys.getenv('GOOGLE_MAPS'))

map <-
  get_map(
    location = 'wigan, uk',
    zoom = 6,
    maptype = 'toner-background',
    color = "bw"
  )

#Local map
ggmap(map, extent = "panel", maprange = TRUE, darken = c(0.5, 'white')) +
  geom_density2d(
    data = msoa_coords_sampled,
    aes(
      x = lon,
      y = lat
    ),
    bins = 20,
    colour = "dark orange",
    alpha = 0.5
  ) +
  stat_density2d(
    data = msoa_coords_sampled,
    aes(
      x = lon,
      y = lat,
      fill = ..level..,
      alpha = ..level..
    ),
    bins = 30,
    geom = 'polygon'
  ) +
  scale_fill_continuous(low = "light green", high = "red", oob = scales::squish) +
  scale_alpha(range = c(0.1, 0.2), guide = FALSE) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    text = element_text(size = 12)
  )



#Local map
ggmap(map, extent = "panel", maprange = TRUE, darken = c(0.5, 'white')) +
  geom_point(
    data = msoa_coords_sampled,
    aes(
      x = lon,
      y = lat
    ),
    alpha = 0.05,
    shape = 16,
    size = 0.5,
    colour = "red"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    text = element_text(size = 12)
  )

library(leaflet)

leaflet(msoa_coords_sampled) %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addCircles(lng = ~lon,
             lat = ~lat,
             radius = 1000,
             stroke = FALSE,
             fillOpacity = 0.05,
             color = "red"
             )
