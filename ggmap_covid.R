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

msoa_coords2 <- msoa %>% 
  left_join(covid_cases) %>% 
  filter(cases_count > 0) %>% 
  mutate(sample_points = map(.x = geometry, .y = cases_count, .f = ~sf::st_sample(.x, .y)))


test <- msoa_coords2 %>% 
  sf::st_drop_geometry() %>% 
  select(msoa11cd, sample_points) %>% 
  unnest(cols = sample_points)

sf::st_sample(msoa[1,]$geometry, 100)


covid_cases_geo <- covid_cases %>%
  left_join(msoa_coords) %>%
  replace_na(list(cases_count = 0)) %>%
  uncount(cases_count)

register_google(Sys.getenv('GOOGLE_MAPS'))

map <-
  get_map(
    location = 'mirfield, uk',
    zoom = 6,
    maptype = 'satellite',
    color = "bw"
  )

#Local map
ggmap(map, extent = "panel", maprange = TRUE, darken = c(0.5, 'white')) +
  geom_density2d(
    data = covid_cases_geo,
    aes(
      x = lon,
      y = lat
    ),
    bins = 20,
    colour = "dark orange",
    alpha = 0.5
  ) +
  stat_density2d(
    data = covid_cases_geo,
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
  coord_cartesian() +
  stat_bin_hex(
    data = covid_cases_geo,
    aes(
      x = lon,
      y = lat,
    ),
    bins = 40,
    alpha = 0.5
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

