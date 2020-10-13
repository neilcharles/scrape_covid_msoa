library(tidyverse)
library(jsonlite)
library(glue)
library(rvest)
library(ggplot2)

source('functions/push_to_bigquery.R')

mapview_url <- xml2::read_html('https://www.arcgis.com/sharing/rest/content/items/47574f7a6e454dc6a42c5f6912ed7076/data') %>% 
  html_text() %>% 
    str_extract('(?<="mapItemId":").*(?=","proxyItems")')


data_url <- xml2::read_html(glue('https://www.arcgis.com/sharing/rest/content/items/{mapview_url}/data')) %>% 
  html_text() %>% 
  str_extract('https://services1.arcgis.com.*MSOA.*FeatureServer/0')

date_code <- xml2::read_html(data_url) %>% 
  html_text() %>% 
  str_extract('[Ww][Kk][_]?[0-9][0-9]_[0-9]')

#Try today
objectIDs <- fromJSON(
  glue('{data_url}/query?where=0%3D0&outFields={date_code},msoa11_cd&returnIdsOnly=true&f=json')
)

#function to download cases data
get_cases <- function(ids_in) {
  url <-
    utils::URLencode(
      glue(
        '{data_url}/query?where=OBJECTID%20IN%20({ids_in})&outFields={date_code},msoa11_cd&f=json'
      )
    )
  
  arcgisData <- xml2::read_html(url)
  
  arcgisData_parsed <- fromJSON(html_text(arcgisData))
  
  tibble(msoa11cd = arcgisData_parsed[["features"]][["attributes"]][["msoa11_cd"]],
         cases_count = arcgisData_parsed[["features"]][["attributes"]][[date_code]])
}

#Paginate the query and run
cases_msoa <- tibble(objectIDs = objectIDs$objectIds) %>%
  group_by(page = rep(row_number(), length.out = n(), each = 100)) %>%
  summarise(objectIDs = paste0(objectIDs, collapse = ',')) %>%
  mutate(cases = map(objectIDs, get_cases)) %>%
  select(-objectIDs, -page) %>%
  unnest(cols = c(cases)) %>% 
  mutate(refresh_time = lubridate::now(),
         dataID = date_code) %>% 
  replace_na(list(cases_count=0))

cases_msoa %>% 
  push_to_bigquery('hilltop-site.covid.msoa_cases', overwrite = FALSE, upsert = TRUE)

#-------------------------------------------------------------------------------

week_numbers <- read_csv("C:/Users/neild/Documents/dev/covid19_mapping/lsoa_csv/week_numbers.csv")

cases_lsoa <- read_csv('https://coronavirus.data.gov.uk/downloads/lsoa_data/LSOAs_latest.csv', na = '-99', col_types = cols(.default = "c")) %>% 
  pivot_longer(cols = contains('wk'), names_to = 'week_number', values_to = 'cases_count') %>% 
  mutate(cases_count = as.integer(cases_count)) %>% 
  rename(lsoa11cd = lsoa11_cd,
         dataID = week_number) %>% 
  mutate(refresh_time = lubridate::now()) %>% 
  select(lsoa11cd, cases_count, refresh_time, dataID) %>% 
  replace_na(list(cases_count = 0))

cases_lsoa %>% 
  push_to_bigquery('hilltop-site.covid.lsoa_cases', overwrite = TRUE, upsert = FALSE)

week_numbers %>% 
  push_to_bigquery('hilltop-site.covid.week_numbers', overwrite = TRUE, upsert = FALSE)

print(glue('Latest date in LSOA file is {
              cases_lsoa %>% 
              left_join(week_numbers, by = c("dataID" = "week_number")) %>% 
              pull(week_ending) %>% 
              max()}'))
