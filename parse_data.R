library(tidyverse)
library(jsonlite)
library(glue)
library(rvest)
library(ggplot2)

data_url <- xml2::read_html('https://www.arcgis.com/sharing/rest/content/items/d8a2d1912b8b4ee19ff76aa892414e59/data') %>% 
  html_text() %>% 
  str_extract('https://services1.arcgis.com.*MSOA.*FeatureServer/0')

date_code <- xml2::read_html(data_url) %>% 
  html_text() %>% 
  str_extract('wk[0-9][0-9]_[0-9]')

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
covid_cases <- tibble(objectIDs = objectIDs$objectIds) %>%
  group_by(page = rep(row_number(), length.out = n(), each = 100)) %>%
  summarise(objectIDs = paste0(objectIDs, collapse = ',')) %>%
  mutate(cases = map(objectIDs, get_cases)) %>%
  select(-objectIDs, -page) %>%
  unnest(cols = c(cases)) %>% 
  mutate(refresh_time = lubridate::now(),
         dataID = date_code) %>% 
  replace_na(list(cases_count=0))

bigrquery::bq_auth(path = Sys.getenv('HILLTOP_GCP'))

con <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = 'hilltop-site'
)

if(!bigrquery::bq_table_exists('hilltop-site.covid.msoa_cases')){
  bigrquery::bq_table_create('hilltop-site.covid.msoa_cases', covid_cases)
}
  
DBI::dbWriteTable(con,
                  name = 'hilltop-site.covid.msoa_cases',
                  value = covid_cases,
                  append = TRUE)

DBI::dbDisconnect(con)