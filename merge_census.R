library(tidyverse)

outfile <- 'demogs/merged.csv'

file.remove(outfile)

files <- list.files(path = 'census_2011/', pattern = ".*DATA.CSV", full.names = TRUE, recursive = TRUE)

long_demogs <- function(demog_file, outfile){
  write_csv(
    read_csv(demog_file) %>% 
      pivot_longer(-GeographyCode),
    outfile,
    append = TRUE
  )
}

walk(files, .f = ~long_demogs(., outfile))


outfile <- 'demogs/merged_desc.csv'

file.remove(outfile)

files <- list.files(path = 'census_2011/', pattern = ".*DESC0.CSV", full.names = TRUE, recursive = TRUE)

long_demogs <- function(demog_file, outfile){
  write_csv(
    read_csv(demog_file),
    outfile,
    append = TRUE
  )
}

walk(files, .f = ~long_demogs(., outfile))