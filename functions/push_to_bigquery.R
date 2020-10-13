push_to_bigquery <- function(data, tablename, overwrite, upsert) {
  bigrquery::bq_auth(path = Sys.getenv('HILLTOP_GCP'))
  
  con <- DBI::dbConnect(bigrquery::bigquery(),
                        project = 'hilltop-site')
  
  if (overwrite) {
    if (bigrquery::bq_table_exists(tablename)) {
      bigrquery::bq_table_delete(tablename)
    }
  }
  if (!bigrquery::bq_table_exists(tablename)) {
    bigrquery::bq_table_create(tablename, data)
  }

  if (upsert) {
    IDs = data %>%
      mutate(dataID = glue("'{dataID}'")) %>%
      group_by(dataID) %>%
      summarise() %>%
      summarise(dataID = paste0(dataID, collapse = ',')) %>%
      pull(dataID)
    
    DBI::dbSendQuery(con,
                     glue("DELETE FROM {tablename} WHERE dataID IN({IDs})"))
  }
  
  batch_ranges = c(seq(1, nrow(data), 10000),
                   nrow(data))
  
  for (i in 1:(length(batch_ranges) - 1)) {
    DBI::dbWriteTable(con,
                      name = tablename,
                      value = data[batch_ranges[i]:batch_ranges[i + 1],],
                      append = TRUE)
  }
  
  DBI::dbDisconnect(con)
  
  invisible(TRUE)
  
}