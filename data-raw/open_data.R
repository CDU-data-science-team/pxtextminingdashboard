## code to prepare `open_data` dataset goes here

## Connect to database ##
con_text_mining <- DBI::dbConnect(
  odbc::odbc(),
  Driver   = "MySQL ODBC 8.0 Unicode Driver",
  Server   = Sys.getenv("HOST_NAME"),
  UID      = Sys.getenv("DB_USER"),
  PWD      = Sys.getenv("MYSQL_PASSWORD"),
  Port     = 3306,
  database = "TEXT_MINING",
  encoding = "UTF-8")

#odbc::dbWriteTable(con_text_mining, "polarity_textblob", polarity_textblob, 
#  overwrite = TRUE, row.names = FALSE)
  
text_data <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM text_data') %>%
  dplyr::mutate(
    criticality = dplyr::case_when(
      criticality == -5 ~ "-4",
      criticality == 5 ~ "4",
      TRUE ~ criticality
    )
  )


### Data for label ###
index_training_data_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_training_data_label')

index_test_data_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_test_data_label')

row_index_label <- index_training_data_label %>% 
  dplyr::bind_rows(index_test_data_label)

predictions_test_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM predictions_test_label')

tuning_results_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM tuning_results_label')


### Data for criticality ###
index_training_data_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_training_data_criticality')

index_test_data_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_test_data_criticality')

row_index_criticality <- index_training_data_criticality %>% 
  dplyr::bind_rows(index_test_data_criticality)

predictions_test_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM predictions_test_criticality')

tuning_results_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM tuning_results_criticality')


### Write data to 'data' folder in RDA format ###
usethis::use_data(text_data, overwrite = TRUE)

usethis::use_data(row_index_label, overwrite = TRUE)
usethis::use_data(tuning_results_label, overwrite = TRUE)
usethis::use_data(predictions_test_label, overwrite = TRUE)

usethis::use_data(row_index_criticality, overwrite = TRUE)
usethis::use_data(tuning_results_criticality, overwrite = TRUE)
usethis::use_data(predictions_test_criticality, overwrite = TRUE)

DBI::dbDisconnect(con_text_mining)