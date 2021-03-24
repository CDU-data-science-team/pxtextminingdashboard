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

### TextBlob polarity scores from Python ###
Sys.setenv(RETICULATE_PYTHON = "C:/Users/andreas.soteriades/Anaconda3/envs/text_mining_dashboard/python.exe")
reticulate::use_python("C:/Users/andreas.soteriades/Anaconda3/envs/text_mining_dashboard/python.exe")
reticulate::use_condaenv("text_mining_dashboard", required = TRUE)
reticulate::py_config()
polarity_textblob <- reticulate::py_run_file("textblob_polarity.py")$
  text_data %>% 
  dplyr::select(-feedback)

#odbc::dbWriteTable(con_text_mining, "polarity_textblob", polarity_textblob, 
#  overwrite = TRUE, row.names = FALSE)
  
text_data <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM text_data')

# Data for label
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

accuracy_per_class_label <- text_data %>% 
  dplyr::select(label, organization, row_index) %>% 
  dplyr::left_join(predictions_test_label, by = "row_index") %>% 
  dplyr::mutate(actual_vs_predicted = label == label_pred) %>% 
  dplyr::filter(!is.na(actual_vs_predicted)) %>% 
  dplyr::group_by(organization, label) %>% 
  dplyr::summarise(accuracy = sum(actual_vs_predicted) / 
                     length(actual_vs_predicted)) %>% 
  dplyr::rename(class = label)

tuning_results_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM tuning_results_label')

# Data for criticality
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

accuracy_per_class_criticality <- text_data %>% 
  dplyr::select(criticality, organization, row_index) %>% 
  dplyr::left_join(predictions_test_criticality, by = "row_index") %>% 
  dplyr::mutate(actual_vs_predicted = criticality == criticality_pred) %>% 
  dplyr::filter(!is.na(actual_vs_predicted)) %>% 
  dplyr::group_by(organization, criticality) %>% 
  dplyr::summarise(accuracy = sum(actual_vs_predicted) / 
                     length(actual_vs_predicted)) %>% 
  dplyr::rename(class = criticality)

tuning_results_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM tuning_results_criticality')

usethis::use_data(polarity_textblob, overwrite = TRUE)
usethis::use_data(text_data, overwrite = TRUE)

usethis::use_data(row_index_label, overwrite = TRUE)
usethis::use_data(accuracy_per_class_label, overwrite = TRUE)
usethis::use_data(tuning_results_label, overwrite = TRUE)

usethis::use_data(row_index_criticality, overwrite = TRUE)
usethis::use_data(accuracy_per_class_criticality, overwrite = TRUE)
usethis::use_data(tuning_results_criticality, overwrite = TRUE)

dbDisconnect(con_text_mining)