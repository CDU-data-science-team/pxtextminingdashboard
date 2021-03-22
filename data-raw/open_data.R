## code to prepare `open_data` dataset goes here
text_blob_scores <- readr::read_csv('data-raw/text_blob_scores.csv')

con_text_mining <- DBI::dbConnect(
  odbc::odbc(),
  Driver   = "MySQL ODBC 8.0 Unicode Driver",
  Server   = Sys.getenv("HOST_NAME"),
  UID      = Sys.getenv("DB_USER"),
  PWD      = Sys.getenv("MYSQL_PASSWORD"),
  Port     = 3306,
  database = "TEXT_MINING",
  encoding = "UTF-8")

text_data <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM text_data') %>%
  dplyr::rename(super = label, improve = feedback, imp_crit = criticality, 
                organization = nhs_trust)

# Data for label
index_training_data_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_training_data_label')

index_test_data_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM index_test_data_label')

row_index_super <- index_training_data_label %>% 
  dplyr::bind_rows(index_test_data_label)

predictions_test_label <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM predictions_test_label')

accuracy_per_class <- text_data %>% 
  dplyr::select(super, organization, row_index) %>% 
  dplyr::left_join(predictions_test_label, by = "row_index") %>% 
  dplyr::mutate(actual_vs_predicted = super == label_pred) %>% 
  dplyr::filter(!is.na(actual_vs_predicted)) %>% 
  dplyr::group_by(organization, super) %>% 
  dplyr::summarise(accuracy = sum(actual_vs_predicted) / 
                     length(actual_vs_predicted)) %>% 
  dplyr::rename(class = super)

tuning_results_super <- DBI::dbGetQuery(
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
  dplyr::select(imp_crit, organization, row_index) %>% 
  dplyr::left_join(predictions_test_criticality, by = "row_index") %>% 
  dplyr::mutate(actual_vs_predicted = imp_crit == criticality_pred) %>% 
  dplyr::filter(!is.na(actual_vs_predicted)) %>% 
  dplyr::group_by(organization, imp_crit) %>% 
  dplyr::summarise(accuracy = sum(actual_vs_predicted) / 
                     length(actual_vs_predicted)) %>% 
  dplyr::rename(class = imp_crit)

tuning_results_criticality <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM tuning_results_criticality')

usethis::use_data(text_blob_scores, overwrite = TRUE)
usethis::use_data(text_data, overwrite = TRUE)

usethis::use_data(row_index_super, overwrite = TRUE)
usethis::use_data(accuracy_per_class, overwrite = TRUE)
usethis::use_data(tuning_results_super, overwrite = TRUE)

usethis::use_data(row_index_criticality, overwrite = TRUE)
usethis::use_data(accuracy_per_class_criticality, overwrite = TRUE)
usethis::use_data(tuning_results_criticality, overwrite = TRUE)
