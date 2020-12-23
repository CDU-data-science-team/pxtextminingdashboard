## code to prepare `open_data` dataset goes here

text_data <- readr::read_csv('data-raw/text_data.csv')
text_blob_scores <- readr::read_csv('data-raw/text_blob_scores.csv')
test_data <- readr::read_csv('data-raw/y_pred_and_x_test.csv')
data_for_tfidf <- readr::read_csv('data-raw/training_data.csv')
accuracy_per_class <- readr::read_csv('data-raw/accuracy_per_class.csv')

test_data_criticality <- readr::read_csv('data-raw/y_pred_and_x_test_imp_crit.csv')
accuracy_per_class_criticality <- readr::read_csv('data-raw/accuracy_per_class_imp_crit.csv')

usethis::use_data(text_data, overwrite = TRUE)
usethis::use_data(text_blob_scores, overwrite = TRUE)
usethis::use_data(test_data, overwrite = TRUE)
usethis::use_data(data_for_tfidf, overwrite = TRUE)
usethis::use_data(accuracy_per_class, overwrite = TRUE)

usethis::use_data(test_data_criticality, overwrite = TRUE)
usethis::use_data(accuracy_per_class_criticality, overwrite = TRUE)
