## code to prepare `open_data` dataset goes here

test_data <- readr::read_csv('data-raw/y_pred_and_x_test.csv')
data_for_tfidf <- readr::read_csv('data-raw/training_data.csv')
accuracy_per_class <- readr::read_csv('data-raw/accuracy_per_class.csv')

usethis::use_data(test_data, overwrite = TRUE)
usethis::use_data(data_for_tfidf, overwrite = TRUE)
usethis::use_data(accuracy_per_class, overwrite = TRUE)
