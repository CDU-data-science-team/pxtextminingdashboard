## code to prepare `data_for_tfidf` dataset goes here

data_for_tfidf <- readr::read.csv('data-raw/training_data.csv')
usethis::use_data(data_for_tfidf, overwrite = TRUE)
