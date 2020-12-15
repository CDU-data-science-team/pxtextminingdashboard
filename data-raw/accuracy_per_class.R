## code to prepare `accuracy_per_class` dataset goes here

accuracy_per_class <- readr::read.csv('data-raw/accuracy_per_class.csv')
usethis::use_data(accuracy_per_class, overwrite = TRUE)
