
## Connect to database ##
con_text_mining <- DBI::dbConnect(
  odbc::odbc(),
  Driver   = "Maria DB",
  Server   = Sys.getenv("HOST_NAME"),
  UID      = Sys.getenv("DB_USER"),
  PWD      = Sys.getenv("MYSQL_PASSWORD"),
  Port     = 3306,
  database = "TEXT_MINING",
  encoding = "UTF-8")

con_suce <- DBI::dbConnect(
  odbc::odbc(),
  Driver   = "Maria DB",
  Server   = Sys.getenv("HOST_NAME"),
  UID      = Sys.getenv("DB_USER"),
  PWD      = Sys.getenv("MYSQL_PASSWORD"),
  Port     = 3306,
  database = "SUCE",
  encoding = "UTF-8")

codes <- DBI::dbReadTable(con_text_mining, "NewCodes")

# care opinion

co_1 <- readxl::read_excel("data-raw/CO1 coded SC .xlsx", 
                           col_names = c("crit", paste0("cat_", 1 : 6),
                                         "feedback"))
co_2 <- readxl::read_excel("data-raw/CO2.xlsx")

co_1 <- co_1 %>% 
  dplyr::mutate(cat_1 = toupper(cat_1)) %>% 
  dplyr::left_join(codes, by = c("cat_1" = "Code")) %>% 
  dplyr::select(code = cat_1, label = Category, 
                subcategory = Subcategory, 
                feedback, criticality = crit) %>% 
  dplyr::mutate(organization = "Care Opinion",
                question = "Care Opinion - Q1") %>% 
  dplyr::filter(!is.na(code))

# co_2 is a bit harder and requires a bit of pre-processing...
# if severity is positive, read the positive theme.
# if negative, read the negative theme

co_2 <- co_2 %>% 
  dplyr::mutate(Code = dplyr::case_when(
    Severity >= 0 ~ `Posititve Codes`,
    Severity < 0 ~ `Negative Codes`,
  )) %>% 
  dplyr::mutate(Code = toupper(Code)) %>% 
  dplyr::left_join(codes) %>% 
  dplyr::select(code = Code, label = Category, 
                subcategory = Subcategory, 
                feedback = PO, criticality = Severity) %>% 
  dplyr::mutate(organization = "Care Opinion",
                question = "Care Opinion - Q1") %>% 
  dplyr::filter(!is.na(code))

care_opinion <- rbind(co_1, co_2)

# now the PACE data

suce_tbl <- dplyr::tbl(con_suce, "Local")

suce <- suce_tbl %>% 
  dplyr::filter(!is.null(Imp_N1) | !is.null(Best_N1)) %>% 
  dplyr::select(comment_1 = Improve, comment_2 = Best,
                category_1 = Imp_N1, category_2 = Best_N1, 
                crit_1 = ImpCrit, crit_2 = BestCrit) %>%
  dplyr::mutate(across(dplyr::starts_with("category"), 
                       ~ toupper(.x))) %>% 
  dplyr::collect() %>% 
  dplyr::mutate(across(dplyr::starts_with("crit"), 
                       ~ dplyr::recode(.x, `9` = NA_integer_))) %>% 
  dplyr::mutate(across(dplyr::starts_with("category"), 
                       ~ dplyr::recode(.x, XX = NA_character_,
                                       `9` = NA_character_))) %>% 
  dplyr::mutate(pt_id = dplyr::row_number()) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("comment_"),
                      names_to = "comment_type",
                      values_to = "comment_txt") %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("category"),
                      names_to = "category_type",
                      values_to = "category") %>% 
  dplyr::filter(
    stringr::str_sub(comment_type, -1) == stringr::str_sub(category_type, -1)) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("crit"),
                      names_to = "crit_type",
                      values_to = "criticality") %>% 
  dplyr::filter(
    stringr::str_sub(comment_type, -1) == stringr::str_sub(crit_type, -1)) %>% 
  dplyr::filter(!is.na(category)) %>% 
  dplyr::mutate(category = stringr::str_trim(category)) %>% 
  dplyr::left_join(codes, by = c("category" = "Code")) %>% 
  dplyr::select(code = category, label = Category, 
                subcategory = Subcategory, feedback = comment_txt,
                criticality, comment_type) %>% 
  dplyr::mutate(organization = "Trust A", 
                question = paste0("Trust A - Q", 
                                  stringr::str_sub(comment_type, -1))) %>% 
  dplyr::select(-comment_type) %>% 
  dplyr::mutate(criticality = as.character(criticality))

# add the other two trusts

two_trusts <- readr::read_csv("data-raw/text_data.csv")

two_trusts <- two_trusts %>% 
  dplyr::mutate(code = toupper(code)) %>% 
  dplyr::filter(organization != "Trust A") %>% 
  dplyr::select(-row_index)

final <- rbind(suce, two_trusts) %>% 
  dplyr::mutate(row_index = 0:(nrow(.) - 1)) %>% 
  dplyr::mutate(
    criticality = dplyr::case_when(
      criticality == "-5" ~ "-4",
      criticality == "5" ~ "4",
      criticality %in% c("-4", "-3", "-2", "-1", "0",
                         "4", "3", "2", "1") ~ criticality,
      TRUE ~ NA_character_
    )
  )

DBI::dbWriteTable(con_text_mining, "text_data", final, 
  overwrite = TRUE, row.names = FALSE)

#odbc::dbWriteTable(con_text_mining, "polarity_textblob", polarity_textblob, 
#  overwrite = TRUE, row.names = FALSE)

### Data for label ###

text_data <- DBI::dbGetQuery(
  con_text_mining,
  'SELECT * FROM text_data')

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

# save CO data in csv for repo

care_opinion <- care_opinion %>% 
  dplyr::mutate(code = tolower(code),
                row_index = 0 : (nrow(.) - 1))

write.csv(care_opinion, file = "~/co.csv", row.names = FALSE)

usethis::use_data(text_data, overwrite = TRUE)

# write unlabelled 

usethis::use_data(row_index_label, overwrite = TRUE)
usethis::use_data(tuning_results_label, overwrite = TRUE)
usethis::use_data(predictions_test_label, overwrite = TRUE)

usethis::use_data(row_index_criticality, overwrite = TRUE)
usethis::use_data(tuning_results_criticality, overwrite = TRUE)
usethis::use_data(predictions_test_criticality, overwrite = TRUE)

DBI::dbDisconnect(con_text_mining)