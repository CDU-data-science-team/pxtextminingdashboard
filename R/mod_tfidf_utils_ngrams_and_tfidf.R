tfidf_ngrams <- function(x, y, class, organization, ngrams_type) {
  
  ngrams_n <- ifelse(ngrams_type == "Unigrams", 1, 2)
  
  x %>%
    dplyr::filter(organization %in% {{organization}}) %>%
    tidytext::unnest_tokens(ngram, feedback, token = "ngrams", n = ngrams_n) %>%
    tidyr::separate(ngram, paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::filter(
      dplyr::across(dplyr::starts_with("word"), 
                    ~ !. %in% tidytext::stop_words$word ) # Do this because some stop words make it through the TF-IDF filtering that happens below.
    ) %>% 
    tidyr::unite(col = "ngram", paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::count(.data[[y]], ngram, sort = TRUE) %>%
    tidytext::bind_tf_idf(ngram, .data[[y]], n) %>%
    dplyr::group_by(.data[[y]]) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::across(dplyr::all_of(y), ~ . %in% {{class}})) %>%
    ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(ngram, tf_idf))) +
    ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
    ggplot2::labs(x = "TF-IDF*", y = NULL,
                  title = paste0("Most frequent ", ngrams_type, 
                                 " in feedback text that is about\n",
                                 "\"", class, "\"")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12)
    )
}
