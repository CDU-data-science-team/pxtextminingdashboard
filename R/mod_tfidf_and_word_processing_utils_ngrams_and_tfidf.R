tfidf_unigrams <- function(x, label, y) {
  
  x %>%
    tidytext::unnest_tokens(word, improve) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) %>% # Do this because some stop words make it through the TF-IDF filtering that happens below.
    dplyr::count(.data[[y]], word, sort = TRUE) %>%
    tidytext::bind_tf_idf(word, .data[[y]], n) %>%
    dplyr::group_by(.data[[y]]) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::across(dplyr::all_of(y), ~ . %in% label)) %>%
    ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(word, tf_idf))) +
    ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
    ggplot2::labs(x = "TF-IDF*", y = NULL,
                  title = paste0("Most frequent words in feedback text that is about\n",
                                 "\"", label, "\"")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12)
    )
}

# tfidf_bigrams <- function(x, label) {
#   
#   x %>%
#     tidytext::unnest_tokens(ngram, improve, token = "ngrams", n = 2) %>%
#     tidyr::separate(ngram, c("word1", "word2"), sep = " ") %>%
#     dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
#     dplyr::filter(!word2 %in% tidytext::stop_words$word) %>%
#     tidyr::unite(col = "ngram", word1, word2, sep = " ") %>%
#     dplyr::count(dplyr::all_of(y), ngram, sort = TRUE) %>%
#     tidytext::bind_tf_idf(ngram, dplyr::all_of(y), n) %>%
#     #dplyr::arrange(dplyr::desc(tf_idf)) %>%
#     dplyr::group_by(dplyr::all_of(y)) %>%
#     dplyr::slice_max(tf_idf, n = 15) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(dplyr::all_of(y) %in% label) %>%
#     ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(ngram, tf_idf))) +
#     ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
#     ggplot2::labs(x = "TF-IDF*", y = NULL,
#                   title = paste0("Most frequent words in feedback text that is about\n",
#                                  "\"", label, "\"")) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(
#       panel.grid.major = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       axis.text.y = ggplot2::element_text(size = 12)
#     )
# }