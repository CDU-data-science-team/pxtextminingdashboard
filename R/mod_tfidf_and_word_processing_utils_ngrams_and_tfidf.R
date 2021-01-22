tfidf_unigrams <- function(x, label) {
  
  x %>%
    tidytext::unnest_tokens(word, improve) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) %>% # Do this because some stop words make it through the TF-IDF filtering that happens below.
    dplyr::count(super, word, sort = TRUE) %>%
    tidytext::bind_tf_idf(word, super, n) %>%
    #dplyr::arrange(dplyr::desc(tf_idf)) %>%
    dplyr::group_by(super) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(super %in% label) %>%
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