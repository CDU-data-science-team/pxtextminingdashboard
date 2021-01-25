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

tfidf_bigrams <- function(x, label) {
  
  x %>%
    tidytext::unnest_tokens(ngram, improve, token = "ngrams", n = 2) %>%
    tidyr::separate(ngram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
    dplyr::filter(!word2 %in% tidytext::stop_words$word) %>%
    tidyr::unite(col = "ngram", word1, word2, sep = " ") %>%
    dplyr::count(super, ngram, sort = TRUE) %>%
    tidytext::bind_tf_idf(ngram, super, n) %>%
    #dplyr::arrange(dplyr::desc(tf_idf)) %>%
    dplyr::group_by(super) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(super %in% label) %>%
    ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(ngram, tf_idf))) +
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
  
bigrams_network_plot <- function(x, label) {
  
  a <- grid::arrow(
    type = "closed", 
    length = grid::unit(.15, "inches")
  )
  
  x %>%
    dplyr::filter(super %in% label) %>%
    tidytext::unnest_tokens(bigram, improve, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(
      !word1 %in% tidytext::stop_words$word,
      !word2 %in% tidytext::stop_words$word
    ) %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::filter(n > 20) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = n), 
                           show.legend = FALSE, arrow = a) +
    ggraph::geom_node_point(color = "lightblue", size = 5) +
    ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    ggplot2::theme_void()
}