bigrams_network_plot <- function(x, label, y) {
  
  a <- grid::arrow(
    type = "closed", 
    length = grid::unit(.15, "inches")
  )
  
  x %>%
    dplyr::filter(dplyr::across(dplyr::all_of(y), ~ . %in% label)) %>%
    tidytext::unnest_tokens(bigram, improve, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(
      !word1 %in% tidytext::stop_words$word,
      !word2 %in% tidytext::stop_words$word
    ) %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::slice_max(prop = 0.1, order_by = n) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = n), 
                           show.legend = FALSE, arrow = a) +
    ggraph::geom_node_point(color = "lightblue", size = 5) +
    ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1) +
    ggplot2::theme_void()
}