bigrams_network_plot <- function(x, label, y, bigrams_prop) {
  
  a <- grid::arrow(
    type = "closed", 
    length = grid::unit(.15, "inches")
  )
  
  x <- x %>%
    dplyr::filter(dplyr::across(dplyr::all_of(y), ~ . %in% label)) %>%
    tidytext::unnest_tokens(bigram, improve, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(
      dplyr::across(dplyr::starts_with("word"), 
      ~ !. %in% tidytext::stop_words$word ) # Do this because some stop words make it through the TF-IDF filtering that happens below.
    ) %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::filter(
      dplyr::across(dplyr::starts_with("word"), ~ !is.na(.)),
      n > 1
    ) %>%
    dplyr::slice_max(prop = bigrams_prop / 100, order_by = n)
  
  if (nrow(x) != 0) {
   x %>%
      igraph::graph_from_data_frame() %>%
      ggraph::ggraph(layout = "fr") +
      ggraph::geom_edge_link(
        ggplot2::aes(edge_alpha = n), 
        show.legend = FALSE, 
        arrow = a
      ) +
      ggraph::geom_node_point(color = "blue", size = 5) +
      ggraph::geom_node_text(
        ggplot2::aes(label = name), 
        vjust = 1, 
        hjust = 1,
        size = 5
      ) +
      ggplot2::theme_void()
  } else {
    
    plot(x = 0, y = 0, xaxt = 'n', yaxt = 'n', ann = FALSE, type = 'n')
    text(x = 0, y = 0, labels= "Not enough data for the selected label")
  }
}