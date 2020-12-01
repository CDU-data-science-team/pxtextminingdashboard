#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             box(width = NULL,
                 selectInput(ns("pred"), "Choose a label:",
                             choices=sort(unique(test_data$pred))),
                 plotOutput(ns("mostCommonWords")))
      ),
      
      column(width = 6,
             box(
               width = NULL,
               shiny::plotOutput(ns("netSentiment"))
             )
      )
    )
  )
}
    
#' sentiment_analysis Server Functions
#'
#' @noRd 
mod_sentiment_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tidy_feedback <- data_for_tfidf %>%
      dplyr::group_by(super) %>%
      dplyr::mutate(linenumber = dplyr::row_number()) %>%
      tidytext::unnest_tokens(word, improve) %>%
      dplyr::group_by(super) %>%
      dplyr::arrange(super, linenumber) %>%
      dplyr::ungroup()
    
    # Find net sentiment in each tag
    net_sentiment_afinn <- tidy_feedback %>%
      dplyr::inner_join(tidytext::get_sentiments("afinn")) %>% 
      dplyr::group_by(super) %>%
      dplyr::summarise(sentiment = sum(value)) %>% 
      dplyr::mutate(method = "AFINN")
    
    net_sentiment_bing_and_nrc <- 
      dplyr::bind_rows(
        tidy_feedback %>%
          dplyr::inner_join(tidytext::get_sentiments("bing")) %>% 
          dplyr::filter(sentiment %in% c("positive", "negative")) %>%
          dplyr::mutate(method = "Bing et al."),
        
        tidy_feedback %>%
          dplyr::inner_join(tidytext::get_sentiments("nrc")) %>% 
          dplyr::filter(sentiment %in% c("positive", "negative")) %>%
          dplyr::mutate(method = "NRC")
      ) %>%
      dplyr::count(super, method, sentiment) %>%
      tidyr::spread(sentiment, n, fill = 0) %>%
      dplyr::mutate(sentiment = positive - negative) %>%
      dplyr::ungroup()
    
    output$netSentiment <- renderPlot({
      
      dplyr::bind_rows(net_sentiment_afinn, 
                       net_sentiment_bing_and_nrc) %>%
        ggplot2::ggplot(ggplot2::aes(sentiment, reorder(super, sentiment))) +
        ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
        ggplot2::facet_wrap(~method, ncol = 1, scales = "free") +
        ggplot2::labs(x = "Sentiment", y = NULL,
                      title = "Sentiment per tag and dictionary") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    output$mostCommonWords <- renderPlot({
      
      # Most common positive and negative words
      bing_word_counts <- tidy_feedback %>%
        dplyr::filter(super == input$pred) %>%
        dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
        dplyr::count(word, sentiment, sort = TRUE)
      
      bing_word_counts %>%
        dplyr::group_by(sentiment) %>%
        dplyr::top_n(10) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(n, reorder(word, n), fill = sentiment)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::facet_wrap(~sentiment, scales = "free_y") +
        ggplot2::labs(x = "Contribution to sentiment",
                      y = NULL)
    })
    
  })
}
    
## To be copied in the UI
# mod_sentiment_analysis_ui("sentiment_analysis_ui_1")
    
## To be copied in the server
# mod_sentiment_analysis_server("sentiment_analysis_ui_1")
