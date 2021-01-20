#' tfidf_and_word_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tfidf_and_word_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(width = 12,
             column(width = 12,
                    selectInput(ns("pred"), "Choose a label:",
                                choices=sort(unique(test_data$pred))),
                    box(
                      width = NULL,
                      plotOutput(ns("tfidf_bars")),
                      box(htmlOutput(ns("tfidfExplanation")), background = 'red', 
                          width = NULL)
                    )
             )
    )
  )
}
    
#' tfidf_and_word_processing Server Functions
#'
#' @noRd 
mod_tfidf_and_word_processing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$tfidf_bars <- renderPlot({
      data_for_tfidf %>%
        tidytext::unnest_tokens(word, improve) %>%
        dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) %>% # Do this because some stop words make it through the TF-IDF filtering that happens below.
        dplyr::count(super, word, sort = TRUE) %>%
        tidytext::bind_tf_idf(word, super, n) %>%
        #dplyr::arrange(dplyr::desc(tf_idf)) %>%
        dplyr::group_by(super) %>%
        dplyr::slice_max(tf_idf, n = 15) %>%
        dplyr::ungroup() %>%
        dplyr::filter(super == input$pred) %>%
        ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(word, tf_idf))) +
        ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
        ggplot2::labs(x = "TF-IDF*", y = NULL,
                      title = paste0("Most frequent words in feedback text that is about\n",
                                     "\"", input$pred, "\"")) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 12)
        )
    })
    
    output$tfidfExplanation <- renderText({
      HTML(paste0("*TF-IDF stands for
          <u><a href='https://en.wikipedia.org/wiki/Tf%E2%80%93idf'>
          Term Frequency–Inverse Document Frequency</a></u>.
          It is a standard way of calculating the frequency (i.e. importance)
          of a word in the given text. It is a little more sophisticated than
          standard frequency as it adjusts for words that appear too frequently
          in the text. For example, stop words like ", "\"", "a", "\"", " and ",
                  "\"", "the", "\"", " are very frequent but uninformative of
          the content of the text."))
    })
  })
}