#' predictions_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predictions_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(7, 
        box(width = NULL,
          textOutput(ns("modelAccuracyBox")), background = 'red'
        )
      )
    ),
    
    fluidRow(
      column(width = 7,
        box(width = NULL,
          selectInput(ns("pred"), "Choose a label:", 
          choices=sort(unique(test_data$pred))),
        reactable::reactableOutput(ns("pedictedLabels")))
      ),
      
     column(width = 5,
       box(shiny::plotOutput(ns("tfidf_bars")), width = NULL),
       box(shiny::htmlOutput(ns("tfidfExplanation")), background = 'red', width = NULL)
     )
   )
  )
}
    
#' predictions_table Server Functions
#'
#' @noRd 
mod_predictions_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pedictedLabels <- reactable::renderReactable({
      
      feedback_col_new_name <- paste0(
        "Feedback that model predicted as ", "\"", input$pred, "\""
      )
      
      reactable::reactable(
        test_data %>%
          dplyr::filter(pred == input$pred) %>%
          dplyr::select(improve),
        columns = list(improve = reactable::colDef(name = feedback_col_new_name)),
        #rownames = TRUE,
        searchable = TRUE,
        sortable = FALSE,
        defaultPageSize = 100,
        pageSizeOptions = 100,
        language = reactable::reactableLang(
          searchPlaceholder = "Search for a word..."),
      )
    })
    
    output$modelAccuracyBox <-renderText({
      accuracy_score <- accuracy_per_class %>%
        dplyr::filter(class == input$pred) %>%
        dplyr::select(accuracy) %>%
        dplyr::mutate(accuracy = round(accuracy * 100)) %>%
        dplyr::pull
      
      paste0("NOTE: Model accuracy for this label is ", accuracy_score, "%. 
           This means that in 100 feedback records, ", accuracy_score, 
             "  are predicted correctly.")
    })
    
    output$tfidf_bars <-renderPlot({
      data_for_tfidf %>%
        tidytext::unnest_tokens(word, improve) %>%
        dplyr::count(super, word, sort = TRUE) %>%
        tidytext::bind_tf_idf(word, super, n) %>%
        dplyr::arrange(dplyr::desc(tf_idf)) %>%
        dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) %>% # Do this because some stop words make it through the TF-IDF filtering that happens below.
        #as_tibble %>%
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
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    output$tfidfExplanation <-renderText({
     HTML(paste0("*TF-IDF stands for 
          <u><a href='https://en.wikipedia.org/wiki/Tf%E2%80%93idf'>
          Term Frequency–Inverse Document Frequency</a></u>.
          It is a standard way of calculating the frequency (i.e. importance) 
          of a word in the given text. It is a little more sophisticated than
          standard frequency as it adjusts for words that appear too frequently
          in the text. For example, stop words like ", "\"", "a", "\"", " and ",
                  "\"", "the", "\"", " are very frequent but uniformative of 
          the cotext of the text."))
    })
  })
}
    
## To be copied in the UI
# mod_predictions_table_ui("predictions_table_ui_1")
    
## To be copied in the server
# mod_predictions_table_server("predictions_table_ui_1")
