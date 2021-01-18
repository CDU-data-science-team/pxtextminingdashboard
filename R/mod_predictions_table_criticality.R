#' predictions_table_criticality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predictions_table_criticality_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(12,
             box(
               width = NULL, background = "red",
               textOutput(ns("modelAccuracyBox"))
             )
      )
    ),
    
    fluidRow(
      column(width = 12,
             box(
               width = NULL,
               selectInput(
                 ns("pred"), 
                 "Choose a label:",
                 choices = sort(unique(test_data_criticality$pred))),
               reactable::reactableOutput(ns("pedictedLabels"))
             )
      )
    )
  )
}

#' predictions_table Server Functions
#'
#' @noRd 
mod_predictions_table_criticality_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pedictedLabels <- reactable::renderReactable({
      
      feedback_col_new_name <- paste0(
        "Feedback that model predicted as having a criticality score of ", 
        "\"", input$pred, "\""
      )
      
      reactable::reactable(
        test_data_criticality %>%
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
    
    output$modelAccuracyBox <- renderText({
      accuracy_score <- accuracy_per_class_criticality %>%
        dplyr::filter(class == input$pred) %>%
        dplyr::select(accuracy) %>%
        dplyr::mutate(accuracy = round(accuracy * 100)) %>%
        dplyr::pull()
      
      paste0("NOTE: Model accuracy for this label is ", accuracy_score, "%.
           This means that in 100 feedback records, ", accuracy_score,
             "  are predicted correctly.")
    })
  })
}
