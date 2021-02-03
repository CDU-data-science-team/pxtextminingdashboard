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
  
    fluidRow(
      column(
        width = 12,
        box(
          title = "Predicted text for each label",
          width = NULL,
          box(
            width = NULL, 
            background = "red",
            htmlOutput(ns("modelAccuracyBox"))
          ),
          uiOutput(ns("classControl")),
          reactable::reactableOutput(ns("pedictedLabels"))
        )
      )
    )
  )
}
    
#' predictions_table Server Functions
#'
#' @noRd 
mod_predictions_table_server <- function(id, x, y){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pedictedLabels <- reactable::renderReactable({

      feedback_col_new_name <- paste0(
        "Feedback that model predicted as ", "\"", input$pred, "\""
      )

      reactable::reactable(
        x %>%
          dplyr::filter(pred %in% input$pred) %>%
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
      accuracy_score <- y %>%
        dplyr::filter(class %in% input$pred) %>%
        dplyr::select(accuracy) %>%
        dplyr::mutate(accuracy = round(accuracy * 100)) %>%
        dplyr::pull()

      HTML(paste0(
             "NOTE: Learner accuracy for this label is ", accuracy_score, "%.
             This means that in 100 feedback records, ", accuracy_score,
             "  are predicted correctly."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("pred"), 
        "Choose a label:",
        choices = sort(unique(x$pred)),
        selected = sort(unique(x$pred))[1]
      )
    })
  })
}
