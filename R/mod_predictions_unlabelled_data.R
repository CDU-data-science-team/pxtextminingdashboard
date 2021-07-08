#' predictions_unlabelled_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predictions_unlabelled_data_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 12,
        
        box(
          title = "Predicted text for each class",
          width = NULL,
          
          downloadButton(ns("downloadPredictions"), "Download data"),
          
          reactable::reactableOutput(ns("predictions")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE)
        )
      )
    )
  )
}
    
#' predictions_unlabelled_data Server Functions
#'
#' @noRd 
mod_predictions_unlabelled_data_server <- function(id, x, target, python_setup, 
                                                   sys_setenv, 
                                                   which_python, which_venv, 
                                                   venv_name, 
                                                   text_col, preds_column,
                                                   column_names, pipe_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
        dataPredictions <- reactive({
          
          withProgress(
            message = "Making the predictions",
            detail = "May take a minute or two...", 
            value = 0, 
            {
              x %>% 
                experienceAnalysis::calc_predict_unlabelled_text(
                  python_setup,
                  sys_setenv,
                  which_python,
                  which_venv,
                  venv_name,
                  text_col,
                  pipe_path,
                  preds_column, 
                  column_names[1]
                )
            }
          )
        })
    
    output$predictions <- reactable::renderReactable({
      
      reactable::reactable(dataPredictions(), filterable = TRUE)
    })
    
    output$downloadPredictions <- downloadHandler(
      filename = function() {paste0("predictions_", target, ".csv")},
      content = function(file) {
        write.csv(dataPredictions(), file)
      }
    )
  })
}
