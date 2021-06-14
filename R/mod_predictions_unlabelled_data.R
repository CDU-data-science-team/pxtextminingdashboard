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
          
          # box(
          #   width = NULL, 
          #   background = "red",
          #   htmlOutput(ns("modelAccuracyBox"))
          # ),
          
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput(ns("classControl"))
          #   ),
          #   
          #   column(
          #     width = 6,
          #     uiOutput(ns("organizationControl"))
          #   )
          # ),
          
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
mod_predictions_unlabelled_data_server <- function(id, x, sys_setenv, 
                                                   which_python, which_venv, 
                                                   venv_name, file_path, 
                                                   predictor, preds_column,
                                                   column_names, pipe_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    preds <- experienceAnalysis::calc_predict_unlabelled_text(
      x,
      sys_setenv,
      which_python,
      which_venv,
      venv_name,
      file_path,
      predictor,
      pipe_path,
      preds_column, 
      column_names
    )
    
    output$predictions <- reactable::renderReactable({
      
      reactable::reactable(preds)
    })
  })
}