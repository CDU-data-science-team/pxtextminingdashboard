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
          
          uiOutput(ns("classControl")),
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
    
    # When we pass NULL to preds_column in experienceAnalysis::calc_predict_unlabelled_text,
    # the column name with the predicted classes is 
    # paste0(text_col_name, "_preds") or, here, paste0(text_col, "_preds"). 
    # We need the preds object for filtering dataPredictions() by input$class.
    if (is.null(preds_column)) {
      preds <- paste0(text_col, "_preds")
    } else {
      preds <- preds_column
    }
    
    output$predictions <- reactable::renderReactable({

      dataPredictions() %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(preds),
            ~ . %in% input$class
          )
        ) %>% 
        reactable::reactable(filterable = TRUE)
    })
    
    output$downloadPredictions <- downloadHandler(
      filename = function() {paste0("predictions_", target, ".csv")},
      content = function(file) {
        write.csv(dataPredictions(), file)
      }
    )
    
    output$classControl <- renderUI({
      
      choices <- sort(unique(dataPredictions()[[preds]]))
      
      selectInput(
        session$ns("class"), 
        "Choose a class to see the predicted text for this class:",
        choices = choices,
        selected = choices[1],
        multiple = TRUE
      )
    })
  })
}
