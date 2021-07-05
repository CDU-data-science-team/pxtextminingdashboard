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
          title = "Predicted text for each class",
          width = NULL,
          
          box(
            width = NULL, 
            background = "red",
            htmlOutput(ns("modelAccuracyBox"))
          ),
          
          fluidRow(
            column(
              width = 4,
              uiOutput(ns("classControl"))
            )
          ),
          
          reactable::reactableOutput(ns("predictedClasses")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE)
        )
      )
    )
  )
}
    
#' predictions_table Server Functions
#'
#' @noRd 
mod_predictions_table_server <- function(id, x, target, target_pred, text_col, 
                                         groups, preds, row_indices) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$predictedClasses <- reactable::renderReactable({
      
      feedback_col_new_name <- paste0(
        "Feedback that model predicted as ", "\"", input$class, "\""
      )
      
      aux <- x %>%
        dplyr::right_join(preds, by = "row_index") %>% 
        # dplyr::select(-dplyr::all_of(target)) %>% 
        # dplyr::rename_with(
        #   ~ target, 
        #   .cols = dplyr::all_of(paste0(target, "_pred"))
        # ) %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(target_pred),
            ~ . %in% input$class
          )
        ) %>%
        dplyr::select(dplyr::all_of(c(text_col, target, groups)))
      
      reactable_cols <- list(
        reactable::colDef(name = feedback_col_new_name),
        reactable::colDef(name = "Actual class", align = "right")
      )
      names(reactable_cols) <- c(text_col, target, groups[1])
        
      reactable::reactable(
        aux,
        columns = reactable_cols,
          # list(
          #   feedback = reactable::colDef(name = feedback_col_new_name),
          #   organization = reactable::colDef(name = "Organization", 
          #                                    align = "right")
          # ),
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
      
      accuracy_score <- x %>% 
        dplyr::select(dplyr::all_of(c(target, groups)), row_index) %>% 
        dplyr::right_join(preds, by = "row_index") %>% 
        experienceAnalysis::calc_accuracy_per_class(
            target_col_name = target, 
            target_pred_col_name = target_pred,
            grouping_variables = groups,
            column_names = NULL
          ) %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(target),
            ~ . %in% input$class
          )
        ) %>%
        dplyr::select(accuracy) %>%
        dplyr::mutate(accuracy = round(accuracy * 100)) %>%
        dplyr::pull()

      HTML(paste0(
             "NOTE: Learner accuracy for this class is ", accuracy_score, "%.
             This means that in 100 feedback records, ", accuracy_score,
             "  are predicted correctly."))
    })
    
    output$classControl <- renderUI({
      
      aux <- x %>%
        dplyr::right_join(row_indices, by = 'row_index')
      
      selectInput(
        session$ns("class"), 
        "Choose a class:",
        choices = sort(unique(unlist(aux[[target]]))),
        selected = sort(unique(unlist(aux[[target]])))[1]
      )
    })
  })
}
