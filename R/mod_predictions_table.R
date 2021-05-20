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
          
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("classControl"))
            ),
            
            column(
              width = 6,
              uiOutput(ns("organizationControl"))
            )
          ),
          
          reactable::reactableOutput(ns("pedictedLabels")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE)
        )
      )
    )
  )
}
    
#' predictions_table Server Functions
#'
#' @noRd 
mod_predictions_table_server <- function(id, x, target, target_pred){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pedictedLabels <- reactable::renderReactable({
      
      if (target == "label") {
        
        feedback_col_new_name <- paste0(
          "Feedback that model predicted as ", "\"", input$class, "\""
        )
        
        aux <- x %>%
          #dplyr::right_join(row_index_label, by = 'row_index') %>% 
          dplyr::right_join(predictions_test_label, by = 'row_index') %>% 
          dplyr::select(-label) %>% 
          dplyr::rename(label = label_pred) %>% 
          dplyr::filter(
            label %in% input$class,
            organization %in% input$organization
          ) %>%
          dplyr::select(feedback, organization)
      } else {
        
        feedback_col_new_name <- paste0(
          "Feedback that model predicted as ", "\"", input$class, "\""
        )
        
        aux <- x %>%
          #dplyr::right_join(row_index_criticality, by = 'row_index') %>% 
          dplyr::right_join(predictions_test_criticality, by = 'row_index') %>% 
          dplyr::select(-criticality) %>% 
          dplyr::rename(criticality = criticality_pred) %>% 
          dplyr::filter(
            criticality %in% input$class,
            organization %in% input$organization
          ) %>%
          dplyr::select(feedback, organization)
      }
        
      reactable::reactable(
        aux,
        columns = 
          list(
            feedback = reactable::colDef(name = feedback_col_new_name),
            organization = reactable::colDef(name = "Organization", 
                                             align = "right")
          ),
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
      
      # accuracy_score <- y %>%
      #   dplyr::filter(
      #     class %in% input$class,
      #     organization %in% input$organization
      #   ) %>%
      #   dplyr::select(accuracy) %>%
      #   dplyr::mutate(accuracy = round(accuracy * 100)) %>%
      #   dplyr::pull()
      
      if (target == "label") {
        
        accuracy_score <- x %>% 
          dplyr::select(label, organization, row_index) %>% 
          dplyr::left_join(predictions_test_label, by = "row_index")
      } else {
          
        accuracy_score <- x %>% 
            dplyr::select(label, organization, row_index) %>% 
            dplyr::left_join(predictions_test_criticality, by = "row_index")
        }
        
      accuracy_score <- accuracy_score %>% 
        experienceAnalysis::calc_accuracy_per_class(
            target_col_name = target, 
            target_pred_col_name = target_pred,
            grouping_variable = "organization"
          ) %>% 
          dplyr::filter(
            class %in% input$class,
            organization %in% input$organization
          ) %>%
          dplyr::select(accuracy) %>%
          dplyr::mutate(accuracy = round(accuracy * 100)) %>%
          dplyr::pull()

      HTML(paste0(
             "NOTE: Learner accuracy for this label is ", accuracy_score, "%.
             This means that in 100 feedback records, ", accuracy_score,
             "  are predicted correctly."))
    })
    
    output$classControl <- renderUI({
      
      if (target == "label") {
        
        aux <- x %>%
          dplyr::right_join(row_index_label, by = 'row_index')
      } else {
        
        aux <- x %>%
          dplyr::right_join(row_index_criticality, by = 'row_index')
      }
      
      selectInput(
        session$ns("class"), 
        "Choose a label:",
        choices = sort(unique(unlist(aux[[target]]))),
        selected = sort(unique(unlist(aux[[target]])))[1]
      )
    })
    
    output$organizationControl <- renderUI({
      
      selectInput(
        session$ns("organization"), 
        "Choose an organization:",
        choices = sort(unique(x$organization)),
        selected = sort(unique(x$organization))[1]
      )
    })
  })
}
