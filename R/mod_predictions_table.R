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
mod_predictions_table_server <- function(id, x, y, predictor){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pedictedLabels <- reactable::renderReactable({
      
      if (predictor == "super") {
        
        feedback_col_new_name <- paste0(
          "Feedback that model predicted as ", "\"", input$label, "\""
        )
        
        aux <- x %>%
          dplyr::right_join(row_index_super, by = 'row_index') %>% 
          dplyr::filter(
            super %in% input$label,
            organization %in% input$organization
          ) %>%
          dplyr::select(improve, organization)
      } else {
        
        feedback_col_new_name <- paste0(
          "Feedback that model predicted as ", "\"", input$label, "\""
        )
        
        aux <- x %>%
          dplyr::right_join(row_index_criticality, by = 'row_index') %>% 
          dplyr::filter(
            imp_crit %in% input$label,
            organization %in% input$organization
          ) %>%
          dplyr::select(improve, organization)
      }
        
      reactable::reactable(
        aux,
        columns = 
          list(
            improve = reactable::colDef(name = feedback_col_new_name),
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
      
      accuracy_score <- y %>%
        dplyr::filter(
          class %in% input$label,
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
      
      if (predictor == "super") {
        
        aux <- x %>%
          dplyr::right_join(row_index_super, by = 'row_index')
      } else {
        
        aux <- x %>%
          dplyr::right_join(row_index_criticality, by = 'row_index')
      }
      
      selectInput(
        session$ns("label"), 
        "Choose a label:",
        choices = sort(unique(unlist(aux[[predictor]]))),
        selected = sort(unique(unlist(aux[[predictor]])))[1]
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
