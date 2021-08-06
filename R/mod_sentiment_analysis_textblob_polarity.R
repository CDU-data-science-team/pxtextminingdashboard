#' text_blob UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_analysis_textblob_polarity_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(
        width = 12,
        
        box(
          width = NULL, 
          background = "red",
          htmlOutput(ns("textBlobBox"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12, 
        
        box(
          width = NULL,
          
          uiOutput(ns("classControl")),
          downloadButton(ns("downloadTextBlob"), "Download data"),
          reactable::reactableOutput(ns("textBlob"))
        )
      )
    )
  )
}

#' sentiment_analysis Server Functions
#'
#' @noRd 
mod_sentiment_analysis_textblob_polarity_server <- function(id, x, sys_setenv, 
                                                            which_python, 
                                                            which_venv, 
                                                            venv_name, 
                                                            text_col, 
                                                            target_label, 
                                                            target_criticality) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # reactable stuff
    reactable_sticky_style <- list(position = "sticky", left = 0, 
                                   background = "#fff", zIndex = 1,
                                   borderRight = "1px solid #eee")
    
    reactable_columns <- list(
      reactable::colDef(
        name = "Feedback",
        style = reactable_sticky_style,
        headerStyle = reactable_sticky_style,
        minWidth = 300
      ),
      
      reactable::colDef(
        name = "Polarity",
        align = "right",
        class = "border-left cell number"
      ),
      
      reactable::colDef(
        name = "Label",
        align = "right",
        minWidth = 120
      ),
      
      reactable::colDef(
        name = "Criticality",
        align = "right"
      )
    )
    
    names(reactable_columns) <- c(text_col, "text_blob_polarity", target_label, 
                                  target_criticality)
    
    polarities <- reactive({
      
      withProgress(
        message = "Making the calculations",
        detail = "May take a minute or two...", 
        value = 0, 
        {
          aux <- x %>% 
            experienceAnalysis::calc_sentiment_indicators(
              sys_setenv = sys_setenv,
              which_python = which_python,
              which_venv = which_venv,
              venv_name = venv_name, 
              text_col_name = text_col
            ) %>% 
            dplyr::mutate(text_blob_polarity = round(text_blob_polarity, 3)) %>% 
            dplyr::bind_cols(
              x %>% 
                dplyr::select(
                  dplyr::all_of(c(text_col, target_label, target_criticality))
                )
            ) %>% 
            dplyr::select(names(reactable_columns))
        }
      )
      
    })
    
    output$textBlobBox <- renderText({
      HTML("<u><a target='_blank' rel='noopener noreferrer' href='https://textblob.readthedocs.io/en/dev/index.html'>
          Polarity</a></u> is a way to calculate how positive or negative a 
       comment is. It ranges between -1 (very negative) to 
       1 (very positive).")
    })
    
    output$textBlob <- reactable::renderReactable({
      
      polarities() %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(target_label),
            ~ . %in% input$class
          )
        ) %>% 
        reactable::reactable(
          columns = reactable_columns,
          #wrap = FALSE,
          filterable = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          defaultPageSize = 100,
          pageSizeOptions = 100
        )
    })
    
    output$downloadTextBlob <- downloadHandler(
      filename = function() {"polarities.csv"},
      content = function(file) {
        write.csv(polarities(), file)
      }
    )
    
    output$classControl <- renderUI({
      
      choices <- sort(unique(polarities()[[target_label]]))
      
      selectInput(
        session$ns("class"), 
        "Choose a class to see the polarities for this class:",
        choices = choices,
        selected = choices[1],
        multiple = TRUE
      )
    })
  })
}
    
## To be copied in the UI
# mod_sentiment_analysis_textblob_polarity_ui("sentiment_analysis_textblob_polarity_ui_1")
    
## To be copied in the server
# mod_sentiment_analysis_textblob_polarity_server("sentiment_analysis_textblob_polarity_ui_1")
