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
      column(12,
             box(width = NULL, background = "red",
                 htmlOutput(ns("textBlobBox"))
             )
      )
    ),
    
    fluidRow(
      column(width = 12,  
             box(width = NULL,
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
                                                            text_col_name) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$textBlobBox <- renderText({
      HTML("<u><a target='_blank' rel='noopener noreferrer' href='https://textblob.readthedocs.io/en/dev/index.html'>
          Polarity</a></u> is a way to calculate how positive or negative a 
       comment is. It ranges between -1 (very negative) to 
       1 (very positive).")
    })
    
    sticky_style <- list(position = "sticky", left = 0, 
                         background = "#fff", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    output$textBlob <- reactable::renderReactable({
      
      aux <- x %>% 
        experienceAnalysis::calc_sentiment_indicators(
          sys_setenv = sys_setenv,
          which_python = which_python,
          which_venv = which_venv,
          venv_name = venv_name, 
          make_table = TRUE,
          text_col_name = text_col
        )
      
      reactable::reactable(
        aux,
        columns = list(
          feedback = reactable::colDef(
            name = "Feedback",
            style = sticky_style,
            headerStyle = sticky_style,
            minWidth = 300
          ),

          polarity = reactable::colDef(
            name = "Polarity",
            align = "right",
            class = "border-left cell number"
          ),

          organization = reactable::colDef(
            name = "Organization",
            align = "right"
          ),

          label = reactable::colDef(
            name = "Label",
            align = "right",
            minWidth = 120
          ),

          criticality = reactable::colDef(
            name = "Criticality",
            align = "right"
          )
        ),

        #wrap = FALSE,
        filterable = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 100,
        pageSizeOptions = 100
      )
    })
  })
}
    
## To be copied in the UI
# mod_sentiment_analysis_textblob_polarity_ui("sentiment_analysis_textblob_polarity_ui_1")
    
## To be copied in the server
# mod_sentiment_analysis_textblob_polarity_server("sentiment_analysis_textblob_polarity_ui_1")
