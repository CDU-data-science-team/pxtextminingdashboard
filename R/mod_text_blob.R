#' text_blob UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_blob_ui <- function(id){
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
mod_text_blob_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$textBlobBox <- renderText({
      HTML("<u><a href='https://textblob.readthedocs.io/en/dev/index.html'>
          Polarity</a></u> is a way to calculate how positive or negative a 
       comment is. It ranges between -1 (very negative) to 
       1 (very positive).")
    })
    
    sticky_style <- list(position = "sticky", left = 0, 
                         background = "#fff", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    output$textBlob <- reactable::renderReactable({
      
      reactable::reactable(
        text_blob_scores %>%
          dplyr::filter(super != "Couldn't be improved") %>%
          dplyr::select(improve, polarity) %>%
          dplyr::mutate_at('polarity',  ~ round(., 2)),
        columns = list(
          improve = reactable::colDef(
            name = "Feedback",
            style = sticky_style,
            headerStyle = sticky_style,
            minWidth = 300
          ),
          
          polarity = reactable::colDef(
            name = "Polarity",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500")
          )
        ),
        
        #wrap = FALSE,
        #filterable = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 100,
        pageSizeOptions = 100
      )
    })
  })
}
    
## To be copied in the UI
# mod_text_blob_ui("text_blob_ui_1")
    
## To be copied in the server
# mod_text_blob_server("text_blob_ui_1")
