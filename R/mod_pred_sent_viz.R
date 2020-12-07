#' pred_sent_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pred_sent_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      column(width = 6,
             box(
               width = NULL,
               shiny::textOutput(ns("hallo"))
             )
      ),
      
      column(width = 6,
             box(
               width = NULL,
               shiny::textOutput(ns("hallo2"))
             )
      )
    )
 
  )
}
    
#' pred_sent_viz Server Functions
#'
#' @noRd 
mod_pred_sent_viz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$hallo <- renderText(print("hallo andreas"))
    output$hallo2 <- renderText(print("hallo milan, how are you?"))
 
  })
}
    
## To be copied in the UI
# mod_pred_sent_viz_ui("pred_sent_viz_ui_1")
    
## To be copied in the server
# mod_pred_sent_viz_server("pred_sent_viz_ui_1")
