#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_predictions_table_server("predictions_table_ui_1")
  mod_sentiment_analysis_server("sentiment_analysis_ui_1")
}
