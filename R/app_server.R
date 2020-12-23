#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_predictions_table_server("predictions_table_ui_1")
  mod_predictions_table_criticality_server("predictions_table_criticality_ui_1")
  mod_sentiment_analysis_server("sentiment_analysis_ui_1")
  mod_pred_sent_viz_server("pred_sent_viz_ui_1")
  mod_text_blob_server("text_blob_ui_1")
}
