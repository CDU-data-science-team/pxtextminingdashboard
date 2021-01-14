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
  mod_tidytext_server("tidytext_ui_1")
  mod_text_blob_server("text_blob_ui_1")
  mod_tfidf_and_word_processing_server("tfidf_and_word_processing_ui_1")
}
