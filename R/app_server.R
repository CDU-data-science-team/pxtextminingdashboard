#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_predictions_table_server(
    "predictions_table_ui_1",
    x = test_data,
    y = accuracy_per_class
  )
  mod_predictions_table_server(
    "predictions_table_ui_2",
    x = test_data_criticality,
    y = accuracy_per_class_criticality
  )
  mod_sentiment_analysis_server("sentiment_analysis_ui_1")
  mod_tidytext_server("tidytext_ui_1")
  mod_text_blob_server("text_blob_ui_1")
  mod_tfidf_and_word_processing_server(
    "tfidf_and_word_processing_ui_1",
    x = data_for_tfidf
  )
  mod_performance_metrics_server(
    "performance_metrics_ui_1",
    x = tuning_results_super
  )
  mod_performance_metrics_server(
    "performance_metrics_ui_2",
    x = tuning_results_criticality
  )
}
