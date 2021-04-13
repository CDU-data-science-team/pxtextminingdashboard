#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  #############################################################################
  mod_predictions_table_server(
    "predictions_table_ui_1",
    x = text_data,
    y = accuracy_per_class_label,
    target = "label"
  )
  
  mod_predictions_table_server(
    "predictions_table_ui_2",
    x = text_data,
    y = accuracy_per_class_criticality,
    target = "criticality"
  )
  
  #############################################################################
  mod_sentiment_analysis_tag_level_server("sentiment_analysis_tag_level_ui_1")
  
  mod_sentiment_analysis_nrc_sentiment_breakdown_server("sentiment_analysis_nrc_sentiment_breakdown_ui_1")
  
  mod_sentiment_analysis_textblob_polarity_server("sentiment_analysis_textblob_polarity_ui_1")
  
  #############################################################################
  mod_tfidf_server(
    "tfidf_ui_1",
    x = text_data,
    target = "label"
  )
  mod_tfidf_server(
    "tfidf_ui_2",
    x = text_data,
    target = "criticality"
  )
  
  #############################################################################
  mod_performance_metrics_server(
    "performance_metrics_ui_1",
    x = tuning_results_label
  )
  mod_performance_metrics_server(
    "performance_metrics_ui_2",
    x = tuning_results_criticality
  )
  
  #############################################################################
  mod_bigrams_network_server(
    "bigrams_network_ui_1",
    x = text_data,
    target = "label"
  )
  mod_bigrams_network_server(
    "bigrams_network_ui_2",
    x = text_data,
    target = "criticality"
  )
}
