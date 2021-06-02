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
    target = "label",
    target_pred = "label_pred",
    text_col = "feedback",
    groups = "organization",
    preds = predictions_test_label,
    row_indices = row_index_label
  )
  
  mod_predictions_table_server(
    "predictions_table_ui_2",
    x = text_data,
    target = "criticality",
    target_pred = "criticality_pred",
    text_col = "feedback",
    groups = "organization",
    preds = predictions_test_criticality,
    row_indices = row_index_criticality
  )
  
  #############################################################################
  mod_sentiment_analysis_tag_level_server(
    "sentiment_analysis_tag_level_ui_1",
    x = text_data,
    target = "label",
    text_col = "feedback",
    groups = "organization"
  )
  
  mod_sentiment_analysis_nrc_sentiment_breakdown_server(
    "sentiment_analysis_nrc_sentiment_breakdown_ui_1",
    x = text_data,
    target = "label",
    text_col = "feedback",
    groups = "organization"
  )
  
  mod_sentiment_analysis_textblob_polarity_server(
    "sentiment_analysis_textblob_polarity_ui_1",
    x = text_data,
    sys_setenv = "C:/Users/andreas.soteriades/Anaconda3/envs/textminingpy38/python.exe", 
    which_python = "C:/Users/andreas.soteriades/Anaconda3/envs/textminingpy38/python.exe", 
    which_venv = "conda",
    venv_name = "textminingpy38", 
    text_col = "feedback"
  )
  
  #############################################################################
  mod_tfidf_server(
    "tfidf_ui_1",
    x = text_data,
    target = "label",
    text_col = "feedback",
    groups = "organization"
  )
  
  mod_tfidf_server(
    "tfidf_ui_2",
    x = text_data,
    target = "criticality",
    text_col = "feedback",
    groups = "organization"
  )
  
  #############################################################################
  mod_performance_metrics_server(
    "performance_metrics_ui_1",
    x = text_data,
    target = "label",
    target_pred = "label_pred",
    text_col = "feedback",
    groups = "organization",
    preds = predictions_test_label,
    row_indices = row_index_label,
    tuning_results = tuning_results_label
  )
  
  mod_performance_metrics_server(
    "performance_metrics_ui_2",
    x = text_data,
    target = "criticality",
    target_pred = "criticality_pred",
    text_col = "feedback",
    groups = "organization",
    preds = predictions_test_criticality,
    row_indices = row_index_criticality,
    tuning_results = tuning_results_criticality
  )
  
  #############################################################################
  mod_bigrams_network_server(
    "bigrams_network_ui_1",
    x = text_data,
    target = "label",
    text_col = "feedback",
    groups = "organization"
  )
  
  mod_bigrams_network_server(
    "bigrams_network_ui_2",
    x = text_data,
    target = "criticality",
    text_col = "feedback",
    groups = "organization"
  )
}
