#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import golem
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  #############################################################################
  # Variables for module functions (mod_*) #
  #############################################################################
  # Datasets
  x <- get(get_golem_config("dataset_text"))
  x_unlabelled <- get(get_golem_config("dataset_unlabelled"))
  preds_label <- get(get_golem_config("dataset_preds_label"))
  row_indices_label <- get(get_golem_config("dataset_row_index_label"))
  tuning_results_label <- get(get_golem_config("dataset_tuning_results_label"))
  preds_criticality <- get(get_golem_config("dataset_preds_criticality"))
  row_indices_criticality <- 
    get(get_golem_config("dataset_row_index_criticality"))
  tuning_results_criticality <- 
    get(get_golem_config("dataset_tuning_results_criticality"))
  
  # Columns
  target_label <- get_golem_config("var_target_label")
  target_pred_label <- get_golem_config("var_target_pred_label")
  target_criticality <- get_golem_config("var_target_criticality")
  target_pred_criticality <- get_golem_config("var_target_pred_criticality")
  text_col <- get_golem_config("var_text_col")
  # The below is an interesting case. We want preds_column = NULL in 
  # pxtextmineR::factory_predict_unlabelled_text_r. We therefore use a mock 
  # YAML object column_names_predictions, but we never list it in the YAML file. 
  # This means that get_golem_config("column_names_predictions") will have 
  # nothing to get, and so it will assign NULL to preds_column, which is what we 
  # want.
  preds_column <- get_golem_config("column_names_predictions")
  
  # Scikit-learn pipelines
  pipe_path_label <- get_golem_config(
    "pipeline_path_label", config = get_golem_options("where_am_i"))
  
  pipe_path_criticality <- get_golem_config(
    "pipeline_path_criticality", config = get_golem_options("where_am_i"))
  
  #############################################################################
  # Modules #
  #############################################################################
  mod_predictions_unlabelled_data_server(
    "predictions_unlabelled_data_ui_1",
    x = x_unlabelled,
    target = target_label,
    text_col,
    pipe_path = pipe_path_label,
    preds_column,
    column_names = text_col,
    theme = NULL
  )
  
  mod_predictions_unlabelled_data_server(
    "predictions_unlabelled_data_ui_2",
    x = x_unlabelled,
    target = target_criticality,
    text_col,
    pipe_path = pipe_path_criticality,
    preds_column,
    column_names = text_col,
    theme = target_label
  )
  
  #############################################################################
  mod_predictions_table_server(
    "predictions_table_ui_1",
    x, 
    target = target_label,
    target_pred = target_pred_label,
    text_col,
    preds = preds_label,
    row_indices = row_indices_label
  )
  
  mod_predictions_table_server(
    "predictions_table_ui_2",
    x, 
    target = target_criticality,
    target_pred = target_pred_criticality,
    text_col,
    preds = preds_criticality,
    row_indices = row_indices_criticality
  )
  
  #############################################################################
  mod_sentiment_analysis_tag_level_server(
    "sentiment_analysis_tag_level_ui_1",
    x, 
    target = target_label,
    text_col
  )
  
  mod_sentiment_analysis_nrc_sentiment_breakdown_server(
    "sentiment_analysis_nrc_sentiment_breakdown_ui_1",
    x, 
    target = target_label,
    text_col
  )
  
  mod_sentiment_analysis_textblob_polarity_server(
    "sentiment_analysis_textblob_polarity_ui_1",
    x, 
    text_col,
    target_label,
    target_criticality
  )
  
  #############################################################################
  mod_tfidf_server(
    "tfidf_ui_1",
    x, 
    target = target_label,
    text_col
  )
  
  mod_tfidf_server(
    "tfidf_ui_2",
    x, 
    target = target_criticality,
    text_col
  )
  
  #############################################################################
  mod_performance_metrics_server(
    "performance_metrics_ui_1",
    x, 
    target = target_label,
    target_pred = target_pred_label,
    text_col,
    preds = preds_label,
    row_indices = row_indices_label,
    tuning_results = tuning_results_label
  )
  
  mod_performance_metrics_server(
    "performance_metrics_ui_2",
    x, 
    target = target_criticality,
    target_pred = target_pred_criticality,
    text_col,
    preds = preds_criticality,
    row_indices = row_indices_criticality,
    tuning_results = tuning_results_criticality
  )
  
  #############################################################################
  mod_bigrams_network_server(
    "bigrams_network_ui_1",
    x, 
    target = target_label,
    text_col
  )
  
  mod_bigrams_network_server(
    "bigrams_network_ui_2",
    x, 
    target = target_criticality,
    text_col
  )
}
