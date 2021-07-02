#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  mod_predictions_unlabelled_data_server(
    "predictions_unlabelled_data_ui_1",
    x = get(get_golem_config("dataset_text")), 
    python_setup = as.logical(get_golem_config("python_setup")),
    sys_setenv = get_golem_config("sys_setenv"),
    which_python = get_golem_config("which_python"), 
    which_venv = get_golem_config("which_venv"), 
    venv_name = get_golem_config("venv_name"), 
    text_col = get_golem_config("var_text_col"), 
    preds_column = get_golem_config("column_names_predictions"),
    column_names = purrr::map_chr(
      paste0("column_names_", c("text", "organization")), 
      ~ get_golem_config(.x)
    ),
    pipe_path = get_golem_config("pipeline_path_label")
  )
  
  mod_predictions_unlabelled_data_server(
    "predictions_unlabelled_data_ui_2",
    x = get(get_golem_config("dataset_text")), 
    python_setup = as.logical(get_golem_config("python_setup")),
    sys_setenv = get_golem_config("sys_setenv"),
    which_python = get_golem_config("which_python"),
    which_venv = get_golem_config("which_venv"), 
    venv_name = get_golem_config("venv_name"),
    text_col = get_golem_config("var_text_col"),
    preds_column = get_golem_config("column_names_predictions"),
    column_names = purrr::map_chr(
      paste0("column_names_", c("text", "organization")), 
      ~ get_golem_config(.x)
    ),
    pipe_path = get_golem_config("pipeline_path_criticality")
  )
  
  #############################################################################
  mod_predictions_table_server(
    "predictions_table_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    target_pred = get_golem_config("var_target_pred_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1"),
    preds = get(get_golem_config("dataset_preds_label")),
    row_indices = get(get_golem_config("dataset_row_index_label"))
  )
  
  mod_predictions_table_server(
    "predictions_table_ui_2",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_criticality"),
    target_pred = get_golem_config("var_target_pred_criticality"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1"),
    preds = get(get_golem_config("dataset_preds_criticality")),
    row_indices = get(get_golem_config("dataset_row_index_criticality"))
  )
  
  #############################################################################
  mod_sentiment_analysis_tag_level_server(
    "sentiment_analysis_tag_level_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
  
  mod_sentiment_analysis_nrc_sentiment_breakdown_server(
    "sentiment_analysis_nrc_sentiment_breakdown_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
  
  mod_sentiment_analysis_textblob_polarity_server(
    "sentiment_analysis_textblob_polarity_ui_1",
    x = get(get_golem_config("dataset_text")), 
    sys_setenv = get_golem_config("sys_setenv"),
    which_python = get_golem_config("which_python"), 
    which_venv = get_golem_config("which_venv"), 
    venv_name = get_golem_config("venv_name"), 
    text_col = get_golem_config("var_text_col")
  )
  
  #############################################################################
  mod_tfidf_server(
    "tfidf_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
  
  mod_tfidf_server(
    "tfidf_ui_2",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_criticality"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
  
  #############################################################################
  mod_performance_metrics_server(
    "performance_metrics_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    target_pred = get_golem_config("var_target_pred_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1"),
    preds = get(get_golem_config("dataset_preds_label")),
    row_indices = get(get_golem_config("dataset_row_index_label")),
    tuning_results = get(get_golem_config("dataset_tuning_results_label"))
  )
  
  mod_performance_metrics_server(
    "performance_metrics_ui_2",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_criticality"),
    target_pred = get_golem_config("var_target_pred_criticality"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1"),
    preds = get(get_golem_config("dataset_preds_criticality")),
    row_indices = get(get_golem_config("dataset_row_index_criticality")),
    tuning_results = get(get_golem_config("dataset_tuning_results_criticality"))
  )
  
  #############################################################################
  mod_bigrams_network_server(
    "bigrams_network_ui_1",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_label"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
  
  mod_bigrams_network_server(
    "bigrams_network_ui_2",
    x = get(get_golem_config("dataset_text")), 
    target = get_golem_config("var_target_criticality"),
    text_col = get_golem_config("var_text_col"),
    groups = get_golem_config("var_groups_1")
  )
}
