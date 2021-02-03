plot_learner_metrics <- function(x) {
  
  x %>%
    dplyr::mutate(learner = sub("\\(.*", "", param_clf__estimator)) %>%
    dplyr::group_by(learner) %>%
    dplyr::arrange(
      dplyr::desc(tidyselect::all_of("mean_test_Class Balance Accuracy"))
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("mean_test"))) %>%
    dplyr::mutate(
      name = sub("mean_test.", "", name),
      # Auxiliary column to order learners by CBA in bar plots
      aux = dplyr::case_when(
        name %in% "Class Balance Accuracy" ~ value,
        TRUE ~ -1
      )#,
      # tooltip_text = HTML(paste0(
      #   "<b>Learner: </b>", learner, "<br>",
      #   "<b>", name, ": </b>", value, "<br>",
      #   "<b>% features kept:</b> ", param_selectperc__percentile, "<br>",
      #   "<b>% Class-balancing:</b> ", param_sampling__kw_args, "<br>",
      #   "<b>Use IDF: </b>", param_preprocessor__text__tfidf__use_idf, "<br>",
      #   "<b>TFIDF norm: </b>", param_preprocessor__text__tfidf__norm, "<br>",
      #   "<b>Min DF: </b>", param_preprocessor__text__tfidf__min_df, "<br>",
      #   "<b>Max DF: </b>", param_preprocessor__text__tfidf__max_df, "<br>",
      # ))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(value, 
                                 reorder(learner, -aux), 
                                 fill = name)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.6) +
    ggplot2::coord_flip() + 
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 12, angle = 90, hjust = 0.95, vjust = 0.2
      ),
      axis.text.y = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12)
    ) + 
    ggthemes::scale_fill_colorblind()
}

table_learner_metrics <- function(x) {
  
  x %>%
    dplyr::select(
      -tidyselect::any_of("X1"), 
      -dplyr::starts_with(c("split", "rank")),
      -params,
      -param_clf__estimator,
    ) %>%
    dplyr::select(
      learner,
      dplyr::starts_with("param"),
      dplyr::contains(c("class balance", "balanced", "matthews"), 
                      ignore.case = TRUE),
      dplyr::ends_with("_Accuracy")
    ) %>%
    dplyr::arrange(
      dplyr::desc(tidyselect::all_of("mean_test_Class Balance Accuracy"))
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), 
                                ~ round(., 2)))
}