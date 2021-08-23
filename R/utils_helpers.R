clean_text <- function(x) {
  
  x %>% 
    stringr::str_replace_all("[^[:alnum:]]", " ") %>% # Keep only alphanumeric characters
    stringr::str_replace_all("[ \t\n\r\v\f]", "_") %>% # No whitespaces
    tolower()
}

################################################################################
# Functions for organizing and presenting the tuning results of the ML model #
################################################################################
#' Plot the best `Scikit-learn` (Python) estimators
#'
#' For internal use only!
#'
#' @param best_estimators The result of \code{\link{prep_best_estimators}}.
#'
#' @return A `ggplot2::geom_col`.
#' @export
#'
#' @examples

plot_best_estimators <- function(best_estimators) {
  
  p <- best_estimators %>%
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
  
  return(p)
}

#' Prepare table with all (hyper)parameter tunings
#'
#' For internal use only! Tidy up a "tuning_results" table from
#' [`pxtextmining`](https://github.com/CDU-data-science-team/pxtextmining).
#'
#' @param x A "tuning_results" data frame.
#'
#' @return A data frame.
#' @export
#'
#' @examples

prep_all_pipeline_tuning_results <- function(x) {
  
  pipeline_tuning_results <- x %>%
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
  
  return(pipeline_tuning_results)
}

#' Prepare table with the best (hyper)parameter tunings for each learner
#'
#' For internal use only! Get the best-scoring (hyper)parameters for each
#' learner based on Class Balance Accuracy.
#'
#' @param x A "tuning_results" data frame. See
#'     \code{\link{prep_all_pipeline_tuning_results}}.
#'
#'@note See \code{\link{prep_all_pipeline_tuning_results}}.
#'
#' @return A data frame.
#' @export
#'
#' @examples

prep_best_estimators <- function(x) {
  
  best_estimators <- x %>%
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
      )
    )
  
  return(best_estimators)
}


################################################################################
# Plot functions #
################################################################################
plotBigramsNetwork <- function(x) {
  
  x %>% 
    experienceAnalysis::plot_bigrams_network()
}

plotBingWordCounts <- function(x) {
  
  x %>%
    experienceAnalysis::plot_bing_word_counts()
}

plotConfusionMatrix <- function(x, ...) {
  
  x %>% 
    experienceAnalysis::plot_confusion_matrix(type = "heatmap", ...)
}

plotLearnerPerformance <- function(x) {
  
  plot_best_estimators(x)
}

plotNetSentiment <- function(x, ...) {
  
  x %>%
    experienceAnalysis::plot_net_sentiment_per_tag(...)
}

plotTfidfNgrams <- function(x, ...) {
  
  x %>% 
    experienceAnalysis::plot_tfidf_ngrams(...)
}