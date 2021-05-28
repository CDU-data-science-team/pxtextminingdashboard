#' performance_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_performance_metrics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 12,
        box(
          title = "Best-perfroming learners",
          width = NULL,
          plotOutput(ns("learnerPerformance")),
          plotOutput(ns("confusionMatrix")),
          box(
            width = NULL, 
            background = "red",
            htmlOutput(ns("modelPerformanceBox"))
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL,
          title = 
            HTML(
            "<p>All tuned (hyper) parameters</pr>
            <p style='font-size:small;'> Lists all (hyper)parameter values tried 
            during pipeline fitting, along with performance metrics.</pr>
            <p style='font-size:small;'> This table was generated from the 
            <a target='_blank' rel='noopener noreferrer' href='https://scikit-learn.org/stable/index.html'>Scikit-learn</a> 
            output that follows pipeline fitting. It was derived from attribute 
            <a target='_blank' rel='noopener noreferrer' href='https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.RandomizedSearchCV.html'>'cv_results_'</a>
            with some modifications.</pr>"
            ),
          downloadButton(ns("downloadData"), "Download Data"),
          reactable::reactableOutput(ns("rawMetrics"))
        )
      )
    )
  )
}
    
#' performance_metrics Server Functions
#'
#' @noRd 
mod_performance_metrics_server <- function(id, x, target, target_pred, groups, 
                                           preds, row_indices, tuning_results) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    output$learnerPerformance <- renderPlot({
      
      tuning_results %>% 
        experienceAnalysis::prep_best_estimators() %>% 
        experienceAnalysis::plot_best_estimators()
    })
    
    output$confusionMatrix <- renderPlot({
      
      x %>%
        dplyr::right_join(preds, by = "row_index") %>% 
        dplyr::select(dplyr::all_of(c(target, target_pred, groups))) %>% 
        experienceAnalysis::plot_confusion_matrix(
          target_col_name = target,
          target_pred_col_name = target_pred,
          grouping_variables = NULL,
          type = "heatmap"
        )
    })
    
    output$rawMetrics <- reactable::renderReactable({
      
      metrics_table <- tuning_results %>%
        experienceAnalysis::prep_all_pipeline_tuning_results()
      cols <- names(metrics_table)
      
      metrics_table %>%
        reactable::reactable(
          height = 1000, # Fix the height to make table scrollable and headers sticky.
          columns = # Fix max column width.
            lapply(.,
              function(cols) {
                reactable::colDef(maxWidth = 201)
              }
            ),
          defaultColDef = # Insert footer.
            reactable::colDef(
              footer = function(values, name) {
                div(name, style = list(fontWeight = 600))
              }
          )
        )
    })
    
    output$modelPerformanceBox <- renderText({
      
      HTML(paste0("Best-performing learners for 
          <u><a target='_blank' rel='noopener noreferrer' href='https://lib.dr.iastate.edu/etd/13537/'>
          Class Balance Accuracy</a></u>, ordered from highest to lowest score.
          Plotted values are the mean scores from a 5-fold CV on the
          training set, for the best hyperparameter values for each learner.
          The leftmost learner is used to make the predictions."))
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {paste0("performance_metrics_", target, ".csv")},
      content = function(file) {
        write.csv(tuning_results, file)
      })
  })
}
