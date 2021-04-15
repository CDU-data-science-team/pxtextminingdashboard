#' performance_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_performance_metrics_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 12,
        box(
          title = "Best-perfroming learners",
          width = NULL,
          plotOutput(ns("learnerPerformance")),
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
          width = 12,
          title = "Technical - all tuned (hyper) parameters",
          reactable::reactableOutput(ns("rawMetrics"))
        )
      )
    )
  )
}
    
#' performance_metrics Server Functions
#'
#' @noRd 
mod_performance_metrics_server <- function(id, x){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$learnerPerformance <- renderPlot({
      
      x %>% 
        experienceAnalysis::get_best_estimators() %>% 
        experienceAnalysis::plot_best_estimators()
    })
    
    output$rawMetrics <- reactable::renderReactable({
      
      x %>%
        experienceAnalysis::get_all_pipeline_tuning_results() %>%
        reactable::reactable()
      
    })
    
    output$modelPerformanceBox <- renderText({
      
      HTML(paste0("Best-performing learners for 
          <u><a target='_blank' rel='noopener noreferrer' href='https://lib.dr.iastate.edu/etd/13537/'>
          Class Balance Accuracy</a></u>, ordered from highest to lowest score.
          Plotted values are the mean scores from a 5-fold CV on the
          training set, for the best hyperparameter values for each learner.
          The leftmost learner is used to make the predictions."))
    })
 
  })
}
