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
        width = 6,
        
        box(
          title = "Confusion matrix",
          width = NULL,
          
          downloadButton(ns("downloadConfusionMatrix"), "Download plot"),
          
          plotOutput(ns("confusionMatrix")),
          
          box(
            width = NULL, 
            background = "red",
            
            HTML(
              "The <a target='_blank' rel='noopener noreferrer' href='https://en.wikipedia.org/wiki/Confusion_matrix'>Confusion Matrix</a> 
              is a way to visually detect which classes the model has performed
              best/worst in predicting. The actual classes are on the x-axis and
              the predicted ones are on the y-axis. With a good model, we would
              see most records on the diagonal. The integers correspond to the 
              number of records belonging to the class on the x-axis that were 
              predicted as the class on the y-axis. The shades of gray translate
              these counts into proportions of the total number of records in 
              the class on the x-axis. With a good model, the darker shades of
              gray would be on the diagonal. The shades help spot where the most
              severe misclassifications may have occurred. They thus help see if
              the model is consistently confusing the text for a class on the
              x-axis as being about a different class on the y-axis."
            )
          )
        )
      ),
      
      column(
        width = 6,
        
        box(
          title = "Best-performing learners",
          width = NULL,
          
          downloadButton(ns("downloadLearnerPerformance"), "Download plot"),
          
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
          title = "Predictions on the test set",
          width = NULL,
          
          box(
            width = NULL, 
            background = "red",
            htmlOutput(ns("modelAccuracyBox"))
          ),
          
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("classControl"))
            )
          ),
          
          downloadButton(ns("downloadPredictionsOnTest"), "Download data"),
          
          reactable::reactableOutput(ns("predictedClasses")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE)
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
          
          downloadButton(ns("downloadPerformanceMetrics"), "Download data"),
          
          reactable::reactableOutput(ns("rawMetrics"))
        )
      )
    )
  )
}
    
#' performance_metrics Server Functions
#'
#' @noRd 
mod_performance_metrics_server <- function(id, x, target, target_pred, text_col,
                                           preds, row_indices, tuning_results) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    #######################
    # Learner performance #
    #######################
    dataLearnerPerformance <- tuning_results %>% 
      prep_best_estimators()
    
    output$learnerPerformance <- renderPlot({
      
      plotLearnerPerformance(dataLearnerPerformance)
    })
    
    ####################
    # Confusion matrix #
    ####################
    dataConfusionMatrix <- x %>%
      dplyr::right_join(preds, by = "row_index") %>% 
      dplyr::select(dplyr::all_of(c(target, target_pred)))
    
    output$confusionMatrix <- renderPlot({
      
      dataConfusionMatrix %>% 
        plotConfusionMatrix(target_col_name = target,
                            target_pred_col_name = target_pred)
    })
    
    #######################
    # Predictions on test #
    #######################
    dataReactable <- reactive(
      
      aux <- x %>%
        dplyr::right_join(preds, by = "row_index") %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(target_pred),
            ~ . %in% input$class
          )
        ) %>%
        dplyr::select(dplyr::all_of(c(text_col, target)))
    )
    
    output$predictedClasses <- reactable::renderReactable({
      
      feedback_col_new_name <- paste0(
        "Feedback that model predicted as ", "\"", input$class, "\""
      )
      
      reactable_cols <- list(
        reactable::colDef(name = feedback_col_new_name),
        reactable::colDef(name = "Actual class", align = "right")
      )
      names(reactable_cols) <- c(text_col, target)
      
      reactable::reactable(
        dataReactable(),
        # height = 1000, # Fix the height to make table scrollable and headers sticky.
        columns = reactable_cols,
        #rownames = TRUE,
        searchable = TRUE,
        sortable = FALSE,
        defaultPageSize = 20,
        pageSizeOptions = 20,
        language = reactable::reactableLang(
          searchPlaceholder = "Search for a word..."),
      )
    })
    
    ######################
    # Accuracy per class #
    ######################
    dataAccuracyScore <- reactive({
      
      x %>% 
        dplyr::select(dplyr::all_of(target), row_index) %>% 
        dplyr::right_join(preds, by = "row_index") %>% 
        experienceAnalysis::calc_accuracy_per_class(
          target_col_name = target, 
          target_pred_col_name = target_pred,
          column_names = NULL
        ) %>% 
        dplyr::filter(
          dplyr::across(
            dplyr::all_of(target),
            ~ . %in% input$class
          )
        ) %>%
        dplyr::select(accuracy) %>%
        dplyr::mutate(accuracy = round(accuracy * 100)) %>%
        dplyr::pull()
    })
    
    output$modelAccuracyBox <- renderText({
      
      HTML(paste0(
        "Learner accuracy for this class is ", dataAccuracyScore(), "%."))
    })
    
    ##################
    # Tuning results #
    ##################
    dataMetricsTable <- tuning_results %>%
      prep_all_pipeline_tuning_results()
    
    output$rawMetrics <- reactable::renderReactable({
      
      cols <- names(dataMetricsTable)
      
      dataMetricsTable %>%
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
    
    output$downloadConfusionMatrix <- downloadHandler(
      filename = function() {paste0("confusion_matrix_", target, ".pdf")},
      content = function(file) {
        ggplot2::ggsave(file, 
                        plot = plotConfusionMatrix(dataConfusionMatrix, 
                                                   target_col_name = target,
                                                   target_pred_col_name = target_pred
                                                   ), 
                        device = pdf, width = 10, height = 10, units = "in")
      }
    )
    
    output$downloadLearnerPerformance <- downloadHandler(
      filename = function() {paste0("learner_performance_", target, ".pdf")},
      content = function(file) {
        ggplot2::ggsave(file, 
                        plot = plotLearnerPerformance(dataLearnerPerformance), 
                        device = pdf, width = 12, units = "in")
      }
    )
    
    output$downloadPerformanceMetrics <- downloadHandler(
      filename = function() {paste0("performance_metrics_", target, ".csv")},
      content = function(file) {
        write.csv(dataMetricsTable, file)
      }
    )
    
    output$downloadPredictionsOnTest <- downloadHandler(
      filename = function() {paste0("performance_metrics_", target, ".csv")},
      content = function(file) {
        write.csv(
          x %>%
            dplyr::right_join(preds, by = "row_index") %>% 
            dplyr::select(dplyr::all_of(c(text_col, target))), 
          file
        )
      }
    )
    
    output$classControl <- renderUI({
      
      aux <- x %>%
        dplyr::right_join(row_indices, by = 'row_index')
      
      choices <- sort(unique(unlist(aux[[target]])))
      
      selectInput(
        session$ns("class"), 
        "Choose a class:",
        choices = choices,
        selected = choices[1]
      )
    })
  })
}
