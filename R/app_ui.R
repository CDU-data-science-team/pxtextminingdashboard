#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "Patient feedback and its predicted label"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Text Classification", tabName = "tab_text_classification", 
                   icon = icon("dashboard")),
          menuItem("Sentiment Analysis", tabName = "tab_sentiment", 
                   icon = icon("dashboard")),
          menuItem("TF-IDF & Bigrams", tabName = "tab_tfidf", 
                   icon = icon("dashboard"))
          #menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          
          tabItem(
            
            tabName = "tab_text_classification",
            
            tabsetPanel(
              
              tabPanel(
                "Super",
                
                tabsetPanel(
                  
                  tabPanel(
                    "Predicted text",
                    mod_predictions_table_ui("predictions_table_ui_1")
                  ),
                  
                  tabPanel(
                    "Model performance",
                    mod_performance_metrics_ui("performance_metrics_ui_1")
                  )
                )
              ),
              
              tabPanel(
                "Criticality",
                
                tabsetPanel(
                  
                  tabPanel(
                    "Predicted text",
                    mod_predictions_table_ui("predictions_table_ui_2")
                  ),
                  
                  tabPanel(
                    "Model performance",
                    mod_performance_metrics_ui("performance_metrics_ui_2")
                  )
                )
              )
            )
          ),
          
          tabItem(
            
            tabName = "tab_sentiment",
            
            tabBox(
              width = 12,
              #title = "tabs",
              
              tabPanel(
                "Sentiment - label", 
                mod_sentiment_analysis_tag_level_ui("sentiment_analysis_tag_level_ui_1")
              ),
              
              tabPanel(
                "Sentiment - text",
                mod_sentiment_analysis_nrc_sentiment_breakdown_ui("sentiment_analysis_nrc_sentiment_breakdown_ui_1")
              ),
              
              tabPanel(
                "Polarity - text",
                mod_sentiment_analysis_textblob_polarity_ui("sentiment_analysis_textblob_polarity_ui_1")
              )
            )
          ),
          
          tabItem(
            
            tabName = "tab_tfidf",
            
            tabsetPanel(
              
              tabPanel(
                "Super",
                
                tabsetPanel(
                  
                  tabPanel(
                    "TF-IDF",
                    mod_tfidf_ui("tfidf_ui_1")
                  ),
                  
                  tabPanel(
                    "Network of bigrams",
                    mod_bigrams_network_ui("bigrams_network_ui_1")
                  )
                )
              ),
              
              tabPanel(
                "Criticality",
                
                tabsetPanel(
                  
                  tabPanel(
                    "TF-IDF",
                    mod_tfidf_ui("tfidf_ui_2")
                  ),
                  
                  tabPanel(
                    "Network of bigrams",
                    mod_bigrams_network_ui("bigrams_network_ui_2")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'positiveAboutChangeTextClassificationDashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

