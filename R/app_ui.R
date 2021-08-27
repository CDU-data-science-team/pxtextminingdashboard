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
      
      dashboardHeader(
        title = NULL # Add header with JavaScript inside dashboardBody instead. See https://stackoverflow.com/questions/45176030/add-text-on-right-of-shinydashboard-header
      ),
      
      dashboardSidebar(
        
        sidebarMenu(
          
          menuItem(
            "Text Classification", 
            tabName = "tab_text_classification", 
            icon = icon("dashboard")
          ),
          
          menuItem(
            "Sentiment Analysis", 
            tabName = "tab_sentiment", 
            icon = icon("dashboard")
          ),
          
          menuItem(
            "Words analysis", 
            tabName = "tab_tfidf", 
            icon = icon("dashboard")
          )
          
          #menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        
        tags$head(
          tags$style(
            HTML(
              '.myClass { 
                font-size: 20px;
                line-height: 50px;
                text-align: left;
                font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                padding: 0 15px;
                overflow: hidden;
                color: white;
              }'
            )
          )
        ),
        
        tags$script(
          HTML(
            '$(document).ready(function() {
              $("header").find("nav").append(\'<span class="myClass"> Text Mining of Patient Feedback </span>\');
            })'
          )
        ),
        
        # Can't use inst/app/www/github_link.html with rendering- readLines tries 
        # to render nothing and as a consequence it displays a '"' on every tab 
        # of the dashboard. Ugly but working solution is to copy script from
        # github_link.html in here directly.
        # HTML(readLines("inst/app/www/github_link.html")),
        HTML('<a target="_blank" rel="noopener noreferrer" 
             href="https://github.com/CDU-data-science-team/pxtextminingdashboard" 
             class="github-corner" aria-label="View source on GitHub"><svg width="80" 
             height="80" viewBox="0 0 250 250" style="fill:black; color:#fff; 
             position: absolute; z-index: 100; top: 50; border: 0; right: 0;" 
             aria-hidden="true"><path d="M0,0 L115,115 L130,115 L142,142 L250,250 
             L250,0 Z"></path><path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 
             C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 
             C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" 
             fill="currentColor" style="transform-origin: 130px 106px;" 
             class="octo-arm"></path><path d="M115.0,115.0 C114.9,115.1 118.7,116.5 
             119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 
             127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 
             163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 
             187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 
             216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 
             203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 
             156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 
             Z" fill="currentColor" class="octo-body"></path></svg></a><style>.github-corner:hover 
             .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes 
             octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}
             40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover 
             .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms 
             ease-in-out}}</style>'),
        
        tabItems(
          
          tabItem(
            
            tabName = "tab_text_classification",
            
            tabsetPanel(
              
              tabPanel(
                "Label",
                
                tabsetPanel(
                  
                  tabPanel(
                    "Predictions",
                    mod_predictions_unlabelled_data_ui("predictions_unlabelled_data_ui_1")
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
                    "Predictions",
                    mod_predictions_unlabelled_data_ui("predictions_unlabelled_data_ui_2")
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
                "Sentiment summaries - theme-level", 
                mod_sentiment_analysis_tag_level_ui("sentiment_analysis_tag_level_ui_1")
              ),
              
              tabPanel(
                "Sentiment breakdown - text-level",
                mod_sentiment_analysis_nrc_sentiment_breakdown_ui("sentiment_analysis_nrc_sentiment_breakdown_ui_1")
              ),
              
              tabPanel(
                "Polarity - text-level",
                mod_sentiment_analysis_textblob_polarity_ui("sentiment_analysis_textblob_polarity_ui_1")
              )
            )
          ),
          
          tabItem(
            
            tabName = "tab_tfidf",
            
            tabsetPanel(
              
              tabPanel(
                "Label",
                
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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'pxtextminingdashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

