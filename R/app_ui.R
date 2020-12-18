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
          menuItem("Predictions - tag", tabName = "tab_predictions", 
                   icon = icon("dashboard")),
          menuItem("Predictions - criticality", 
                   tabName = "tab_predictions_criticality", 
                   icon = icon("dashboard")),
          menuItem("Sentiment", tabName = "tab_sentiment", 
                   icon = icon("dashboard"))
          #menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          
          tabItem(tabName = 'tab_predictions',
                  mod_predictions_table_ui("predictions_table_ui_1")
          ),
          
          tabItem(tabName = 'tab_predictions_criticality',
                  mod_predictions_table_criticality_ui("predictions_table_criticality_ui_1")
          ),
          
          tabItem(tabName = 'tab_sentiment',
                  tabBox(
                    width = 12,
                    #title = "tabs",
                    tabPanel(
                      "My stuff", 
                      mod_sentiment_analysis_ui("sentiment_analysis_ui_1")),
                    tabPanel(
                      "N&J",
                      mod_pred_sent_viz_ui("pred_sent_viz_ui_1"))
                  )
          )#,
          
          #tabItem(tabName = 'tab_sentiment',
          #        mod_sentiment_analysis_ui("sentiment_analysis_ui_1")
          #)
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

