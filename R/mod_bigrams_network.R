#' bigrams_network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bigrams_network_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      width = 12,
      column(
        width = 12,
        uiOutput(ns("classControl")),
        box(
          width = NULL,
          plotOutput(ns("bigrams_network")),
          box(
            htmlOutput(ns("tfidfExplanation")), 
            background = 'red', 
            width = NULL
          )
        )
      )
    )
  )
}
    
#' bigrams_network Server Functions
#'
#' @noRd 
mod_bigrams_network_server <- function(id, x, label, predictor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$bigrams_network <- renderPlot({
      
      bigrams_network_plot(x, label = input$pred, y = predictor)
    })
    
    output$tfidfExplanation <- renderText({
      HTML(paste0("*TF-IDF stands for
          <u><a href='https://en.wikipedia.org/wiki/Tf%E2%80%93idf'>
          Term Frequency–Inverse Document Frequency</a></u>.
          It is a standard way of calculating the frequency (i.e. importance)
          of a word in the given text. It is a little more sophisticated than
          standard frequency as it adjusts for words that appear too frequently
          in the text. For example, stop words like ", "\"", "a", "\"", " and ",
                  "\"", "the", "\"", " are very frequent but uniformative of
          the cotent of the text."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("pred"), 
        "Choose a label:",
        choices = sort(unique(x[, predictor])),
        selected = sort(unique(x[, predictor]))[1]
      )
    })
  })
}