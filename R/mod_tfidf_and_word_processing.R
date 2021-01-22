#' tfidf_and_word_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tfidf_and_word_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      width = 12,
      column(
        width = 12,
        uiOutput(ns("classControl")),
        box(
          width = NULL,
          plotOutput(ns("tfidf_bars")),
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
    
#' tfidf_and_word_processing Server Functions
#'
#' @noRd 
mod_tfidf_and_word_processing_server <- function(id, x, label){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$tfidf_bars <- renderPlot({
      
      tfidf_unigrams(x, label = input$pred)
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
        choices = sort(unique(x$super)),
        selected = sort(unique(x$super))[1]
      )
    })
  })
}