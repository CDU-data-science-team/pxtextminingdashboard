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
      column(width = 6,
             uiOutput(ns("classControl"))
      ),
      
      column(width = 6,
             uiOutput(ns("bigramsPropControl"))
      )
    ),
    
    fluidRow(
      width = 12,
      column(
        width = 12,
        box(
          width = NULL,
          plotOutput(ns("bigramsNetwork")),
          box(
            htmlOutput(ns("bigramsNetworkExplanation")), 
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
    
    output$bigramsNetwork <- renderPlot({
      
      bigrams_network_plot(x, label = input$pred, y = predictor, 
                           bigrams_prop = input$bigramsProp)
    })
    
    output$bigramsNetworkExplanation <- renderText({
      HTML(paste0("This plot shows the top-<i>X</i>% (<i>X</i> is 
                  user-specified) most frequent pairs of consecutive words in 
                  the feedback text. The arrows point towards the second word 
                  in each pair. For example, if ", 
                  "\"", "snack time", "\"",
                  "is a frequent pair of consecutive words, then it will show 
                  on the graph as ", 
                  "\"", "snack -> time", "\"", 
                  ". <br><b>NOTE: </b>A <i>X</i> value of 1%-20% will normally 
                  reveal sufficient information. Higher values will produce 
                  an over-populated plot."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("pred"), 
        "Choose a label:",
        choices = sort(unique(unlist(x[,predictor]))),
        selected = sort(unique(unlist(x[,predictor])))[1]
      )
    })
    
    output$bigramsPropControl <- renderUI({
      
      sliderInput(
        session$ns("bigramsProp"),
        label = HTML("<b>Proportion (%) of most frequent bigrams:</b>"),
        value = NULL,
        min = 1,
        max = 50
      )
    })
  })
}