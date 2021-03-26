#' tfidf_and_word_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tfidf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 4,
        uiOutput(ns("classControl"))
      ),
      
      column(
        width = 4,
        uiOutput(ns("organizationControl"))
      ),
      
      column(
        width = 4,
        uiOutput(ns("ngramsNumControl"))
      )
    ),
    
    fluidRow(
      width = 12,
      
      column(
        width = 12,
        
        box(
          width = NULL,
          
          plotOutput(ns("tfidf_bars"), height = "1000px") %>%
            shinycssloaders::withSpinner(hide.ui = TRUE),
          
          box(
            htmlOutput(ns("tfidfExplanation")), 
            background = "red", 
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
mod_tfidf_server <- function(id, x, predictor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_function <- reactive({
      
      req(input$class)
      req(input$ngramsType)
      
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a few seconds...', 
        value = 0, 
        {
          p <- tfidf_ngrams(x, y = predictor, class = input$class, 
                            organization = input$organization,
                            ngrams_type = input$ngramsType)
                     
          incProgress(1)
        }
      )
      
      return(p)
    }) %>% 
      debounce(1000)
    
    output$tfidf_bars <- renderPlot({
      
      plot_function()
    })
    
    output$tfidfExplanation <- renderText({
      
      HTML(paste0("*TF-IDF stands for
          <u><a target='_blank' rel='noopener noreferrer' href='https://en.wikipedia.org/wiki/Tf%E2%80%93idf'>
          Term Frequency–Inverse Document Frequency</a></u>.
          It is a standard way of calculating the frequency (i.e. importance)
          of a word or series of words 
          (i.e. <u><a target='_blank' rel='noopener noreferrer' href='https://en.wikipedia.org/wiki/N-gram'>n-grams</a></u>) 
          in the given text. It is a little more sophisticated than
          standard frequency as it adjusts for words that appear too frequently
          in the text. For example, stop words like ", "\"", "a", "\"", " and ",
          "\"", "the", "\"", " are very frequent but uniformative of
          the cotent of the text.", 
          "<p> NOTE: IN THIS VERSION, USING THE TRUST SELECTION BOX WILL NOT 
          RETURN TRUST-SPECIFIC PLOTS. THIS IS DUE TO THE LACK OF SUFFICIENT 
          DATA. THE PLOTS TEMPORARILY DISPLAY TF-IDFs FOR ALL TRUSTS TOGETHER."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("class"), 
        "Choose a label:",
        choices = sort(unique(unlist(x[[predictor]]))),
        selected = sort(unique(unlist(x[[predictor]])))[1]
      )
    })
    
    output$organizationControl <- renderUI({
      
      selectInput(
        session$ns("organization"), 
        "Choose an organization:",
        choices = sort(unique(x$organization)),
        selected = sort(unique(x$organization))[1]
      )
    })
    
    output$ngramsNumControl <- renderUI({
      
      selectInput(
        session$ns("ngramsType"),
        label = HTML("<b>Choose between unigrams or bigrams:</b>"),
        choices = c("Unigrams", "Bigrams"),
        selected = "Unigrams"
      )
    })
  })
}
