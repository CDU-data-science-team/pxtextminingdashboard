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
        width = 3,
        uiOutput(ns("classControl"))
      ),
      
      column(
        width = 3,
        uiOutput(ns("organizationControl"))
      ),
      
      column(
        width = 3,
        uiOutput(ns("ngramsNumControl"))
      ),
      
      column(
        width = 3,
        uiOutput(ns("barsNumberControl"))
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
mod_tfidf_server <- function(id, x, target, text_col, groups) {
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
          p <- x %>% 
            experienceAnalysis::calc_tfidf_ngrams(
              target_col_name = target, 
              text_col_name = text_col,
              grouping_variables = groups,
              filter_class = input$class, 
              filter_main_group = input$organization,
              ngrams_type = input$ngramsType,
              number_of_ngrams = input$barsNum
            ) %>% 
            experienceAnalysis::plot_tfidf_ngrams(
              ngrams_type = input$ngramsType,
              filter_class = input$class
            )
            
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
          the cotent of the text."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("class"), 
        "Choose a label:",
        choices = sort(unique(unlist(x[[target]]))),
        selected = sort(unique(unlist(x[[target]])))[1]
      )
    })
    
    output$organizationControl <- renderUI({
      
      selectInput(
        session$ns("organization"), 
        "Choose an organization:",
        choices = sort(unique(x[[groups[1]]])), # The first group is always the "main" one (see {experienceAnalysis}), i.e. the Trust/Organization in the Patient Experience case.
        selected = sort(unique(x[[groups[1]]]))[1]
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
    
    output$barsNumberControl <- renderUI({
      
      sliderInput(
        session$ns("barsNum"),
        label = HTML("<b>Number of bars:</b>"),
        value = 15,
        min = 1,
        max = 100
      )
    })
  })
}
