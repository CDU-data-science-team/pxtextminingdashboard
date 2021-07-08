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
        uiOutput(ns("ngramsNumControl"))
      ),
      
      column(
        width = 4,
        uiOutput(ns("barsNumberControl"))
      )
    ),
    
    fluidRow(
      width = 12,
      
      column(
        width = 12,
        
        box(
          width = NULL,
          
          downloadButton(ns("downloadTfidfNgrams"), "Download plot"),
          
          plotOutput(ns("tfidfBars"), height = "1000px") %>%
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
mod_tfidf_server <- function(id, x, target, text_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reactive function arguments to pass to plotTfidfNgrams(). This is 
    # necessary for the download button, as the plot to download is for the 
    # chosen ngram type and class.
    ngramsType <- reactive({input$ngramsType})
    filterClass <- reactive({input$class})
    
    dataTfidf <- reactive({
      
      req(filterClass())
      
      x %>% 
        experienceAnalysis::calc_tfidf_ngrams(
          target_col_name = target, 
          text_col_name = text_col,
          filter_class = filterClass(), 
          ngrams_type = ngramsType(),
          number_of_ngrams = input$barsNum
        )
    })
    
    # We want to debounce the EXPRESSION dataTfidf (note no "()") before
    # passing the reactive element dataTfidf() that has the data. We therefore 
    # must use object dataTfidf_d instead of the original dataTfidf in what 
    # follows from now on.
    dataTfidf_d <- dataTfidf %>% # No "()" to indicate we are debouncing the EXPRESSION NOT the reactive object.
      debounce(1000)
      
    output$tfidfBars <- renderPlot({
      
      req(filterClass())
      
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a few seconds...',
        value = 0,
        {
          dataTfidf_d() %>% 
            plotTfidfNgrams(
              ngrams_type = ngramsType(), 
              filter_class = filterClass()
            )
        }
      )
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
    
    output$downloadTfidfNgrams <- downloadHandler(
      filename = function() {
        
        filterClass_clean <- clean_text(filterClass())
        
        paste0("tfidf_bars_", target, "_", filterClass_clean, "_", 
               tolower(ngramsType()), ".pdf")
      },
      content = function(file) {
        ggplot2::ggsave(file, 
                        plot = plotTfidfNgrams(x = dataTfidf_d(), 
                                               ngrams_type = ngramsType(), 
                                               filter_class = filterClass()), 
                        device = pdf, height = 16, units = "in")
      }
    )
  })
}
