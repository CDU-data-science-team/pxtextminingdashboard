#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_analysis_tag_level_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(width = 12,
             
      box(
        width = 12,
        
        box(
          htmlOutput(ns("sentimentAnalysisExplanation")), 
          background = 'red', width = NULL
        ),
        
        uiOutput(ns("organizationControl")),
        
        column(
          width = 5,
          uiOutput(ns("classControl")),
          
          plotOutput(ns("mostCommonWords"), height = "400px"),
          
          box(
            htmlOutput(ns("mostCommonWordsText")),
            background = 'red', width = NULL
          )
        ),
        
        column(
          width = 7,
          
          plotOutput(ns("netSentiment"), height = "800px"),
          
          box(
            htmlOutput(ns("sentimentPerTagAndDictionaryText")),
            background = 'red', width = NULL
          )
        )
      )
    )
  )
}
    
#' sentiment_analysis Server Functions
#'
#' @noRd 
mod_sentiment_analysis_tag_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$netSentiment <- renderPlot({
      
      req(input$organization)
      
      net_sentiment_all_dicts <- reactive({
        
        text_data %>% 
          experienceAnalysis::get_net_sentiment_per_tag(
            target_col_name = 'label', 
            filter_organization = input$organization
          )
      })
      
      
      net_sentiment_all_dicts() %>%
        experienceAnalysis::plot_net_sentiment_per_tag(target_col_name = 'label')
    })
    
    output$mostCommonWords <- renderPlot({
      
      req(input$class)
      req(input$organization)
      
      bing_word_counts <- reactive({
        text_data %>%
          experienceAnalysis::get_bing_word_counts(
            target_col_name = 'label',
            filter_organization = input$organization,
            filter_class = input$class
          )
      })
      
      bing_word_counts() %>%
        experienceAnalysis::plot_bing_word_counts()
    })
    
    output$sentimentAnalysisExplanation <- renderText({
      HTML(paste0("<u><a target='_blank' rel='noopener noreferrer' href='https://en.wikipedia.org/wiki/Sentiment_analysis'>
          Sentiment Analysis</a></u> is a method for extracting the sentiment
          in a text. It uses pre-defined <i>sentiment lexicons</i> (e.g. <u><a target='_blank' rel='noopener noreferrer' href=
          'http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html'>AFINN</a></u>, 
          <u><a target='_blank' rel='noopener noreferrer' href=
          'https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html'>Bing</a></u> 
          or <u><a target='_blank' rel='noopener noreferrer' href=
          'https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm'>NRC</a></u>).
          These lexicons either empirically score the positivity or negativity of a word (e.g. 
          AFINN scores ", "\"", "happy", "\"", " as 3 and ", "\"", "sad", "\"", 
                  " as -2 on a scale of -5 to +5) or empirically associate a word
          with a certain sentiment (e.g. Bing associates ",
          "\"", "happy", "\"", " with a positive sentiment, while NRC associates it
          with sentiments anticipation, joy, positive & trust)."))
    })
    
    output$sentimentPerTagAndDictionaryText <- renderText({
      HTML(paste0("The bar plots show the ", "\"", "net sentiment", "\"", 
                  " for each feedback text tag as calculated by each of the 
                  three sentiment dictionaries. For AFINN, net sentiment is
                  the sum of sentiment values of the words in the feedback text.
                  For Bing and NRC, the net sentiment is the number of words with
                  positive sentiment minus the number of words with negative sentiment
                  in the feedback text."))
    })
    
    output$mostCommonWordsText <- renderText({
      HTML(paste0("The bar plots show the most common words with a 
                  positive or negative sentiment (Bing dictionary) that appear 
                  in the feedback text."))
    })
    
    output$classControl <- renderUI({
      
      selectInput(
        session$ns("class"), 
        "Choose a label:",
        choices = sort(unique(text_data$label)),
        selected = sort(unique(text_data$label))[1]
      )
    })
    
    output$organizationControl <- renderUI({
      
      selectInput(
        session$ns("organization"), 
        "Choose an organization:",
        choices = sort(unique(text_data$organization)),
        selected = sort(unique(text_data$organization))[1]
      )
    })
  })
}
