#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_analysis_nrc_sentiment_breakdown_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(12,
             box(width = NULL, background = "red",
                 htmlOutput(ns("netSentimentBox"))
             )
      )
    ),
    
    fluidRow(
      
      column(width = 4,
             uiOutput(ns("nrcSentimentControl")),
             uiOutput(ns("numberOfFacetsControl"))
      )
    ),
    
    h4("Click a plot to see further information"),
    
    fluidRow(
      column(width = 12,   
             uiOutput(ns("dynamicPlot"))
      )
    )
  )
}

#' sentiment_analysis Server Functions
#'
#' @noRd 
mod_sentiment_analysis_nrc_sentiment_breakdown_server <- function(id, x, 
                                                                  target, 
                                                                  text_col, 
                                                                  groups) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    nrc_sentiments <- experienceAnalysis::prep_sentiments_nrc()
    
    net_sentiment_wide_nrc <- reactive({
      experienceAnalysis::calc_net_sentiment_nrc(
        x, 
        target_col_name = target, 
        text_col_name = text_col,
        grouping_variables = groups,
        filter_class = input$class, 
        filter_main_group = get_golem_config("filter_organization"))
    })
    
    net_sentiment_long_nrc <- reactive({
      
      #req(input$nrcSentiments)
      req(input$numberOfFacets)
      
      if (isTruthy(req(input$nrcSentiments))) {
        
        experienceAnalysis::tidy_net_sentiment_nrc(
          net_sentiment_wide_nrc(),
          sorting_sentiments = input$nrcSentiments,
          num_of_lines = input$numberOfFacets
        )
      } else {
        req(input$nrcSentiments)
      }
    }) %>% 
      debounce(2000)
  
  output$nrcSentimentControl <- renderUI({
    
    selectInput(
      session$ns("nrcSentiments"),
      HTML("<b>Sort feedback comments in descending order by one or
           more sentiments</b>"),
      nrc_sentiments,
      multiple = TRUE,
      selected = nrc_sentiments[1]
    )
  })
  
  output$numberOfFacetsControl <- renderUI({
    
    sliderInput(
      session$ns("numberOfFacets"),
      label = HTML("<b>Select number of plots to display:</b>"),
      value = 60,
      min = 60,
      max = 300,
      step = 20, 
      ticks = FALSE
    )
  })
  
  output$netSentimentBox <- renderText({
    HTML(paste0("This tab uses <u><a target='_blank' rel='noopener noreferrer' href='https://en.wikipedia.org/wiki/Sentiment_analysis'>Sentiment Analysis</a></u> 
          to see which sentiments are expressed the most in a given patient 
          feedback comment. We use a pre-defined <i>sentiment lexicon</i> 
          known as <u><a target='_blank' rel='noopener noreferrer' href='https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm'>NRC</a></u>, 
          that empirically assigns one or more sentiments to a word. For example, 
          according to NRC, the word ", "\"", "happy", "\"", " expresses four 
          sentiments, namely anticipation, joy, positive & trust. 
          (The NRC lexicon has 10 sentiments, namely anger, anticipation, disgust, fear, joy, negative, positive, 
          sadness, surprise & trust.)
          The bar plots show, for each feedback text, the number of times a 
          certain sentiment is expressed in the text."))
  })
  
  output$dynamicPlot <- renderUI({
    
    if (isTruthy(req(input$nrcSentiments))) {
      number_of_plots <- length(unique(net_sentiment_long_nrc()$linenumber))
      plot_height <- ceiling(number_of_plots / 5) * 300
      
      plotOutput(
        session$ns("facetPlot"), 
        height = plot_height,
        click = ns("plot_click")
      ) %>%
        shinycssloaders::withSpinner(hide.ui = FALSE)
     }
  })
  
  output$facetPlot <- renderCachedPlot({
    
    # withProgress(
    #   message = 'Calculation in progress',
    #   detail = 'This may take a few seconds...', 
    #   value = 0,
    #   {
        p <- experienceAnalysis::plot_net_sentiment_long_nrc(
          net_sentiment_long_nrc()
        )
        
    #     incProgress(1)
    #   }
    # )
    
    return(p)
  },
  sizeGrowthRatio(width = 1024, height = 768, growthRate = 1.2),
  res = 108,
  pointsize = 2,
  cacheKeyExpr = 
    {
      list(
        net_sentiment_long_nrc(),
        input$class, 
        input$ngramsType
      ) 
    }
  
  )
  
  observeEvent(input$plot_click, {
    
    showModal(
      modalDialog(
        htmlOutput(session$ns("tooltipWindow")),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  output$tooltipWindow <- renderText({
    
    tooltip_info <- net_sentiment_long_nrc() %>%
      tidyr::pivot_wider(
        names_from = name, 
        values_from = value, 
        values_fill = 0
      ) %>%
      dplyr::filter(linenumber %in% input$plot_click$panelvar1) %>%
      dplyr::select(
        linenumber, 
        dplyr::all_of(c(nrc_sentiments, groups[1], target, text_col))
      ) %>%
      dplyr::slice(1) %>%
      dplyr::rename(
        "Comment number" = linenumber,
        "Organization" = get_golem_config("filter_organization"),
        "Feedback text tag" = {{target}},
        "Feedback text" = {{text_col}}
      )
    
    HTML(
      tooltip_info %>%
        mapply(
          FUN = function(x, y) {
            paste0("<b>", y, ": </b>", x, "<br>")
          }, 
          y = names(tooltip_info), 
          USE.NAMES = FALSE
        ) %>%
        paste(collapse = "")
    )
  })
  
  output$classControl <- renderUI({
    
    selectInput(
      session$ns("class"), 
      "Choose a label:",
      choices = sort(unique(x[[target]])),
      selected = sort(unique(x[[target]]))[1]
    )
  })
 })
}
