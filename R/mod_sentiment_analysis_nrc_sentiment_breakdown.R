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
      
      column(width = 6,
             uiOutput(ns("organizationControl")),
             uiOutput(ns("classControl"))
      ),
      
      column(width = 6,
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
mod_sentiment_analysis_nrc_sentiment_breakdown_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    nrc_sentiments <- experienceAnalysis::get_sentiments_nrc()
    
    net_sentiment_wide_nrc <- reactive({
      experienceAnalysis::get_net_sentiment_wide_nrc(
        text_data, 
        class_col_name = "label", 
        org_col_name = "organization",
        filter_class = input$class, 
        filter_organization = input$organization)
    })
    
    net_sentiment_long_nrc <- reactive({
      
      #req(input$nrcSentiments)
      req(input$numberOfFacets)
      
      if (isTruthy(req(input$nrcSentiments))) {
        
        experienceAnalysis::get_net_sentiment_long_nrc(
          net_sentiment_wide_nrc(),
          sorting_sentiments = input$nrcSentiments,
          num_of_facets = input$numberOfFacets
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
      dplyr::select(linenumber, organization, label, feedback, 
                    dplyr::all_of(nrc_sentiments)) %>%
      dplyr::slice(1) %>%
      dplyr::rename(
        "Comment number" = linenumber,
        "Organization" = organization,
        "Feedback text tag" = label,
        "Feedback text" = feedback
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
