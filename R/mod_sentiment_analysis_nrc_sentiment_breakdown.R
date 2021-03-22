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
    
    nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
      dplyr::select(sentiment) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      sort()
    
    text_data_filtered <- reactive({
      aux <- text_data %>%
        #dplyr::filter(super != "Couldn't be improved") %>%
        dplyr::mutate(linenumber = dplyr::row_number()) %>% 
        dplyr::filter(
          label %in% input$label,
          organization %in% input$organization
        )
    })
    
    net_sentiment_nrc <- reactive({
      text_data_filtered() %>%
        tidytext::unnest_tokens(word, feedback) %>%
        dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
        dplyr::count(linenumber, sentiment, name = "sentiment_count") %>%
        dplyr::mutate(
          sentiment_count =
            dplyr::case_when(
              is.na(sentiment) ~ NA_integer_,
              TRUE ~ sentiment_count
            )
        ) %>%
        dplyr::select(linenumber, sentiment, sentiment_count) %>%
        tidyr::pivot_wider(names_from = sentiment,
                           values_from = sentiment_count,
                           values_fill = 0,
                           names_sort = TRUE
        ) %>%
        dplyr::left_join(text_data_filtered(), by = "linenumber") %>%
        dplyr::select(feedback, everything(), -`NA`) %>%
        # dplyr::mutate(all_sentiments =
        #                 dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
        #                 split(seq(nrow(.))) %>%
        #                 lapply(function(x) unlist(names(x)[x != 0]))
        # ) %>%
        #dplyr::select(feedback, all_sentiments, everything())
        dplyr::select(feedback, everything())
      })
    
    plot_data <- reactive({
      
      #req(input$nrcSentiments)
      req(input$numberOfFacets)
      
      if (isTruthy(req(input$nrcSentiments))) {
        net_sentiment_nrc() %>% 
        dplyr::arrange(
          dplyr::across(input$nrcSentiments, dplyr::desc)	
        ) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(nrc_sentiments)) %>%
        dplyr::filter(value != 0) %>%
        dplyr::filter(linenumber %in% 
                        unique(.$linenumber)[1:input$numberOfFacets]) %>%
        dplyr::mutate(
          name = factor(name, levels = sort(nrc_sentiments, decreasing = TRUE)),
          linenumber = factor(linenumber, levels = unique(.$linenumber))
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
    HTML(paste0("This tab uses <u><a href='https://en.wikipedia.org/wiki/Sentiment_analysis'>Sentiment Analysis</a></u> 
          to see which sentiments are expressed the most in a given patient 
          feedback comment. We use a pre-defined <i>sentiment lexicon</i> 
          known as <u><a href='https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm'>NRC</a></u>, 
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
      number_of_plots <- length(unique(plot_data()$linenumber))
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
        p <- plot_data() %>%
          ggplot2::ggplot(ggplot2::aes(value, name)) +
          ggplot2::geom_col(fill = "blue", alpha = 0.6) +
          ggplot2::facet_wrap(~ linenumber, ncol = 5) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::ylab('')
        
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
        plot_data(),
        input$label, 
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
    
    tooltip_info <- plot_data() %>%
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
      session$ns("label"), 
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
