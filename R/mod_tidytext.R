#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tidytext_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(12,
             box(width = NULL, background = "red",
                 htmlOutput(ns("netSentimentBox"))
             )
      )
    ),
    
    h4("Click a plot to see further information"),
    
    fluidRow(
      column(width = 12,
             uiOutput(ns("nrcSentimentControl"))
      )
    ),
    
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
mod_tidytext_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
      dplyr::select(sentiment) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      sort()
    
    net_sentiment_nrc <- data_for_tfidf %>%
      dplyr::filter(super != "Couldn't be improved") %>%
      dplyr::mutate(linenumber = dplyr::row_number()) %>%
      tidytext::unnest_tokens(word, improve) %>%
      dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
      dplyr::count(linenumber, sentiment, name = 'sentiment_count') %>%
      dplyr::mutate(sentiment_count = dplyr::case_when(
        is.na(sentiment) ~ NA_integer_,
        TRUE ~ sentiment_count
      )) %>%
      dplyr::select(linenumber, sentiment, sentiment_count) %>%
      tidyr::pivot_wider(names_from = sentiment, 
                         values_from = sentiment_count, 
                         values_fill = 0,
                         names_sort = TRUE
      ) %>%
      dplyr::left_join(
        data_for_tfidf %>%
          dplyr::filter(super != "Couldn't be improved") %>%
          dplyr::mutate(linenumber = dplyr::row_number()),
        by = "linenumber"
      ) %>%
      dplyr::select(improve, everything(), -`NA`) %>%
      dplyr::mutate(all_sentiments =  
                      dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
                      split(seq(nrow(.))) %>%
                      lapply(function(x) unlist(names(x)[x != 0]))
      ) %>%
      dplyr::select(improve, all_sentiments, everything()) %>%
      dplyr::slice(1:60)
    
    sorted_data <- reactive({
      net_sentiment_nrc %>% 
        dplyr::arrange(
          dplyr::across(input$nrcSentiments, dplyr::desc)	
        ) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(nrc_sentiments)) %>%
      dplyr::filter(value != 0) %>%
      dplyr::mutate(
        name = factor(name, levels = sort(nrc_sentiments, decreasing = TRUE)),
        linenumber = factor(linenumber, levels = unique(.$linenumber))
      )
    })
  
  output$nrcSentimentControl <- renderUI({
    
    selectInput(
      session$ns("nrcSentiments"),
      HTML("<b>Sort feedback comments in descending order by:</b>"),
      nrc_sentiments,
      multiple = TRUE,
      selected = nrc_sentiments[1]
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
    
    # calculate height of plot
    
    #number_of_plots <- length(unique(plotFacets()))
    
    #plot_height <- ceiling(number_of_plots / 5) * 300
    
    plotOutput(
      session$ns("facetPlot"), 
      height = 12 * 300,
      click = ns("plot_click")
    )
  })
  
  output$facetPlot <- renderPlot({
    
    sorted_data() %>%
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
  })
  
  observeEvent(input$plot_click, {
    
    showModal(
      modalDialog(
        htmlOutput(ns("tooltipWindow")),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  output$tooltipWindow <- renderText({
    
    tooltip_info <- sorted_data() %>%
      tidyr::pivot_wider(
        names_from = name, 
        values_from = value, 
        values_fill = 0
      ) %>%
      dplyr::filter(linenumber %in% input$plot_click$panelvar1) %>%
      dplyr::select(linenumber, super, improve, all_of(nrc_sentiments)) %>%
      dplyr::slice(1) %>%
      dplyr::rename(
        "Comment number" = linenumber,
        "Feedback text tag" = super,
        "Feedback text" = improve
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
  
 })
}