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
    # Boxes need to be put in a row (or column)
    
    fluidRow(
      column(12,
             box(width = NULL, background = "red",
                 htmlOutput(ns("netSentimentBox"))
             )
      )
    ),
    
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
      dplyr::mutate(linenumber = dplyr::row_number()) %>%
      tidytext::unnest_tokens(word, improve) %>%
      dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
      dplyr::count(linenumber, sentiment, name = 'sentiment_count') %>%
      dplyr::mutate(sentiment_count = dplyr::case_when(
        is.na(sentiment) ~ NA_integer_,
        TRUE ~ sentiment_count
      )) %>%
      #dplyr::group_by(linenumber) %>%
      #dplyr::mutate(sentiment = factor(sentiment, levels = nrc_sentiments)) %>%
      #dplyr::ungroup() %>%
      dplyr::select(linenumber, sentiment, sentiment_count) %>%
      tidyr::pivot_wider(names_from = sentiment, 
                         values_from = sentiment_count, 
                         values_fill = 0,
                         names_sort = TRUE
      ) %>%
      dplyr::left_join(
        data_for_tfidf %>%
          dplyr::mutate(linenumber = dplyr::row_number()),
        by = "linenumber"
      ) %>%
      dplyr::select(improve, everything(), -`NA`) %>%
      dplyr::mutate(all_sentiments =  
                      dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
                      split(seq(nrow(.))) %>%
                      lapply(function(x) unlist(names(x)[x != 0]))
      ) %>%
      dplyr::select(improve, all_sentiments, everything())

    sorted_data <- reactive({ 
      
    net_sentiment_nrc %>% 
      dplyr::arrange(
        dplyr::across(input$variables, dplyr::desc)	
      )
  })
  
  output$nrcSentimentControl <- renderUI({
    
    nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
      dplyr::select(sentiment) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      sort()
    
    selectInput(
      session$ns("variables"),
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
    
    #plotOutput(session$ns("facetPlot"), height = 12 * 300)
    plotly::plotlyOutput(session$ns("facetPlot"), height = 12 * 300)
  })
  
  
  #output$facetPlot <- renderPlot({
  output$facetPlot <- plotly::renderPlotly({
    
    break_text_into_several_lines <- function(x) {
      
      x %>%
        quanteda::tokens() %>%
        quanteda::tokens_chunk(size = 6) %>%
        lapply(
          function(x) {
            paste0(paste(x, collapse = " "), "<br>")
          }
        ) %>%
        unlist() %>%
        paste(collapse = " ")
    }
    
    tooltip_text <- function(name, value, feedback_text,
                             line_breaking_function = 
                               break_text_into_several_lines) {
      
      if (is.null(line_breaking_function)) {
        fdbck_text <- feedback_text
      } else {
        fdbck_text <- line_breaking_function(feedback_text)
      }
      
      paste0(
        "<b>Sentiment:</b> ", name, "<br>",
        "<b>Count</b>: ",  value, "<br>",
        "<b>Feedback</b>: ", fdbck_text
      )
    }
    
    p <- sorted_data() %>%
      dplyr::filter(super != "Couldn't be improved") %>%
      ##dplyr::select(-super, -linenumber) %>%
      dplyr::slice(1:60) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(nrc_sentiments)) %>%
      dplyr::filter(value != 0) %>%
      dplyr::mutate(
        name = factor(name, levels = sort(nrc_sentiments, decreasing = TRUE)),
        linenumber = factor(linenumber, levels = unique(.$linenumber))
      ) %>%
      ggplot2::ggplot(ggplot2::aes(value, name, 
                                   text = tooltip_text(name, value, feedback_text = improve))) +
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
    
    #p
    plotly::ggplotly(p, height = 2000, tooltip = "text") %>%
      plotly::layout(
        hoverlabel = list(
          align = "left",
          namelength = -1
        )
      )
  })
})
}