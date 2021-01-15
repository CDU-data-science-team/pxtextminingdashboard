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
             #uiOutput(ns("hover_info")),
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
          dplyr::across(input$nrcSentiments, dplyr::desc)	
        )
    })
    
    plot_data <- reactive({
      sorted_data() %>%
      dplyr::filter(super != "Couldn't be improved") %>%
      ##dplyr::select(-super, -linenumber) %>%
      dplyr::slice(1:60) %>%
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
    
    div(
      width = 7,
      style = "position:relative",
      plotOutput(
        session$ns("facetPlot"), 
        height = 12 * 300,
        hover = 
          hoverOpts(
            session$ns("plot_hover"), 
            delay = 100, 
            delayType = "debounce"
          )
      ),
      uiOutput(session$ns("hover_info"))
    )
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(plot_data(), hover, threshold = 25, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / 
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / 
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + 
      left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + 
      top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Car: </b>", 'a', "<br/>",
                    #"<b> mpg: </b>", point$value, "<br/>",
                    "<b> hp: </b>", 'B', "<br/>")))
    )
  })
  
  output$facetPlot <- renderPlot({
    
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
    
    p
  })
})
}