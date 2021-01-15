#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_analysis_ui <- function(id){
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
        
        column(
          width = 5,
          selectInput(ns("pred"), "Choose a tag:",
                      choices = sort(unique(test_data$pred))),
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
mod_sentiment_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tidy_feedback <- data_for_tfidf %>%
      dplyr::group_by(super) %>%
      dplyr::mutate(linenumber = dplyr::row_number()) %>%
      tidytext::unnest_tokens(word, improve) %>%
      dplyr::group_by(super) %>%
      # dplyr::arrange(super, linenumber) %>%
      dplyr::ungroup()
    
    # Find net sentiment in each tag
    net_sentiment_afinn <- tidy_feedback %>%
      dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>% 
      dplyr::group_by(super) %>%
      dplyr::summarise(sentiment = sum(value)) %>% 
      dplyr::mutate(method = "AFINN")
    
    net_sentiment_bing_and_nrc <- 
      dplyr::bind_rows(
        tidy_feedback %>%
          dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>% 
          dplyr::filter(sentiment %in% c("positive", "negative")) %>%
          dplyr::mutate(method = "Bing et al."),
        
        tidy_feedback %>%
          dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>% 
          dplyr::filter(sentiment %in% c("positive", "negative")) %>%
          dplyr::mutate(method = "NRC")
      ) %>%
      dplyr::count(super, method, sentiment) %>%
      tidyr::spread(sentiment, n, fill = 0) %>%
      dplyr::mutate(sentiment = positive - negative) %>%
      dplyr::ungroup()
    
    output$netSentiment <- renderPlot({
      
      dplyr::bind_rows(net_sentiment_afinn, 
                       net_sentiment_bing_and_nrc) %>%
        ggplot2::ggplot(ggplot2::aes(sentiment, reorder(super, sentiment))) +
        ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
        ggplot2::facet_wrap(~method, ncol = 1, scales = "free") +
        ggplot2::labs(x = "Net sentiment", y = NULL,
                      title = "Net sentiment per tag") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90),
          axis.text.y = ggplot2::element_text(size = 12)
        )
    })
    
    output$mostCommonWords <- renderPlot({
      
      # Most common positive and negative words
      bing_word_counts <- tidy_feedback %>%
        dplyr::filter(super == input$pred) %>%
        dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
        dplyr::count(word, sentiment, sort = TRUE)
      
      bing_word_counts %>%
        dplyr::group_by(sentiment) %>%
        dplyr::top_n(10) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(n, reorder(word, n), fill = sentiment)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::facet_wrap(~sentiment, scales = "free_y") +
        #ggplot2::labs(x = "Contribution to sentiment") +
        ggplot2::ylab("") + 
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90),
          axis.text.y = ggplot2::element_text(size = 12)
        )
    })
    
    output$sentimentAnalysis <- renderPlot({
      data_for_tfidf %>%
        tidytext::unnest_tokens(word, improve) %>%
        #dplyr::distinct() %>%
        dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) %>%
        dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>% 
        dplyr::group_by(super) %>% 
        dplyr::mutate(value = sum(value) / length(unique(word))) %>%
        dplyr::ungroup() %>% 
        dplyr::select(-word) %>%
        dplyr::distinct() %>%
        ggplot2::ggplot(ggplot2::aes(value, reorder(super, value))) +
        ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
        ggplot2::labs(x = "Sentiment score", y = NULL,
                      title = "Sentiment per tag") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    output$sentimentAnalysisExplanation <- renderText({
      HTML(paste0("<u><a href='https://en.wikipedia.org/wiki/Sentiment_analysis'>
          Sentiment Analysis</a></u> is a method for extracting the sentiment
          in a text. It uses pre-defined <i>sentiment lexicons</i> (e.g. <u><a href=
          'http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html'>AFINN</a></u>, 
          <u><a href=
          'https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html'>Bing</a></u> 
          or <u><a href=
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
      HTML(paste0("The bar plots show the most common words with a positive or
                  negative sentiment (Bing dictionary) that appear in the 
                  feedback text."))
    })
  })
}
