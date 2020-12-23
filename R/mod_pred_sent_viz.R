#' sentiment_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pred_sent_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Boxes need to be put in a row (or column)
    fluidRow(

      column(width = 12,   
             box(width = NULL,
                 reactable::reactableOutput(ns("nigel_and_jonathan")))
      )
    )
  )
}

#' sentiment_analysis Server Functions
#'
#' @noRd 
mod_pred_sent_viz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # https://kcuilla.netlify.app/post/2019-nfl-team-ratings/
    bar_chart <- function(label,
               width = "100%",
               height = "13px",
               fill = "#00bfc4",
               background = NULL) {
        bar <-
          div(style = list(
            background = fill,
            width = width,
            height = height
          ))
        
        chart <-
          div(style = list(
            flexGrow = 1,
            marginLeft = "8px",
            #background = background
            background = "#dcdcdc"
          ),
          bar)
        
        div(style = list(display = "flex", alignItems = "center"), 
            label, chart)
    }
    
    nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
      dplyr::select(sentiment) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      sort()
    
    net_sentiment_nrc <- data_for_tfidf %>%
      dplyr::mutate(linenumber = dplyr::row_number()) %>%
      tidytext::unnest_tokens(word, improve) %>%
      dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
      dplyr::count(linenumber, sentiment) %>%
      dplyr::mutate(n = dplyr::case_when(
        is.na(sentiment) ~ NA_integer_,
        TRUE ~ n
      )) %>%
      dplyr::group_by(linenumber) %>%
      dplyr::mutate(proportion_of_sentiment = 
                      round(n / sum(n, na.rm = TRUE) * 100)) %>%
      dplyr::ungroup() %>%
      dplyr::select(linenumber, sentiment, proportion_of_sentiment) %>%
      tidyr::pivot_wider(names_from = sentiment, 
                         values_from = proportion_of_sentiment, 
                         values_fill = 0
      ) %>%
      dplyr::left_join(
        data_for_tfidf %>%
          dplyr::mutate(linenumber = dplyr::row_number()),
        by = "linenumber"
      ) %>%
      dplyr::select(improve, everything(), -`NA`) %>%
      dplyr::mutate(altogether =  
                        dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
                        split(seq(nrow(.))) %>%
                        lapply(unlist, use.names = FALSE)
      ) %>%
      dplyr::select(improve, altogether, everything())
    
    sticky_style <- list(position = "sticky", left = 0, 
                         background = "#fff", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    output$nigel_and_jonathan <- reactable::renderReactable({
      
      reactable::reactable(
        net_sentiment_nrc %>%
          dplyr::filter(super != "Couldn't be improved") %>%
          dplyr::select(-super, -linenumber) %>%
          dplyr::slice(1:10),
        columns = list(
          altogether = reactable::colDef(
            cell = 
              function(values) {
                sparkline::sparkline(values, type = "bar", 
                  chartRangeMin = 0, chartRangeMax = 50, 
                  #tooltipFormat =  '{{x}}: ${{y}}', # https://stackoverflow.com/questions/40859098/customise-sparkline-tooltip-in-shiny
                  colorMap = ggthemes::colorblind_pal()(8))
          }),
          
          improve = reactable::colDef(
            name = "Feedback",
            style = sticky_style,
            headerStyle = sticky_style,
            minWidth = 300
          ),
          
          anger = reactable::colDef(
            name = "Anger",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          anticipation = reactable::colDef(
            name = "Anticipation",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          disgust = reactable::colDef(
            name = "Disgust",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          fear = reactable::colDef(
            name = "Fear",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          joy = reactable::colDef(
            name = "Joy",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          negative = reactable::colDef(
            name = "Negative",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          positive = reactable::colDef(
            name = "Positive",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          sadness = reactable::colDef(
            name = "Sadness",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          surprise = reactable::colDef(
            name = "Surprise",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          ),
          
          trust = reactable::colDef(
            name = "Trust",
            align = "left",
            class = "border-left cell number",
            headerStyle = list(fontWeight = "500"),
            cell = function(value) {
              width <- paste0(value, "%")
              bar_chart(value,
                        width = width,
                        fill = "#66bbff",
                        background = "#dcdcdc")
            }
          )
        ),
        
        #wrap = FALSE,
        #filterable = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 100,
        pageSizeOptions = 100
      )
    })
  })
}