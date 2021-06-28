#' bigrams_network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bigrams_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 4,
        uiOutput(ns("classControl"))
      ),
      
      column(
        width = 4,
        uiOutput(ns("organizationControl"))
      ),
      
      column(
        width = 4,
        uiOutput(ns("bigramsPropControl"))
      )
    ),
    
    fluidRow(
      width = 12,
      column(
        width = 12,
        box(
          width = NULL,
          plotOutput(ns("bigramsNetwork")) %>%
            shinycssloaders::withSpinner(hide.ui = FALSE),
          box(
            htmlOutput(ns("bigramsNetworkExplanation")), 
            background = 'red', 
            width = NULL
          )
        )
      )
    )
  )
}
    
#' bigrams_network Server Functions
#'
#' @noRd 
mod_bigrams_network_server <- function(id, x, target, text_col, groups) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$bigramsNetwork <- renderPlot({
      
      req(input$class)
      req(input$bigramsProp)
      
      x %>% 
        experienceAnalysis::calc_bigrams_network(
          target_col_name = target, 
          text_col_name = text_col,
          grouping_variables = groups,
          filter_class = input$class,
          filter_main_group = input$organization, 
          bigrams_prop = input$bigramsProp
        ) %>% 
        experienceAnalysis::plot_bigrams_network()
    })
    
    output$bigramsNetworkExplanation <- renderText({
      HTML(paste0("This plot shows the top-<i>X</i>% (<i>X</i> is 
                  user-specified) most frequent pairs of consecutive words in 
                  the feedback text. The arrows point towards the second word 
                  in each pair. For example, if ", 
                  "\"", "snack time", "\"",
                  "is a frequent pair of consecutive words, then it will show 
                  on the graph as ", 
                  "\"", "snack -> time", "\"", 
                  ". <br><b>NOTE: </b>A <i>X</i> value of 1%-20% will normally 
                  reveal sufficient information. Higher values will produce 
                  an over-populated plot."))
    })
    
    output$classControl <- renderUI({
      
      if (target == "label") {
        
        aux <- x %>%
          dplyr::right_join(row_index_label, by = 'row_index')
      } else {
        
        aux <- x %>%
          dplyr::right_join(row_index_criticality, by = 'row_index')
      }
      
      selectInput(
        session$ns("class"), 
        "Choose a label:",
        choices = sort(unique(unlist(aux[[target]]))),
        selected = sort(unique(unlist(aux[[target]])))[1]
      )
    })
    
    output$organizationControl <- renderUI({
      
      selectInput(
        session$ns("organization"), 
        "Choose an organization:",
        choices = sort(unique(x[[groups[1]]])), # The first group is always the "main" one (see {experienceAnalysis}), i.e. the Trust/Organization in the Patient Experience case.
        selected = sort(unique(x[[groups[1]]]))[1]
      )
    })
    
    output$bigramsPropControl <- renderUI({
      
      sliderInput(
        session$ns("bigramsProp"),
        label = HTML("<b>Proportion (%) of most frequent bigrams:</b>"),
        value = 50,
        min = 1,
        max = 100
      )
    })
  })
}
