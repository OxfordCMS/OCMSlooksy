# Module UI
  
#' @title   mod_setup_ui and mod_setup_server
#' @description  Prepare data for analysis
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_setup
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_setup_ui <- function(id){
  ns <- NS(id)
  tagList(
  
    fluidPage(
      fluidRow(
        box(width = 12,
            h3('Check Box'), verbatimTextOutput(ns('check')))),
      fluidRow(
        
        # samples to include in analysis
        box(radioButtons(ns('sample_select'), "Samples to include in analysis:",
                         choices = c('Use all samples' = 'all', 
                                     'Select samples to include' = 'include',
                                     'Select samples to exclude' = 'exclude'), 
                         selected = 'all'),
            conditionalPanel(condition = "input.sample_select != 'all'",
                             DT::dataTableOutput(ns('sample_options'))),
            actionButton(ns('sample_filter'), 'Filter samples'),
        # set taxonomic level
        selectInput(ns('tax_level'), 'Taxonomic level:',
                    choices = c('ASV','Phylum','Class','Order',
                                'Family','Genus','Species'),
                    selected = 'ASV'),
        br(),
        br(),
        
        # next tab
        actionButton("next_tab", "Next Step")
        )
      )
    )
  )
}
    
# Module Server-----------------------------------------------------------------
    
#' @rdname mod_setup
#' @export
#' @keywords internal
    
mod_setup_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  # import data into module
  data_set <- reactive({improxy$data_db})
  
  curr_data <- reactive(data_set()$example_metadata)
  
  # Decide if sample filtering desired
  observeEvent(input$sample_select, {
    if(input$sample_select != 'all') {
      # use metadata to select samples to be included in analysis
      output$sample_options <- DT::renderDataTable(curr_data())
    }
    })
 
  # index of sample to include
  index_include <- eventReactive(input$sample_filter, {
    req(input$sample_filter)
    
    if(input$sample_select == 'all') {
      1:nrow(curr_data())
    }
    else {
      input$sample_options_rows_selected
      }
  })
  
  # include specified samples
  sample_include <- eventReactive(input$sample_filter, {
    req(input$sample_filter)
    all_sample <- curr_data()$sampleID
    if(input$sample_select == 'exclude') {
      all_sample[-index_include()]
    }
    else { 
      all_sample[index_include()]
}
  })
 
  # filter samples in data set
  working_meta <- reactive({
    req(input$sample_filter)
    curr_data() %>%
      dplyr::filter(sampleID %in% sample_include())
  })
  
  # aggregate asv data based on tax_level *********add tax*****
  working_asv <- reactive({
    req(tax_level)
    data_set()$example_asv %>%
      gather('sampleID','read_count', -ASV) %>%
      group_by(sampleID, .data[[input$tax_level]]) %>%
      summarize(agg_count = sum(read_count))
  })
  
  working_data <- reactive({
    working_asv() %>%
    inner_join(working_meta(), 'sampleID')
  })
  
  output$check <- renderPrint({
    working_asv
  })
  
  # jump to next tab
  observeEvent(input$next_tab, {
    updateTabsetPanel(session, "tabs",
                      selected = "overview")
  })
  # return dataset
  cross_module = reactiveValues()
  observe({cross_module$data_db <- curr_data})
  return(cross_module)
}
    
## To be copied in the UI
# mod_setup_ui("setup_ui_1")
    
## To be copied in the server
# callModule(mod_setup_server, "setup_ui_1")
 
