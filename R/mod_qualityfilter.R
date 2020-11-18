#' qualityfilter UI Function
#'
#' @description Filter out poor quality samples
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qualityfilter_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      
      #sidebar------------------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'menu',
          br(),
          tags$div(
            style = 'text-align: center',
            tags$b('Input controls')),
          
          fixedPanel(
            radioButtons(ns('sample_select_prompt'), 
                         "Samples to include in analysis:",
                         choices = c('Use all samples' = 'all', 
                                     'Include select samples' = 'include',
                                     'Exclude select samples' = 'exclude'), 
                         selected = 'all'),
            withBusyIndicatorUI(
              actionButton(ns('submit_sample'), "Filter samples")  
            )
          )
        ) # end sidebar menu
      ), # end dashboard sidebar
      dashboardBody(
        box(width = '100%', height = 'auto', br(),br(), br(),
            fluidRow(
              box(width = 12, h3('Check'),
                  verbatimTextOutput(ns('check')))),
            column(
              width = 12, 
              h1('Remove Poor Quality Samples'),
              tags$div("Before starting data exploration, it may be desirable to remove poor-quality samples. This ensures that the analysis is performed on a dataset that is relevant to the current research question.")),
            column(
              width = 12,
              h1('Filter Samples'), br(),
              tags$div("It may be desirable to perform analysis on a subset of samples if certain samples did not pass QC, or are no longer relevant to the current research question.")),
            fluidRow(
              br(), br(),
              column(
                width = 8,
                hidden(div(
                  id = ns('sample_filter_div'),
                  column(
                    width = 12,
                    p("Select samples to include/exclude from the table and click 'Filter samples' to apply changes"), br(),
                    DT::dataTableOutput(ns('sample_options_ui')) 
                  )
                )),
                column(
                  width = 12,
                  h3(textOutput(ns('preview_sample_title'))),
                  DT::dataTableOutput(ns('preview_sample')) %>% 
                    shinycssloaders::withSpinner()
                )
              ),
              hidden(div(
                id = ns('sample_filter_selcted_div'),
                column(
                  width = 4,
                  br(),
                  wellPanel(tags$b('Selected samples:'),
                            htmlOutput(ns('sample_select'))
                  )
                )
              ))
            )
        ) # end box
      ) # end dashboardBody
    ) # end dashboardPage
  ) # end taglist
}
    
#' qualityfilter Server Function
#'
#' @noRd 
mod_qualityfilter_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  # # check
  # output$check <- renderPrint({
  # 
  # })
  
  # import data into module
  met <- reactive(improxy$data_db$metadata)
  asv <- reactive(improxy$data_db$merged_abundance_id)
  tax <- reactive(improxy$data_db$merged_taxonomy)
  
  # subset samples--------------------------------------------------------------
  # show sample table
  observeEvent(input$sample_select_prompt, {
    toggle("sample_filter_div", condition = input$sample_select_prompt != 'all')
  })
  observeEvent(input$sample_select_prompt, {
    toggle("sample_filter_selcted_div", 
           condition = input$sample_select_prompt != 'all')
  })
  
  
  output$sample_options_ui <- DT::renderDataTable({
    out <- met()
    DT::datatable(out, filter = 'top', options = list(scrollX = TRUE))
  })
  
  all_sample <- reactive(met()$sampleID)
  
  # Decide if sample filtering desired
  rows_selected <- reactive({
    
    # select all samples
    if(input$sample_select_prompt == 'all') {
      1:length(all_sample())
    }
    else {
      # use metadata to select samples to be included in analysis
      input$sample_options_ui_rows_selected
    }
  })
  
  # map row index to sample names
  sample_include <- reactive({
    if(input$sample_select_prompt == 'exclude') {
      all_sample()[-rows_selected()]
    }
    else { 
      all_sample()[rows_selected()]
    }  
  })
  
  output$sample_select <- renderUI({
    HTML(paste(all_sample()[rows_selected()], collapse = '<br/>'))
  })
  
  # filter samples in data set
  met_filtered <- eventReactive(input$submit_sample, {
    withBusyIndicatorServer('submit_sample', "setup_ui_1", {
      Sys.sleep(1)
      met() %>%
        filter(sampleID %in% sample_include())
    })
    
  })
  
  
  output$preview_sample_title <- renderText({
    req(input$submit_sample)
    'Samples included in analysis:'
  })
  
  output$preview_sample <- DT::renderDataTable({
    req(input$submit_sample)
    DT::datatable(met_filtered(), extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 pageLength = 30,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  
  # update ASV with filtered samples
  samp_filtered <- eventReactive(input$submit_sample, {
    improxy$asv_gather %>%
      filter(sampleID %in% unique(met_filtered()$sampleID))
  })
  
  # store prepared data to pass on to next next module--------------------------
  working_set <- reactive({
    req(input$sample_select_prompt)
    # keep in wide format to be consistent with db format
    list(met = met_filtered(),
         asv = samp_filtered() %>% spread(sampleID, read_count),
         tax = tax(),
         work = samp_filtered() %>%
           inner_join(tax() %>% mutate_all(as.character), 'featureID') %>%
           inner_join(met_filtered(), 'sampleID')
    )
  })
  
  # return dataset
  cross_module <- reactiveValues()
  observe({
    req(input$submit_sample)
    cross_module$work_db <- working_set()
  })
  return(cross_module)
 
}
    
## To be copied in the UI
# mod_qualityfilter_ui("qualityfilter_ui_1")
    
## To be copied in the server
# callModule(mod_qualityfilter_server, "qualityfilter_ui_1")
 
