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
#' @import shinyjs
mod_setup_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      
      #sidebar------------------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'menu',
          menuItem('Filter Samples', tabName = 'filter_sample', selected = FALSE),
          menuItem('Filter ASVs', tabName = 'filter_asv'),
          
          conditionalPanel(
            condition = "input.menu === 'filter_sample'",
            br(), hr(),
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
              
              br(), br(), br(),
              actionButton(ns('submit_sample'), "Filter samples")
            )),
          
          conditionalPanel(
            condition = "input.menu === 'filter_asv'",
            br(), hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')),
            
            fixedPanel(
              radioButtons(ns('asv_select_prompt'), 
                           "ASVs to include in analysis:",
                           choices = c('Use all ASVs' = 'all', 
                                       'Filter ASVs' = 'some'),
                           selected = 'all'),
              
              hidden(
                div(id = ns('asv_filter_options_ui'),
                    radioButtons(
                      ns('asv_filter_options'), 'Filter ASVs by:',
                      choices = c('read count' = 'asv_by_count',
                                  'selection' = 'asv_by_select'),
                      selected = character(0))
                    )),
              br(), br(), br(),
              actionButton(ns('submit_asv'), "Filter ASVs")))
              
          )),
      
      # main panel--------------------------------------------------------------
      dashboardBody(
        box(width = '100%',
            h1('Prepare Data for Analysis'),
            fluidRow(width = 12,
                    h3('Check Box'), 
                    verbatimTextOutput(ns('check'))),
            
            tabItems(
              # filter samples--------------------------------------------------
              tabItem(
                tabName = 'filter_sample', 
                column(width = 12,
                       tags$div(
                         "It may be desirable to perform analysis on a subset of samples if certain samples did not pass QC, or are no longer relevant to the current research question.")),
                br(),
                column(width = 8,
                       DT::dataTableOutput(ns('sample_options_ui'))),
                column(width = 4,
                       tags$b('Selected samples:'),
                       htmlOutput(ns('sample_select'))),
                column(width = 12,
                       h3(textOutput(ns('preview_sample_title'))),
                       DT::dataTableOutput(ns('preview_sample')))),
              
              # filter asv------------------------------------------------------
              tabItem(
                tabName = 'filter_asv',
                column(width = 12,
                        tags$div(
                          "It may be desirable to perform analysis on a subset of ASVs if certain taxa are considered contamination, or are no longer relevant to the current research question.",
                          br(),br(),
                          tags$em(
                            tags$b("NB:"), "Filtering sequences based on sequence quality and minimum count threshold has already been performed during quality control processing of the dataset. Filtering ASVs at this stage should only be done if you have additional reasoning for omitting certain sequences or ASVs"),
                          br())),
                
                div(id = ns('asv_filter_div'),
                column(width = 12,
                       
                       hidden(
                         div(id = ns('asv_option_count'),
                             fluidRow(h3('Filter ASVs based on read count threshold'),
                             radioButtons(
                               ns('cutoff_method'), 'Set filter cutoff by:',
                               c('Read count' = 'abs_count',
                                 'Percent of total read count of each sample' = 'percent_sample',
                                 'Percent of total read count of dataset' = 'percent_total'),
                               selected = character(0))),
                             fluidRow(
                               column(width = 2, 
                                 tags$div(
                                   br(), 'min: 0',br(),
                                   'max:', textOutput(ns('cutoff_max'), inline = TRUE))),
                               column(width = 6,
                                    uiOutput(ns('asv_cutoff_ui')))),
                             fluidRow(tags$b(textOutput(ns('asv_cutoff_msg'))))
                             )),
                       hidden(
                         div(id = ns('asv_option_select'),
                             h3('Filter ASVs based on selection'),
                             DT::dataTableOutput(ns('asv_table_select'))
                             )) 
                       ),
                       
                column(width = 12, br(),
                       hidden(
                         div(id = ns('asv_remove_div'),
                             tags$b('ASVs to be removed:'),
                             htmlOutput(ns('asv_remove'))
                            ))
                       )),
                column(width = 12,
                       h3(textOutput(ns('preview_asv_title'))),
                       DT::dataTableOutput(ns('preview_asv')))
                       
              )
            )
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
  
  met <- reactive(data_set()$metadata)
  asv <- reactive(data_set()$species_abundance)
  tax <- reactive(data_set()$merged_taxonomy)
  
  # format data tables tidy coding----------------------------------------------
  format_asv <- reactive({
    asv() %>%
      gather('sampleID', 'read_count', -Taxon)
  })
  
  format_tax <-  reactive({
    tax() %>% 
      mutate(Taxon = paste(Phylum, Class, Order, Family, 
                         Genus, Species, sep=";"),
             Taxon = stringr::str_replace_all(Taxon, "_", "__"))
  })
  
 
  
  # subset samples--------------------------------------------------------------
  # show sample table
  output$sample_options_ui <- DT::renderDataTable({
    out <- met()
    DT::datatable(out, options = list(scrollX = TRUE))
  })
  
  # Decide if sample filtering desired
  rows_selected <- reactive({
    req(input$sample_select_prompt)
    
    # select all samples
    if(input$sample_select_prompt == 'all') {
      input$sample_options_ui_rows_all
    }
    else {
      # use metadata to select samples to be included in analysis
      input$sample_options_ui_rows_selected
    }
  })
  
  # map row index to sample names
  all_sample <- reactive(met()$sampleID)
  
  sample_include <- eventReactive(input$sample_select_prompt, {
    req(rows_selected())
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
  observeEvent(input$submit_sample, {
    working_meta <- reactive({
      met() %>%
        dplyr::filter(sampleID %in% sample_include())
    })
    
    output$preview_sample_title <- renderText({
      'Samples included in analysis:'
    })
    
    output$preview_sample <- DT::renderDataTable({
      working_meta()
    })
  })
  
  
  # subset ASVs-----------------------------------------------------------------
  
  # control UI based on filter method--------------------------------------
  # sidebar - filter yes/no
  observeEvent(input$asv_select_prompt, {
    toggle("asv_filter_options_ui", condition = input$asv_select_prompt == 'some')
    toggle('asv_filter_div', condition = input$asv_select_prompt == 'some')
  })
  
  # main panel
  # by read count
  observeEvent(input$asv_filter_options, {
    toggle(id = 'asv_option_count', 
           condition = stringr::str_detect(input$asv_filter_options, 'asv_by_count'))
  })
  
  
  # by selected ASV
  observeEvent(input$asv_filter_options, {
    toggle(id = 'asv_option_select',
           condition = stringr::str_detect(input$asv_filter_options, 'asv_by_select'))
    toggle(id = 'asv_remove_div', condition = length(input$asv_filter_options) > 0)
  })
  
  # ui for  cut-off threshold
  ui_entry <- eventReactive(input$cutoff_method, {
    
    if(input$cutoff_method == 'abs_count') {
      label <- 'Read count cut-off:'
      max_cutoff <- max(format_asv()$read_count)
      step <- 1000
      default_value <- 1

      msg <- 'Removing all sequences with a read count of less than REPLACE'
    }
    if(input$cutoff_method == 'percent_sample') {
      label <- 'Read count cut-off (% of sample total):'
      max_cutoff <- 100
      step <- 1
      default_value = 0.01

      msg <- 'Removing all seqeunces with a read count that is less than REPLACE% of sample total read count'
    }
    if(input$cutoff_method == 'percent_total') {
      label <- 'Read count cut-off (% of dataset total):'
      max_cutoff <- 100
      step <- 1
      default_value = 0.01

      msg <- 'Removing all sequences with a read count that is less than REPLACE% of dataset total read count'
    }
    
    list('label' = label, 'max_cutoff' = max_cutoff, 'step' = step, 
         'default_value' = default_value, 'msg' = msg) 
  })

  output$asv_cutoff_ui <- renderUI({
    
    numericInput(ns('asv_cutoff'), label = ui_entry()$label,
                 value = ui_entry()$default_value, step = ui_entry()$step,
                 min = 0, max = ui_entry()$max_cutoff)
  })
  
  output$cutoff_max <- renderText(ui_entry()$max_cutoff)
  output$asv_cutoff_msg <- renderText({
    stringr::str_replace(ui_entry()$msg, 'REPLACE', as.character(input$asv_cutoff))
  })

  # by selecting ASVs
  output$asv_table_select <- DT::renderDataTable({
    out <- format_tax() %>%
      left_join(asv(), 'Taxon') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum)
    
    DT::datatable(out, options = list(scrollX = TRUE))
  })
  
  output$check <- renderPrint({

  })
  
  # define ASVs to be removed---------------------------------------------------
  to_remove <- reactive({
    
    if(input$asv_filter_options == 'asv_by_count') {
      req(input$cutoff_method)
      
      # count cut-off
      if(input$cutoff_method == 'abs_count') {
        
        out <- format_asv() %>%
          filter(read_count < input$asv_cutoff)
      }
      # cut-off based on percent of sample total
      if(input$cutoff_method == 'percent_sample') {
        out <- format_asv() %>%
          group_by(sampleID) %>%
          mutate(sample_total = sum(read_count),
                 rel_abund = read_count / sample_total * 100) %>%
          filter(rel_abund < input$asv_cutoff) %>%
          select(-sample_total, -rel_abund)
        
      }
      # cut-off based on percent of dataset total
      if(input$cutoff_method == 'percent_total') {
        
        dataset_total <- sum(format_asv()$read_count)
        
        out <- format_asv() %>%
          mutate(rel_abund = read_count / dataset_total * 100) %>%
          filter(rel_abund < input$asv_cutoff) %>%
          select(-rel_abund)
      }
    }

    # filter ASV based on selection---------------------------------------------
    if(input$asv_filter_options == 'asv_by_select') {
      req(input$asv_table_select_rows_selected)
      
      out <- format_tax() %>%
        left_join(asv(), 'Taxon') %>%
        arrange(Kingdom, Phylum) %>%
        slice(c(input$asv_table_select_rows_selected)) %>%
        gather('sampleID', 'read_count', -Taxon)
    }

    unique(out$Taxon)
  })

  # show ASVs removed-----------------------------------------------------------
  output$asv_remove <- renderText({
    to_remove()
  })
  
  # filter ASVs based on set cutoff---------------------------------------------

  observeEvent(input$submit_asv, {
    
    working_asv <- reactive({
      if(input$asv_select_prompt == 'some') {
        req(input$asv_filter_options)
        format_asv() %>%
          filter(!Taxon %in% to_remove())  
      }
      else {
        format_asv()
      }
    })
    
    output$preview_asv_title <- renderText({
      'ASVs included in analysis'
    })
    
    output$preview_asv <- DT::renderDataTable({
      out <- format_tax() %>%
        right_join(working_asv(), 'Taxon') %>%
        spread(sampleID, read_count) %>%
        select(-Taxon, -sequence) %>%
        arrange(Kingdom, Phylum)
      
      DT::datatable(out, options = list(scrollX = TRUE))
    })
  })
  
  # store prepared data to pass on to next next module--------------------------
  working_set <- reactive({
    # keep in wide format to be consistent with db format
    
    list(metadata = working_meta(),
         asv = working_asv() %>% spread(sampleID, read_count),
         tax = format_tax() %>% filter(Taxon %in% working_asv()$Taxon))
  })
  # jump to next tab
  observeEvent(input$next_tab, {
    updateTabsetPanel(session, "tabs",
                      selected = "overview")
  })
  # return dataset
  cross_module = reactiveValues()
  observe({cross_module$data_db <- working_set()})
  return(cross_module)
}
    
## To be copied in the UI
# mod_setup_ui("setup_ui_1")
    
## To be copied in the server
# callModule(mod_setup_server, "setup_ui_1")
 
