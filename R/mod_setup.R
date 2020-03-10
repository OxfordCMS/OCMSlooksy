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
          br(),
          menuItem('Task Info', tabName = 'info_tab_setup', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Filter Samples', tabName = 'filter_sample'),
          menuItem('Filter Features', tabName = 'filter_asv'),
          menuItem('Read Count Transformation', tabName = "transform_asv"),
          
          # filter samples------------------------------------------------------
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
              actionButton(ns('submit_sample'), "Filter samples")
            )),
          
          # filter asvs---------------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'filter_asv'",
            br(), hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')),
            
            fixedPanel(
              radioButtons(ns('asv_select_prompt'), 
                           "Features to exclude from analysis:",
                           choices = c('Use all features' = 'all', 
                                       'Filter features' = 'some'),
                           selected = 'all'),
              
              hidden(
                div(id = ns('asv_filter_options_ui'),
                    radioButtons(
                      ns('asv_filter_options'), 'Filter features by:',
                      choices = c('read count' = 'asv_by_count',
                                  'selection' = 'asv_by_select'),
                      selected = character(0))
                    )),
              actionButton(ns('submit_asv'), "Filter features"))),
            
          # transform counts----------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'transform_asv'",
            br(), hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')),
            fixedPanel(
              radioButtons(ns('transform_method'), "Transformation method:",
                                 choices = c('none' = 'none', 
                                             'centre log-ratio' = 'clr',
                                             'additive log-ratio' = 'alr'),
                                 selected = 'clr'),
              actionButton(ns('submit_transform'), "Transform Counts")
            )
           
          )
          )),
      
      # main panel--------------------------------------------------------------
      dashboardBody(
        box(width = '100%', br(),br(), br(),
            
            # fluidRow(width = 12,
            #         h3('Check Box'),
            #         verbatimTextOutput(ns('check'))),
            
            tabItems(
              # main page---------------------------------------------------------
              tabItem(
                tabName = 'info_tab_setup',
                column(width = 12, 
                  h1('Prepare Data for Analysis'),
                  tags$div("Before starting data exploration, it may be desirable to specify which samples and features should be included in all subsequent analyses. This ensures that the analysis is performed on a dataset that is relevant to the current research question.
                  
                  Read counts also need to be normalized to compensate for the fact the marker gene sequencing produces compositional data, rather than absolute counts. Please see [link] for more information on compositional data."))
              ),
              # filter samples--------------------------------------------------
              tabItem(
                tabName = 'filter_sample', 
                column(width = 12,
                       h1('Filter Samples'), br(),
                       tags$div(
                         "It may be desirable to perform analysis on a subset of samples if certain samples did not pass QC, or are no longer relevant to the current research question.")),
                br(), br(),
                hidden(div(id = ns('sample_filter_div'),
                   column(width = 8,
                          DT::dataTableOutput(ns('sample_options_ui'))),
                   column(width = 4,
                          wellPanel(tags$b('Selected samples:'),
                          htmlOutput(ns('sample_select')))))),
                column(width = 12,
                       h3(textOutput(ns('preview_sample_title'))),
                       DT::dataTableOutput(ns('preview_sample')))),
              
              # filter asv------------------------------------------------------
              tabItem(
                tabName = 'filter_asv',
                column(width = 12,
                  h1('Filter Features'),
                  tags$div(
                    "It may be desirable to perform analysis on a subset of features if certain taxa are considered contamination, or are no longer relevant to the current research question.",
                    br(),br(),
                    tags$em(
                      tags$b("NB:"), "Filtering sequences based on sequence quality and minimum count threshold has already been performed during quality control processing of the dataset. Filtering features at this stage should only be done if you have additional reasoning for omitting certain sequences or features"),
                    br()),
                
                div(id = ns('asv_filter_div'),
                column(width = 12,
                  hidden(
                   div(id = ns('asv_option_count'),
                       column(width = 5, br(),
                              wellPanel(
                                h3('Filter features based on read count threshold'),
                                radioButtons(
                                  ns('cutoff_method'), 'Set filter cutoff by:',
                                  c('Read count' = 'abs_count',
                                    'Percent of total read count of each sample' = 'percent_sample',
                                    'Percent of total read count of dataset' = 'percent_total'),
                                  selected = character(0)))),
                       column(width = 5, br(),
                         hidden(
                           div(id = ns('cutoff_limit'),
                               wellPanel(
                                 tags$div('min: 0', br(),
                                    'max:', 
                                    textOutput(ns('cutoff_max'), inline = TRUE)),
                                 br(),
                                 uiOutput(ns('asv_cutoff_ui')),
                                 tags$b(textOutput(ns('asv_cutoff_msg')))))))
                       )),
                hidden(
                 div(id = ns('asv_option_select'),
                     column(
                       width = 12,
                          h3('Filter features based on selection'),
                          DT::dataTableOutput(ns('asv_table_select'))
                     ))))),
                       
                column(width = 12, br(),
                       hidden(
                         div(id = ns('asv_remove_div'),
                             wellPanel(tags$b('features to be removed:'),
                                htmlOutput(ns('asv_remove')))
                            ))),
                column(width = 12,
                       h3(textOutput(ns('preview_asv_title'))),
                       DT::dataTableOutput(ns('preview_asv')))
              )),
              
              # asv transformation----------------------------------------------
              tabItem(
                tabName = 'transform_asv',
                column(width = 12,
                       h1('Transform Read Counts'),
                       tags$div("Surveying an ecosystem based on DNA sequence produces compositional data due to the constant sum constraint of sequencing platforms. Sequence read 'count' is not directly reflective of the absolute count of sequences in the sampled environment because the changes in the absolute abundance of a sequence can only be observed at the expense of other sequences. Lack of independance in sequence counts can result in spurious correlations, ultimately leading to false associations between variables. Further detail on compositional data analysis are discussed by [Greg Gloor and others, link].", 
                                br(), 
                                "Applying log transformations corrects for the 'closure problem' [Aitcheson reference, link], such ecological and statistical tools are applicable to sequence data sets. The log transformations will be applied to the filtered data. Transformed data will be used throughout the analysis, where necessary. Instances of its usage is recorded in the final [report]."),
                       DT::dataTableOutput(ns('preview_transform')))
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
  asv <- reactive(data_set()$merged_abundance_id)
  tax <- reactive(data_set()$merged_taxonomy)
  
  # format data tables tidy coding----------------------------------------------
  format_asv <- reactive({
    asv() %>%
      gather('sampleID', 'read_count', -featureID)
  })
  
  # subset samples--------------------------------------------------------------
  # show sample table
  observeEvent(input$sample_select_prompt, {
    toggle("sample_filter_div", condition = input$sample_select_prompt != 'all')
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
  working_meta <- eventReactive(input$submit_sample, {
      met() %>%
        filter(sampleID %in% sample_include())
    })
  
  observeEvent(input$submit_sample, {
    output$preview_sample_title <- renderText({
      'Samples included in analysis:'
    })
    
    output$preview_sample <- DT::renderDataTable({
      DT::datatable(working_meta(), extensions = 'Buttons', 
                    options = list(scrollX = TRUE, 
                                   dom = 'Blfrtip', buttons = c('copy','csv')))
    })
  })
  
  # update ASV with filtered samples
  asv_filt_samp <- eventReactive(input$submit_sample, {
    format_asv() %>%
      filter(sampleID %in% working_meta()$sampleID)
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
    show(id = "cutoff_limit")
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
      max_cutoff <- max(asv_filt_samp()$read_count)
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
  
  output$cutoff_max <- renderText({
    req(input$cutoff_method)
    ui_entry()$max_cutoff})
  output$asv_cutoff_msg <- renderText({
    stringr::str_replace(ui_entry()$msg, 'REPLACE', as.character(input$asv_cutoff))
  })

  # by selecting ASVs
  output$asv_table_select <- DT::renderDataTable({
    out <- tax() %>%
      left_join(asv(), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)
    
    DT::datatable(out, filter = 'top', options = list(scrollX = TRUE))
  })
  

  # define ASVs to be removed---------------------------------------------------
  to_remove <- reactive({
    
    if(input$asv_filter_options == 'asv_by_count') {
      req(input$cutoff_method)
      
      # count cut-off
      if(input$cutoff_method == 'abs_count') {
        
        out <- asv_filt_samp() %>%
          filter(read_count < input$asv_cutoff)
      }
      # cut-off based on percent of sample total
      if(input$cutoff_method == 'percent_sample') {
        out <- asv_filt_samp() %>%
          group_by(sampleID) %>%
          mutate(sample_total = sum(read_count),
                 rel_abund = read_count / sample_total * 100) %>%
          filter(rel_abund < input$asv_cutoff) %>%
          select(-sample_total, -rel_abund)
        
      }
      # cut-off based on percent of dataset total
      if(input$cutoff_method == 'percent_total') {
        
        dataset_total <- sum(asv_filt_samp()$read_count)
        
        out <- asv_filt_samp() %>%
          mutate(rel_abund = read_count / dataset_total * 100) %>%
          filter(rel_abund < input$asv_cutoff) %>%
          select(-rel_abund)
      }
    }

    # filter ASV based on selection---------------------------------------------
    if(input$asv_filter_options == 'asv_by_select') {
      req(input$asv_table_select_rows_selected)
      
      out <- tax() %>%
        left_join(asv(), 'featureID') %>%
        select(-sequence, -Taxon) %>%
        arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
        slice(c(input$asv_table_select_rows_selected)) %>%
        gather('sampleID', 'read_count', -featureID)
    }

    unique(out$featureID)
  })
  
  # show ASVs removed-----------------------------------------------------------
  output$asv_remove <- renderText({
    HTML(paste(to_remove(), collapse = "<br/>"))
  })
  
  # filter ASVs based on set cutoff---------------------------------------------
  working_asv <- eventReactive(input$submit_asv, {
    if(input$asv_select_prompt == 'some') {
      req(input$asv_filter_options)
      asv_filt_samp() %>%
        filter(!featureID %in% to_remove())  
    }
    else {
      asv_filt_samp()
    }
  })
    
  observeEvent(input$submit_asv, {
    output$preview_asv_title <- renderText({
      'Features included in analysis'
    })
    
    output$preview_asv <- DT::renderDataTable({
      out <- tax() %>%
        left_join(working_asv() %>% spread(sampleID, read_count), 
                  'featureID') %>%
        select(-Taxon, -sequence) %>%
        arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)
      
      DT::datatable(out, extensions = 'Buttons', 
                    options = list(scrollX = TRUE, 
                                   dom = 'Blfrtip', buttons = c('copy','csv')))
    })
  })
  
  output$check <- renderPrint({

  })
  
  # transform ASVs--------------------------------------------------------------
  asv_transform <- eventReactive(input$submit_transform, {
    req(input$transform_method)
    
    asv_df <- working_asv() %>% 
      spread(sampleID, read_count) %>% 
      as.data.frame()
    rownames(asv_df) <- asv_df$featureID
    asv_df <- asv_df[, colnames(asv_df) != 'featureID']
    
    if(input$transform_method == 'clr') {
      ## generate Monte Carlo samples from Dirichlet distribution
      ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
      ## use geometric mean abundance of features
      
      asv_clr <- ALDEx2::aldex.clr(asv_df, conds = working_meta()$sampleID)
      clr_instance <- lapply(ALDEx2::getMonteCarloInstances(asv_clr),
                             function(m){t(apply(m,1,median))})
      ## samples in columns
      clr_df <- data.frame(matrix(unlist(clr_instance), ncol = length(clr_instance),
                                  byrow = FALSE,
                                  dimnames = list(colnames(clr_instance[[1]]),
                                                  names(clr_instance))),
                           stringsAsFactors=FALSE)
      out <- clr_df
    }
    else {
      out <- asv_df
    }
    out
  })
  
  output$preview_transform <- DT::renderDataTable({
    DT::datatable(asv_transform(), extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  # store prepared data to pass on to next next module--------------------------
  working_set <- reactive({
    req(input$sample_select_prompt, input$asv_select_prompt)
    # keep in wide format to be consistent with db format
    
    list(metadata = working_meta(),
         asv = working_asv() %>% spread(sampleID, read_count),
         t_asv = asv_transform(),
         tax = tax() %>% 
           filter(featureID %in% working_asv()$featureID)
         )
  })
  # jump to next tab
  observeEvent(input$next_tab, {
    updateTabsetPanel(session, "tabs",
                      selected = "overview")
  })
  # return dataset
  cross_module <- reactiveValues()
  observe({cross_module$data_db <- working_set()})
  return(cross_module)
}
    
## To be copied in the UI
# mod_setup_ui("setup_ui_1")
    
## To be copied in the server
# callModule(mod_setup_server, "setup_ui_1")
 
