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
              radioButtons(ns('transform_method'),
                           tags$div(title = "CLR transforamtion may take a few minutes for large datasets", "Transformation method:"),
                                 choices = c('none' = 'none', 
                                             'centre log-ratio' = 'clr',
                                             'log10 of percent abundance' = 'log10',
                                             'percent abundance' = 'percent'),
                                 selected = 'clr'),
            actionButton(ns('submit_transform'), "Transform Counts")
            )
           
          )
          )),
      
      # main panel--------------------------------------------------------------
      dashboardBody(
        box(width = '100%', br(),br(), br(),
            
            fluidRow(width = 12,
                    h3('Check Box'),
                    verbatimTextOutput(ns('check'))),
            
            tabItems(
              # main page-------------------------------------------------------
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
                fluidRow(
                  column(width = 8,
                    hidden(div(
                      id = ns('sample_filter_div'),
                      column(width = 12,
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
                      wellPanel(tags$b('Selected samples:'),
                                htmlOutput(ns('sample_select'))
                                )
                    )
                  ))
                )
              ),
              
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
                ), # end column 1 description text
                div(
                  id = ns('asv_filter_div'),
                  column(width = 12, # begin asv_option_count column 
                    hidden(div(
                      id = ns('asv_option_count'),
                      column(
                        width = 3, br(),
                        wellPanel(
                          h3('Filter features based on read count threshold'),
                          radioButtons(
                            ns('cutoff_method'), 'Set filter cutoff by:',
                            c('Read count' = 'abs_count',
                              'Percent of total read count of each sample' = 'percent_sample',
                              'Percent of total read count of dataset' = 'percent_total'),
                            selected = 'abs_count')
                        )
                      ),
                      column(
                        width = 4, br(),
                        hidden(div(
                          id = ns('cutoff_limit'),
                          wellPanel(
                            tags$div('min: 0', br(),
                              'max:', 
                              textOutput(ns('cutoff_max'), inline = TRUE)),
                            br(),
                            uiOutput(ns('asv_cutoff_ui')),
                            uiOutput(ns('prevalence_ui')),
                            tags$b(textOutput(ns('asv_cutoff_msg')) %>%
                                     shinycssloaders::withSpinner()),
                            checkboxInput(ns("show_prev_plot"), "Show read prevalence plot")
                          )
                        )) # end cutoff_limit
                      ),
                      column(
                        width = 5, br(),
                        hidden(div(
                          id = ns('prev_read_plot_div'),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Aggregated view", 
                                     plotlyOutput(ns('prev_agg_plot')) %>% 
                                       shinycssloaders::withSpinner()),
                            tabPanel("Expanded view",
                                     plotlyOutput(ns('prev_read_plot')) %>% 
                                       shinycssloaders::withSpinner())
                          )
                        ))
                      )
                    )) # end asv_option_count
                  ), # end asv_option_count column
                ), # end asv_filter_div
                column(
                  width = 12,
                  hidden(div(
                    id = ns('asv_option_select'),
                    h3('Filter features based on selection'),
                    DT::dataTableOutput(ns('asv_table_select')) %>% 
                      shinycssloaders::withSpinner()
                  )) # end asv_option_select
                ), 
                column(
                  width = 12, br(),
                  hidden(div(
                    id = ns('asv_remove_div'),
                    wellPanel(textOutput(ns('asv_remove')))
                  )) # end asv_remove_div
                ),
                column(
                  width = 12, 
                  hidden(div(
                    id = ns('secondary_check_div'),
                    wellPanel(
                      tags$b(textOutput(ns('secondary_filter'))),
                      htmlOutput(ns('secondary_filter_samples'))
                    )
                  ))
                ),
                column(
                  width = 12,
                  h3(textOutput(ns('preview_asv_title'))),
                  DT::dataTableOutput(ns('preview_asv'))  %>% 
                    shinycssloaders::withSpinner()
                )
              ), # end tabItem
              
              # asv transformation----------------------------------------------
              tabItem(
                tabName = 'transform_asv',
                column(width = 12,
                       h1('Transform Read Counts'),
                       tags$div("Surveying an ecosystem based on DNA sequence produces compositional data due to the constant sum constraint of sequencing platforms. Sequence read 'count' is not directly reflective of the absolute count of sequences in the sampled environment because the changes in the absolute abundance of a sequence can only be observed at the expense of other sequences. Lack of independance in sequence counts can result in spurious correlations, ultimately leading to false associations between variables. Further detail on compositional data analysis are discussed by [Greg Gloor and others, link].", 
                                br(), 
                                "Applying log transformations corrects for the 'closure problem' [Aitcheson reference, link], such ecological and statistical tools are applicable to sequence data sets. The log transformations will be applied to the filtered data. Transformed data will be used throughout the analysis, where necessary. Instances of its usage is recorded in the final [report]."),
                       DT::dataTableOutput(ns('preview_transform')) %>%
                         shinycssloaders::withSpinner())
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
      met() %>%
        filter(sampleID %in% sample_include())
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
           condition = grepl('asv_by_count', input$asv_filter_options))
    show(id = "cutoff_limit")
  })
  
  observeEvent(input$show_prev_plot, {
    toggle(id = 'prev_read_plot_div', condition = input$show_prev_plot == TRUE)
  })

  # by selected ASV
  observeEvent(input$asv_filter_options, {
    toggle(id = 'asv_option_select',
           condition = grepl('asv_by_select', input$asv_filter_options))
    toggle(id = 'asv_remove_div',
           condition = grepl('asv_by_select', input$asv_filter_options))
  })

  observeEvent(input$submit_asv, {
    toggle(id = 'secondary_check_div', condition = secondary_check() == TRUE)
  })
  # ui for prevalence threshold
  output$prevalence_ui <- renderUI({
    nsample <- length(unique(samp_filtered()$sampleID))
    numericInput(ns('prevalence'),
                 "Feature prevalence (# of samples):",
                 min = 1, max = nsample, value = round(nsample * 0.05))
  })
  # ui for  cut-off threshold
  ui_entry <- eventReactive(input$cutoff_method, {
    req(input$cutoff_method)
    if(input$cutoff_method == 'abs_count') {
      label <- 'Read count cut-off:'
      max_cutoff <- max(samp_filtered()$read_count)
      step <- 1000
      default_value <- 1

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2 in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }
    if(input$cutoff_method == 'percent_sample') {
      label <- 'Read count cut-off (% of sample total):'
      max_cutoff <- 100
      step <- 0.01
      default_value = 0.01

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of sample total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }
    if(input$cutoff_method == 'percent_total') {
      label <- 'Read count cut-off (% of dataset total):'
      max_cutoff <- 100
      step <- 0.01
      default_value = 0.01

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of dataset total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }

    list('label' = label, 'max_cutoff' = max_cutoff, 'step' = step,
         'default_value' = default_value, 'msg' = msg)
  })

  output$asv_cutoff_ui <- renderUI({
    req(input$cutoff_method)
    numericInput(ns('asv_cutoff'), label = ui_entry()$label,
                 value = ui_entry()$default_value, step = ui_entry()$step,
                 min = 0, max = ui_entry()$max_cutoff)
  })

  output$cutoff_max <- renderText({
    req(input$cutoff_method)
    ui_entry()$max_cutoff})

  output$asv_cutoff_msg <- renderText({
    req(input$cutoff_method)
    tot_feat <- samp_filtered() %>% filter(read_count > 0)
    tot_feat <- length(unique(tot_feat$featureID))
    rel_feat <- length(to_keep()) / tot_feat * 100
    rel_feat <- round(rel_feat, 1)
    tot_samp <- length(unique(met_filtered()$sampleID))
    rel_samp <- as.numeric(input$prevalence) / tot_samp * 100
    rel_samp <- round(rel_samp, 1)
    out <- gsub("REPLACE1", as.character(length(to_keep())), ui_entry()$msg)
    out <- gsub("TOT_FEAT", as.character(tot_feat), out)
    out <- gsub("REL_FEAT", as.character(rel_feat), out)
    out <- gsub("REL_FEAT", as.character(rel_feat), out)
    out <- gsub('REPLACE2', as.character(input$asv_cutoff), out)
    out <- gsub("REPLACE3", as.character(input$prevalence), out)
    out <- gsub("TOT_SAMP", as.character(tot_samp), out)
    out <- gsub("REL_SAMP", as.character(rel_samp), out)
  })

  # by selecting ASVs
  output$asv_table_select <- DT::renderDataTable({
    req(input$asv_filter_options)


    out <- tax() %>%
      left_join(samp_filtered(), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      spread(sampleID, read_count) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # by default, only show first 50 samples + 8 tax columns
    if(ncol(out) <= 58) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(out) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 59:ncol(out) # index of columns to hide
      vis_val <- FALSE
    }

    DT::datatable(out, filter = 'top',
                  extensions = list(c('Buttons', 'FixedColumns')),
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv'),
                                   list(extend = 'colvis')),
                    filter = 'top',
                    columnDefs = list(
                      list(targets = col_ind, visible = vis_val)
                    )))
  })


  # prepare asv dataframe-------------------------------------------------------
  working_asv <- reactive({
    
    if(input$asv_select_prompt == 'some') {
      dataset_total <- sum(samp_filtered()$read_count)

      out <- samp_filtered() %>%
        # calculate relative abundance as dataset total
        mutate(ds_rel_abund = read_count / dataset_total * 100) %>%
        # calculate relative abundance as sample total
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        group_by(sampleID, featureID) %>%
        mutate(samp_rel_abund = read_count / sample_total * 100) %>%
        ungroup() 
      out

    }
    else {
      samp_filtered()
    }
  })
  
  # define ASVs to keep---------------------------------------------------------
  to_keep <- reactive({
    req(input$asv_filter_options)
    if(input$asv_filter_options == 'asv_by_count') {
      req(input$cutoff_method, input$asv_cutoff, input$prevalence)

      # count cut-off
      if(input$cutoff_method == 'abs_count') {

        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(read_count >= input$asv_cutoff)) %>%
          filter(read_count >= input$asv_cutoff & prev_observed >= input$prevalence)
      }
      # cut-off based on percent of sample total
      if(input$cutoff_method == 'percent_sample') {
        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(samp_rel_abund >= input$asv_cutoff)) %>%
          filter(samp_rel_abund >= input$asv_cutoff & prev_observed >= input$prevalence)

      }
      # cut-off based on percent of dataset total
      if(input$cutoff_method == 'percent_total') {
        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(ds_rel_abund >= input$asv_cutoff)) %>%
          filter(ds_rel_abund >= input$asv_cutoff & prev_observed >= input$prevalence)
      }
    }

    # filter ASV based on selection---------------------------------------------
    if(input$asv_filter_options == 'asv_by_select') {
      req(input$asv_table_select_rows_selected)

      out <- tax() %>%
        left_join(samp_filtered(), 'featureID') %>%
        select(-Taxon, -sequence) %>%
        spread(sampleID, read_count) %>%
        arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
        slice(-c(input$asv_table_select_rows_selected)) %>%
        gather('sampleID', 'read_count', -featureID)
    }

    unique(out$featureID)
  })

  # show ASVs removed-----------------------------------------------------------
  output$asv_remove <- renderText({
    req(input$asv_filter_options, input$asv_table_select_rows_selected)
    featID <- unique(samp_filtered()$featureID)
    sprintf("Removing %s Features", length(featID[!featID %in% to_keep()]))
  })
  
  # check
  output$check <- renderPrint({
    head(asv_filtered2())
  })
  
  # giving preview on read and prevalence
  output$prev_agg_plot <- renderPlotly({
    scaleFUN <- function(x) sprintf("%.0f", x)
    
    req(input$asv_filter_options)
    
    if(input$cutoff_method == 'abs_count') {
      y = 'agg_count'
      ylab <- 'Aggregated Read count'
    }
    if(input$cutoff_method == 'percent_total') {
      y = 'agg_ds_rel'
      ylab <- 'Aggregated Relative abundance\n(% of total reads)'
    }
    if(input$cutoff_method == 'percent_sample') {
      y = 'agg_samp_rel'
      ylab <- 'Aggregated Relative abundance\n(% of sample)'
    }

    pdata <- working_asv() %>% 
      group_by(featureID) %>%
      # number of samples in which asv meets read cutoff
      mutate(prev_observed = sum(read_count >= input$asv_cutoff)) %>%
      group_by(featureID) %>%
      summarise(prev_observed = prev_observed,
                agg_count = sum(read_count),
                agg_samp_rel = sum(samp_rel_abund),
                agg_ds_rel = sum(ds_rel_abund)) %>%
      mutate(colour = ifelse(featureID %in% to_keep(), 
                             'to keep', 'to remove')) %>%
      distinct()
      
    p <- ggplot(pdata, aes_string(x = 'prev_observed', y = y, colour = 'colour')) +
      geom_point(aes(text=sprintf("featureID: %s", featureID)), alpha = 0.5, size = 2) +
      xlab('ASV prevalence (# of samples)') +
      ylab(ylab) +
      scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met_filtered()))) +
      scale_y_continuous(trans = 'log10') +
      theme_bw(12) +
      theme(legend.position = 'bottom')
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = -0.5, y = -1))
  })
  
  output$prev_read_plot <- renderPlotly({
    scaleFUN <- function(x) sprintf("%.0f", x)
    
    req(input$asv_filter_options)
    
    if(input$cutoff_method == 'abs_count') {
      y = 'read_count'
      ylab <- 'Read count'
    }
    if(input$cutoff_method == 'percent_total') {
      y = 'ds_rel_abund'
      ylab <- 'Relative abundance\n(% of total reads)'
    }
    if(input$cutoff_method == 'percent_sample') {
      y = 'samp_rel_abund'
      ylab <- 'Relative abundance\n(% of sample)'
    }

    pdata <- working_asv() %>% 
      group_by(featureID) %>%
      # number of samples in which asv meets read cutoff
      mutate(prev_observed = sum(read_count >= input$asv_cutoff),
             colour = ifelse(featureID %in% to_keep(), 'to keep', 'to remove')) %>%
      distinct()
    
    p <- ggplot(pdata, aes_string(x = 'prev_observed', y = y, colour = 'colour')) +
      geom_point(aes(text=sprintf("featureID: %s<br>sampleID: %s", 
                                  featureID, sampleID)), 
                 alpha = 0.5, size = 2) +
      xlab('ASV prevalence (# of samples)') +
      ylab(ylab) +
      scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met_filtered()))) +
      scale_y_continuous(trans = 'log10') +
      theme_bw(12) +
      theme(legend.position = 'bottom')
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = -0.5, y = -1))
  })
  
  # filter ASVs based on set cutoff/selection-----------------------------------
  asv_filtered <- eventReactive(input$submit_asv, {

    if(input$asv_select_prompt == 'some') {
      working_asv() %>%
        filter(featureID %in% to_keep())  %>%
        select(-ds_rel_abund, -samp_rel_abund, -sample_total)
    }
    else {
      working_asv()
    }
  })
  
  
  # secondary check for samples with no reads-----------------------------------
  sample_total <- reactive({
    asv_filtered() %>%
      group_by(sampleID) %>%
      summarise(sample_total = sum(read_count))
  })
  secondary_check <- reactive({
    any(sample_total()$sample_total == 0)
  })
  
  # identify empty samples
  empty_sample <- reactive({
    out <- sample_total() %>%
      filter(sample_total == 0)
    out$sampleID
  })
  
  n_empty <- reactive({length(empty_sample())})
  
  output$secondary_filter <- renderText({
    sprintf("%s samples contained 0 reads after ASV filtering. The following samples have been removed:", n_empty())
  })
  
  output$secondary_filter_samples <- renderUI({
    HTML(paste(empty_sample(), collapse = '<br/>'))
  })
  
  # update asv_filtered
  asv_filtered2 <- eventReactive(input$submit_asv, {
    if(secondary_check()) {
      asv_filtered() %>%
        filter(!sampleID %in% empty_sample())
    }
    else {
      asv_filtered()
    }
  })
  
  # update met_filtered
  met_filtered2 <- eventReactive(input$submit_asv, {
    if(secondary_check()) {
      met_filtered() %>%
        filter(!sampleID %in% empty_sample())
    }
    else {
      met_filtered()
    }
  })

  output$preview_asv_title <- renderText({
    req(input$submit_asv)
    'Features included in analysis'
  })

  output$preview_asv <- DT::renderDataTable({
    req(input$submit_asv)
    out <- tax() %>%
      right_join(asv_filtered2() %>% 
                   spread(sampleID, read_count), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # by default, only show first 50 samples + 8 tax columns
    if(ncol(out) <= 58) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(out) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 59:ncol(out) # index of columns to hide
      vis_val <- FALSE
    }
    DT::datatable(out, extensions = list(c('Buttons', 'FixedColumns')),
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv'),
                                   list(extend = 'colvis')),
                    fixedColumns=list(leftColumns = 2),
                    columnDefs = list(
                      list(targets = col_ind, visible = vis_val)
                    )))
  })


  # transform ASVs--------------------------------------------------------------

  asv_transform <- eventReactive(input$submit_transform, {
    req(input$transform_method)

    asv_df <- asv_filtered2() %>%
      spread(sampleID, read_count) %>%
      as.data.frame()
    rownames(asv_df) <- asv_df$featureID
    asv_df <- asv_df[, colnames(asv_df) != 'featureID']

    if(input$transform_method == 'clr') {
      ## generate Monte Carlo samples from Dirichlet distribution
      ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
      ## use geometric mean abundance of features

      asv_clr <- ALDEx2::aldex.clr(asv_df, conds = met_filtered2()$sampleID,
                                   useMC = TRUE)
      clr_instance <- lapply(ALDEx2::getMonteCarloInstances(asv_clr),
                             function(m){t(apply(m,1,median))})
      ## samples in columns
      clr_df <- data.frame(matrix(unlist(clr_instance),
                                  ncol = length(clr_instance),
                                  byrow = FALSE,
                                  dimnames = list(colnames(clr_instance[[1]]),
                                                  names(clr_instance))),
                           stringsAsFactors=FALSE)
      out <- clr_df
    }
    if(input$transform_method == 'log10') {
      out <- apply(asv_df, 2, function(x) log10(x + 1*10^-6))
    }
    if(input$transform_method == 'percent') {
      calc <- asv_filtered2() %>%
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count),
               samp_rel_abund = read_count / sample_total * 100) %>%
        ungroup() %>%
        select(-read_count, -sample_total) %>%
        spread(sampleID, samp_rel_abund)
      
      out <- as.data.frame(calc %>% select(-featureID))
      rownames(out) <- calc$featureID
    
    }
    if(input$transform_method == 'none') {
      out <- asv_df
    }
    out
  })

  output$preview_transform <- DT::renderDataTable({
    req(input$submit_transform, input$transform_method)

    # by default, only show first 50 samples + 8 tax columns
    if(ncol(asv_transform()) <= 58) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(asv_transform()) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 59:ncol(asv_transform()) # index of columns to hide
      vis_val <- FALSE
    }

    DT::datatable(asv_transform(),
                  extensions = list(c('Buttons', 'FixedColumns')),
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv'),
                                   list(extend = 'colvis')),
                    fixedColumns=list(leftColumns = 2),
                    columnDefs = list(
                      list(targets = col_ind, visible = vis_val)
                    ))) %>%
      DT::formatRound(column = colnames(asv_transform()), digits = 3)
  })

  # store prepared data to pass on to next next module--------------------------
  working_set <- reactive({
    req(input$sample_select_prompt, input$asv_select_prompt,
        input$transform_method)
    # keep in wide format to be consistent with db format
    tax_filtered <- tax() %>%
      filter(featureID %in% asv_filtered2()$featureID)
    list(met = met_filtered2(),
         asv = asv_filtered2() %>% spread(sampleID, read_count),
         asv_transform = asv_transform(),
         transform_method = input$transform_method,
         tax = tax_filtered,
         asv_gather = asv_filtered2(),
         asv_tax = asv_filtered2() %>% inner_join(tax_filtered, 'featureID'),
         asv_met = asv_filtered2() %>% inner_join(met_filtered2(), 'sampleID'),
         work = asv_filtered2() %>%
           inner_join(tax_filtered, 'featureID') %>%
           inner_join(met_filtered2(), 'sampleID')
         )
  })

  # return dataset
  cross_module <- reactiveValues()
  observe({
    req(input$submit_sample, input$submit_asv, input$submit_transform)
    cross_module$work_db <- working_set()
  })
  return(cross_module)
}
    
## To be copied in the UI
# mod_setup_ui("setup_ui_1")
    
## To be copied in the server
# callModule(mod_setup_server, "setup_ui_1")
 
