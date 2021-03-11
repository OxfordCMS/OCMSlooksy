#' diff_abund UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_diff_abund_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(
          id = 'menu', br(),
          menuItem('Task Info', tabName = 'info_tab_diff', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Aggregate Features', tabName = 'agg_diff_tab'),
          menuItem('Filter Features', tabName = 'filter_diff_tab'),
          menuItem('DESeq2', tabName = 'deseq_tab'),
          menuItem('Heatmap', tabName = 'hmap_tab'),
          menuItem('Boxplot', tabName = 'box_tab'),
          # aggregate menu controls---------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'agg_diff_tab'",
            hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')
            ),
            fixedPanel(
              radioButtons(ns('aggregate_by'), "Aggregate counts by:",
                           choices = c('featureID','Kingdom','Phylum','Class',
                                       'Order','Family','Genus','Species'),
                           selected = 'Genus'),
              withBusyIndicatorUI(
                actionButton(ns('agg_calculate'), "Aggregate")
              )
            )
          ),
          # filter menu controls------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'filter_diff_tab'",
            hr(),
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
                      selected = "asv_by_count")
                )),
              withBusyIndicatorUI(
                actionButton(ns('submit_asv'), "Filter features")))  
          ),
          # Deseq menu controls---------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'deseq_tab'",
            hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')
            ),
            fixedPanel(
              width = 225,
              uiOutput(ns('variable_ui')),
              uiOutput(ns('group1_ui')),
              uiOutput(ns('group2_ui')),
              withBusyIndicatorUI(
                actionButton(ns('deseq_submit'), "Compare Groups")
              )
            )
          )
        )
      ),
      # dashboard body----------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          tabItems(
            # info tab body-----------------------------------------------------
            tabItem(
              tabName = 'info_tab_diff',
              h1("Differential Abundance"),
              tags$div(
                "Compare groups to see what taxa are differentially abundant between the groups. This task uses DESeq2 and provides a couple different methods of visualising the results. "
              )
            ), # end tabItem
            # aggregate tab body------------------------------------------------
            tabItem(
              tabName = 'agg_diff_tab',
              mod_aggregate_ui(ns("aggregate_ui_1"))
            ), # end tabItem
            # filter tab body---------------------------------------------------
            tabItem(
              tabName = "filter_diff_tab",
              mod_filterfeat_ui(ns("filterfeat_ui_1"))
            ), # end tabItem
            tabItem(
              tabName = "deseq_tab",
              h1("Differential Abundance with DESeq2"),
              tags$div(
                "Compare groups using DESeq2. DESeq2 normalises and transforms data by [...]. DESeq2 assess differential abundance based on the assumption of a negative bionmial distribution..."
              )
            )
          ) # end tabItems
        ) # end box
      ) # end dashboardBody
    ) # end dashboard Page
  ) # end taglist
}
    
#' diff_abund Server Function
#'
#' @noRd 
mod_diff_abund_server <- function(input, output, session, improxy){
  ns <- session$ns
 
  # initiate value to pass into submodules--------------------------------------
  bridge <- reactiveValues()
  observe({
    bridge$qualfilt_db <- improxy$work_db
    bridge$agg_input <- list(
      aggregate_by = input$aggregate_by,
      agg_calculate = input$agg_calculate
    )
  })
  # initiate list to pass onto report submodule
  for_report <- reactiveValues()
  
  # store values to pass to report
  observe({
    req(input$agg_calculate)
    for_report$params <- list(
      # sample filter
      met1 = improxy$work_db$met,
      sample_select_prompt = improxy$work_db$sample_select_prompt,
      sample_select = improxy$work_db$sample_select
    )
  })
  # aggregate features----------------------------------------------------------
  withBusyIndicatorServer('agg_calculate', 'diff_abund_ui_1', {
    agg_output <- callModule(mod_aggregate_server, "aggregate_ui_1", bridge)
    
  })
  
  # store data in reactiveValues to pass onto submodules
  observe({
    req(input$agg_calculate)
    if(!is.null(agg_output$output)) {
      tax_entry <- dplyr::select(agg_output$output$aggregated_tax, -n_collapse)
      
      # add aggregate features to bridge to be passed to submodules
      bridge$work_db <- list(
        met = improxy$work_db$met,
        asv = agg_output$output$aggregated_count,
        tax = tax_entry
      )
      
    } else { 
      # agg_output starts out as NULL initially. else statement stops that from causing app to crash
      bridge$work_db <- 'tempstring'
    }
    
  })
  
  observe({
    req(input$agg_calculate)
    # add aggregate features to report params
    for_report$params$aggregate_by <- agg_output$output$aggregate_by
    for_report$params$aggregated_count <- agg_output$output$aggregated_count
    for_report$params$aggregated_tax <- agg_output$output$aggregated_tax
  })
  
  # filter features-------------------------------------------------------------
  # render sidebar controls - filter yes/no
  observeEvent(input$asv_select_prompt, {
    toggle("asv_filter_options_ui", condition = input$asv_select_prompt == 'some')
  })
  
  # pass in filter menu controls
  bridge$filter_input <- reactiveValues()
  observe({
    bridge$filter_input$asv_select_prompt <- input$asv_select_prompt
    bridge$filter_input$asv_filter_options <- input$asv_filter_options
    bridge$filter_input$submit_asv <- input$submit_asv
  })
  
  withBusyIndicatorServer('submit_asv', 'diff_abund_ui_1', {
    # submodule returns list of filtered met, asv and tax tables
    filter_output <- callModule(mod_filterfeat_server, "filterfeat_ui_1", bridge)
  })
  
  # add filtered data to bridge
  bridge$filtered <- reactiveValues()
  observe({
    bridge$filtered <- filter_output$filtered
  })
  
  # update report params
  observe({
    req(input$submit_asv)
    #feature filter
    for_report$params$asv_select_prompt <- input$asv_select_prompt
    for_report$params$asv_filter_options <- input$asv_filter_options
    for_report$params$cutoff_method <- filter_output$params$cutoff_method
    for_report$params$asv_cutoff <- filter_output$params$asv_cutoff
    for_report$params$prevalence <- filter_output$params$prevalence
    for_report$params$asv_cutoff_msg <- filter_output$params$asv_cutoff_msg
    for_report$params$asv_remove <- filter_output$params$asv_remove
    for_report$params$prev_agg_plot <- filter_output$params$prev_agg_plot
    for_report$params$prev_read_plot <- filter_output$params$prev_read_plot
    for_report$params$empty_sample <- filter_output$params$empty_sample
    for_report$params$empty_asv <- filter_output$params$empty_asv
    for_report$params$met2 <- filter_output$filtered$met
    for_report$params$tax2 <- filter_output$filtered$tax
  })
  
  # render ui for deseq tab-----------------------------------------------------
  output$variable_ui <- renderUI({
    selectInput(ns('variable'), "Observational Variable",
                choices = colnames(bridge$filtered$met))
  })
  
  output$group1_ui <- renderUI({
    
    validate(
      need(length(unique(bridge$filtered$met[,input$variable])) >= 2, 
           "Need at least two groups in selected variable")
    )
    choices <- unique(bridge$filtered$met[,input$variable])
    selectInput(ns('group1'), "Group 1", choices = choices)
  })
  
  output$group2_ui <- renderUI({
    validate(
      need(length(unique(bridge$filtered$met[,input$variable])) >= 2, 
           "Need at least two groups in selected variable")
    )
    choices <- unique(bridge$filtered$met[,input$variable])
    choices <- choices[which(choices != input$group1)]
    
    selectInput(ns('group2'), "Group 2", choices = choices)
  })
  
  # filter working data based on select variable and groups---------------------
  work_ls <- eventReactive(input$deseq_submit, {
    met <- bridge$filtered$met %>%
      filter(.data[[input$variable]] %in% c(input$group1, input$group2)) %>%
      tibble::column_to_rownames('sampleID')
    
    asv <- bridge$filtered$asv %>%
      tibble::column_to_rownames('featureID')
    asv <- asv[, rownames(met)]
    
    # remove any features with all 0s
    asv <- asv[which(colSums(asv) != 0),]
    
    tax <- bridge$filtered$tax %>%
      filter(featureID %in% rownames(asv))
    
    list(met = met, asv = asv, tax = tax)
  })
  
  # run deseq2------------------------------------------------------------------
  # put data into deseq matrix
  dds_data <- eventReactive(input$deseq_submit, {
    DESeq2::DESeqDataSetFromMatrix(
      countData = as.data.frame(t(work_ls()$asv)),
      colData = work_ls()$met,
      design = as.formula(paste0("~", input$variable))
    )
  })
  output$check <- renderPrint({
    print(summary(work_ls()))
    for(i in 1:length(work_ls())) {
      print(dim(work_lw()[[i]]))
    }
    
    print(summary(dds_data()))
  })
}
    
## To be copied in the UI
# mod_diff_abund_ui("diff_abund_ui_1")
    
## To be copied in the server
# callModule(mod_diff_abund_server, "diff_abund_ui_1")
 
