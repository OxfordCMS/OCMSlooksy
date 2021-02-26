#' beta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs
#'  
mod_beta_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(
          id = 'menu', br(),
          menuItem('Task Info', tabName = 'info_tab_beta', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Aggregate Features', tabName = 'tab_aggregate'),
          menuItem('Filter Features', tabName = 'filter_asv_beta'),
          menuItem('Read Count Transformation', tabName = "transform_asv"),
          uiOutput(ns("diss_menu_ui")),
          menuItem('PCoA', tabName = 'pcoa_tab'),
          uiOutput(ns('pca_menu_ui')),
          menuItem('PERMANOVA', tabName = 'permanova_tab'),
          menuItem('Report', tabName = "beta_report_tab"),
          
          # aggregate menu controls---------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'tab_aggregate'",
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
            condition = "input.menu === 'filter_asv_beta'",
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
          # transform menu controls---------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'transform_asv'",
            hr(),
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
              withBusyIndicatorUI(
                actionButton(ns('submit_transform'), "Transform Counts")    
              )
            )
          ),
          
          # Dissimilarity menu controls-----------------------------------------
          conditionalPanel(
            condition = "input.menu === 'diss_tab'",
            hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b("Parameters")),
              uiOutput(ns('diss_grp_ui')),
              uiOutput(ns('diss_panel_ui')),
              withBusyIndicatorUI(
                actionButton(ns('diss_calculate'), 'Calculate')  
              )
            )
          ),
          # PCoA menu controls--------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pcoa_tab'",
            hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b("PCoA Parameters")),
              uiOutput(ns('pcoa_dist_ui')),
              withBusyIndicatorUI(
                actionButton(ns('pcoa_calculate'), 'Calculate')  
              )
            )
          ),
          # PCA controls--------------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pca_tab'",
            hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b('PCA Parameters')),
              uiOutput(ns('pca_scale_ui')),
              withBusyIndicatorUI(
                actionButton(ns('pca_calculate'), "Calculate")  
              )
            )
          )
      )),
      # dashboard body----------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          tabItems(
            # info tab body-----------------------------------------------------
            tabItem(
              tabName = 'info_tab_beta',
              h1("\u03B2-Diversity")
            ), # end tabItem
            # aggregate tab body------------------------------------------------
            tabItem(
              tabName = 'tab_aggregate',
              mod_aggregate_ui(ns("aggregate_ui_1"))
            ), # end tabItem
            # filter tab body---------------------------------------------------
            tabItem(
              tabName = "filter_asv_beta",
              mod_filterfeat_ui(ns("filterfeat_ui_1"))
            ), # end tabItem
            # transform tab body------------------------------------------------
            tabItem(
              tabName = 'transform_asv',
              mod_transform_ui(ns("transform_ui_1"))
            ), # end tabItem
            # Dissimilarity body------------------------------------------------
            tabItem(
              tabName = "diss_tab",
              mod_ov_diss_ui(ns("ov_diss_ui_1"))
            ), # end tabItem
            # PCoA body----------------------------------------------------------
            tabItem(
              tabName = "pcoa_tab",
              mod_ov_pcoa_ui(ns("ov_pcoa_ui_1"))
            ),
            # PCA body----------------------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
              mod_ov_pca_ui(ns("ov_pca_ui_1"))
            ),
            tabItem(
              tabName = 'permanova_tab',
              mod_ov_permanova_ui(ns("ov_permanova_ui_1"))
            ),
            tabItem(
              tabName = 'beta_report_tab',
              mod_report_ui(ns("beta_report_ui"))
            )
          ) # end tabItems
        ) # end box
      ) # end dashbaoad body
    ) # end dashboard Page
  )
}
    
#' beta Server Function
#'
#' @noRd 
mod_beta_server <- function(input, output, session, improxy){
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
  withBusyIndicatorServer('agg_calculate', 'beta_ui_1', {
    agg_output <- callModule(mod_aggregate_server, "aggregate_ui_1", bridge)
    
  })

  output$check <- renderPrint({

  })

  # store data in reactiveValues to pass onto submodules
  observe({
    req(input$agg_calculate)
    if(!is.null(agg_output$agg)) {
      tax_entry <- dplyr::select(agg_output$output$aggregated_tax, -n_collapse)
      
      # add aggregate features to bridge to be passed to submodules
      bridge$work_db <- list(
        met = improxy$work_db$met,
        asv = agg_output$output$aggregated_count,
        tax = tax_entry
      )
      # add aggregate features to report params
      for_report$params$aggregate_by <- agg_output$output$aggregate_by
      for_report$params$aggregated_count <- agg_output$output$aggregated_count
      for_report$params$aggregated_tax <- agg_output$output$aggregated_tax
    } else { # agg_result starts out as NULL initially. else stops that from causing app to crash
      bridge$work_db <- list(
        met = improxy$work_db$met,
        asv = improxy$work_db$asv,
        tax = improxy$work_db$tax
      )
      
      # add aggregate features to report params
      for_report$params$aggregate_by <- NA
      for_report$params$aggregated_count <- improxy$work_db$asv
      for_report$params$aggregated_tax <- improxy$work_db$tax
    }

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

  withBusyIndicatorServer('submit_asv', 'beta_ui_1', {
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

  # transform filtered data-----------------------------------------------------

  observe({
    bridge$transform_input <- list(
      transform_method = input$transform_method,
      submit_transform = input$submit_transform
    )
  })
  withBusyIndicatorServer("submit_transform", "beta_ui_1", {
    transform_output <- callModule(mod_transform_server, "transform_ui_1", bridge)
  })

  # add transformed data to reactive values
  observe({
    req(input$submit_transform, input$transform_method)
    
    # add to bridge
    bridge$asv_transform <- transform_output$output$asv_transform
    
    # add to report params
    for_report$params$transform_method <- input$transform_method
    for_report$params$asv_transform <- transform_output$output$asv_transform
  })

  # Dissimilarity server--------------------------------------------------------
  # render dissimilarity menu item
  output$diss_menu_ui <- renderUI({
    if(input$transform_method == 'percent') {
      sidebarMenu(menuItem('Sample Dissimilarity', tabName = 'diss_tab'))
    }
  })

  # render controls - dissimilarity
  output$diss_grp_ui <- renderUI({
    selectInput(ns('diss_grp'), "Compare Sample Groups",
                 choices = colnames(improxy$work_db$met), selected = 'sampleID')
  })

  output$diss_panel_ui <- renderUI({
    selectInput(ns('diss_panel'), "panel by",
                choices = c('none', colnames(improxy$work_db$met)),
                selected = 'none')
  })

  bridge$diss_input <- reactiveValues()
  observe({
    bridge$diss_input$diss_grp <- input$diss_grp
    bridge$diss_input$diss_panel <- input$diss_panel
    bridge$diss_input$diss_calculate <- input$diss_calculate
  })

  withBusyIndicatorServer('diss_calculate','beta_ui_1', {
    dissimilarity <- callModule(mod_ov_diss_server, "ov_diss_ui_1", bridge = bridge)
  })

  # dissimilarity
  observe({
    req(input$transform_method)
    if(input$transform_method == 'percent') {
      req(input$diss_grp, input$diss_panel, input$diss_calculate)
      for_report$params$diss_grp <- input$diss_grp
      for_report$params$diss_panel <- input$diss_panel
      for_report$params$validation_msg <- dissimilarity$output$validation_msg
      for_report$params$diss_msg <- dissimilarity$output$diss_msg
      for_report$params$diss_result <- dissimilarity$output$diss_result
      for_report$params$p_diss <- dissimilarity$output$p_diss
      for_report$params$diss_stat <- dissimilarity$output$diss_stat
    }
  })
  
  # PCoA server-----------------------------------------------------------------
  # render pcoa distance ui
  output$pcoa_dist_ui <- renderUI({
    if(input$transform_method == 'percent') choices <- 'bray'
    else choices <- c("manhattan", "euclidean", "canberra")

    selectInput(ns('pcoa_dist'), "Distance method",
                choices = choices,
                selected = choices[1])
  })

  bridge$pcoa_input <- reactiveValues()
  observe({
    bridge$pcoa_input$pcoa_dist <- input$pcoa_dist
    bridge$pcoa_input$pcoa_calculate <- input$pcoa_calculate
  })

  withBusyIndicatorServer('pcoa_calculate', 'beta_ui_1', {
    pcoa_output <- callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1", bridge = bridge)

  })

  observe({
    req(input$pcoa_calculate)
    # pcoa
    for_report$params$pcoa_dist <- input$pcoa_dist
    for_report$params$pcoa_summary <- pcoa_output$pcoa$pcoa_summary
    for_report$params$p_pcoa <- pcoa_output$output$p_pcoa
  })
  
  # render pca menu item--------------------------------------------------------
  output$pca_menu_ui <- renderUI({
    if(input$transform_method != 'percent') {
      sidebarMenu(menuItem('PCA', tabName = 'pca_tab'))
    }
  })

  # PCA server------------------------------------------------------------------
  output$pca_scale_ui <- renderUI({
    if(any(bridge$asv_transform < 0)) {
      choices <- c("none" = "none",
                   "unit-variance scaling" = 'UV',
                   "vast scaling" = 'vast')
    }
    else {
      choices <- c("none" = "none",
                   "unit-variance scaling" = 'UV',
                   "pareto scaling" = 'pareto',
                   "vast scaling" = 'vast')
    }

    radioButtons(ns('pca_scale'), "Scale",
                 choices = choices,
                 selected = 'UV')
  })

  # pass pca reactive inputs to submodule
  bridge$pca_input <- reactiveValues()
  observe({
    req(input$transform_method)
    if(input$transform_method != 'percent') {
      req(input$transform_method, input$pca_scale, input$pca_calculate)
      bridge$pca_input$pca_calculate <- input$pca_calculate
      bridge$pca_input$pca_scale <- input$pca_scale
    }
  })
  
  withBusyIndicatorServer('pca_calculate', 'beta_ui_1', {
    pca_output <- callModule(mod_ov_pca_server, "ov_pca_ui_1", bridge = bridge)
  })
  
  # pca
  observe({
    req(input$transform_method)
    if(input$transform_method != 'percent') {
      req(input$pca_calculate, input$pca_scale)
      for_report$params$pca_scale <- input$pca_scale
      for_report$params$pca_summary <- pca_output$output$pca_summary
      for_report$params$p_pca <- pca_output$output$p_pca
    }
  })
  
  # PERMANOVA ------------------------------------------------------------------

  permanova_output <- callModule(mod_ov_permanova_server, "ov_permanova_ui_1",
                                 bridge = bridge)

  # permanova
  observeEvent(permanova_output$output$permanova_calculate, {
    for_report$params$permanova_stratify <-
      permanova_output$output$permanova_stratify
    for_report$params$permanova_dist <-
      permanova_output$output$permanova_dist
    for_report$params$permanova_terms <-
      permanova_output$output$permanova_terms
    for_report$params$permanova_formula <-
      permanova_output$output$permanova_formula
    for_report$params$permanova_summary <-
      permanova_output$output$permanova_summary
  })

  # build report
  callModule(mod_report_server, "beta_report_ui", bridge = for_report,
             template = "beta_report",
             file_name = "beta_report")
}
    
## To be copied in the UI
# mod_beta_ui("beta_ui_1")
    
## To be copied in the server
# callModule(mod_beta_server, "beta_ui_1")
 
