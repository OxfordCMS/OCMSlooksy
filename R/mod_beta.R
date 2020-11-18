#' beta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
          menuItem('PCoA', tabName = 'pcoa_tab'),
          uiOutput(ns('pca_menu_ui')),
          
          # aggregate menu controls---------------------------------------------
          # filter menu controls------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'filter_asv_beta'",
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
              withBusyIndicatorUI(
                actionButton(ns('submit_asv'), "Filter features")))  
          ),
          # transform menu controls---------------------------------------------
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
              withBusyIndicatorUI(
                actionButton(ns('submit_transform'), "Transform Counts")    
              )
            )
          ),
          
          # PCoA menu controls--------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pcoa_tab'",
            br(), hr(),
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
            br(), hr(),
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
          wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          tabItems(
            # info tab body-----------------------------------------------------
            tabItem(
              tabName = 'info_tab_overview',
              h1("\u03B2-Diversity")
            ), # end tabItem
            # aggregate tab body------------------------------------------------
            tabItem(
              tabName = 'tab_aggregate',
              h1("Aggregate Features")
            ), # end tabItem
            # filter tab body---------------------------------------------------
            tabItem(
              tabName = "filter_asv_beta",
              mod_filterfeat_ui("filterfeat_ui_1")
            ), # end tabItem
            # transform tab body------------------------------------------------
            tabItem(
              tabName = 'transform_asv',
              column(width = 12,
                     h1('Transform Read Counts'),
                     tags$div("Surveying an ecosystem based on DNA sequence produces compositional data due to the constant sum constraint of sequencing platforms. Sequence read 'count' is not directly reflective of the absolute count of sequences in the sampled environment because the changes in the absolute abundance of a sequence can only be observed at the expense of other sequences. Lack of independance in sequence counts can result in spurious correlations, ultimately leading to false associations between variables. Further detail on compositional data analysis are discussed by [Greg Gloor and others, link]."),
                     br(), 
                     p("Applying log transformations corrects for the 'closure problem' [Aitcheson reference, link], such ecological and statistical tools are applicable to sequence data sets. The log transformations will be applied to the filtered data."),
                     br(),
                     p("Other forms of transformation include log10 of percent abundance (of the sample) and percent abundance (of the sample). Note that choosing percent abundance (without any log transformation) limits the analysis options available in subsequent analyses."),
                     br(),
                     p("Transformed data will be used throughout the analysis, where necessary. Instances of its usage is recorded in the final [report]."), br(),
                     DT::dataTableOutput(ns('preview_transform')) %>%
                       shinycssloaders::withSpinner())
            ), # end tabItem
            #PCoA body----------------------------------------------------------
            tabItem(
              tabName = "pcoa_tab",
              mod_ov_pcoa_ui(ns("ov_pcoa_ui_1"))
            ),
            # PCA body----------------------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
              mod_ov_pca_ui(ns("ov_pca_ui_1"))
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
  
  # aggregate features----------------------------------------------------------
  
  # store data in reactiveValues to pass onto submodules------------------------
  bridge <- reactiveValues()
  observe({
    bridge$work_db <- improxy$work_db
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
  
  filtered <- callModule(mod_filterfeat_server, "filterfeat_ui_1", bridge)
  
  # transform server------------------------------------------------------------
  asv_transform <- eventReactive(input$submit_transform, {
    req(input$transform_method)
    
    withBusyIndicatorServer('submit_transform', 'beta_ui_1', {
      asv_df <- as.data.frame(filtered$asv)
      rownames(asv_df) <- asv_df$featureID
      asv_df <- asv_df[, colnames(asv_df) != 'featureID']
      
      if(input$transform_method == 'clr') {
        ## generate Monte Carlo samples from Dirichlet distribution
        ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
        ## use geometric mean abundance of features
        
        asv_clr <- ALDEx2::aldex.clr(asv_df, conds = filtered$met$sampleID,
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
        calc <- filtered$asv %>%
          gather('sampleID','read_count', -featureID) %>%
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
  
  # add transformed data to bridge
  bridge$asv_transform <- reactiveVal(asv_transform())
  
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
    callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1", param = bridge)  
  })
  
  # render pca menu item--------------------------------------------------------
  output$pca_menu_ui <- renderUI({
    if(input$transform_method != 'percent') {
      sidebarMenu(menuItem('PCA', tabName = 'pca_tab'))
    }
  })
  
  # PCA server------------------------------------------------------------------
  output$pca_scale_ui <- renderUI({
    if(any(asv_transform() < 0)) {
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
  
  bridge$pca_input <- reactiveValues()
  observeEvent(input$pca_calculate, {
    withBusyIndicatorServer('pca_calculate', 'overview_ui_1', {
      if(improxy$work_db$transform_method != 'percent') {
        # pass pca reactive inputs to submodule
        bridge$pca_input$pca_calculate <- input$pca_calculate
        bridge$pca_input$pca_scale <- input$pca_scale
        
        callModule(mod_ov_pca_server, "ov_pca_ui_1", param = bridge)
      }  
    })
    
  })
  
 
}
    
## To be copied in the UI
# mod_beta_ui("beta_ui_1")
    
## To be copied in the server
# callModule(mod_beta_server, "beta_ui_1")
 
