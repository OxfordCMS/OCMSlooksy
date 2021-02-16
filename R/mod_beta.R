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
          menuItem('Report', tabName = "beta_report_tab"),
          
          # aggregate menu controls---------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'tab_aggregate'",
            br(), hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')
            ),
            fixedPanel(
              radioButtons(ns('aggregate_by'), "Aggregate counts by:",
                           choices = c('featureID','Kingdom','Phylum','Class',
                                       'Order','Family','Genus','Species'),
                           selected = 'Genus'),
              
              actionButton(ns('agg_calculate'), "Aggregate")
            )
          ),
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
                      selected = "asv_by_count")
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
          
          # Dissimilarity menu controls-----------------------------------------
          conditionalPanel(
            condition = "input.menu === 'diss_tab'",
            br(), hr(),
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
              h1("Aggregate Features"),
              h4(textOutput(ns("agg_message"))),
              tags$div("The number of features collapsed is listed in the 'n_collapse' column"),
              DT::dataTableOutput(ns('agg_preview_tax')),
              DT::dataTableOutput(ns('agg_preview_count'))
            ), # end tabItem
            # filter tab body---------------------------------------------------
            tabItem(
              tabName = "filter_asv_beta",
              mod_filterfeat_ui(ns("filterfeat_ui_1"))
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
  

  # aggregate features----------------------------------------------------------
  output$agg_message <- renderText({
    req(input$aggregate_by)
    sprintf("Aggregating feature counts at the %s level", input$aggregate_by)
  })
  
  # perform aggregation with base aggregate
  aggregated_count <- eventReactive(input$agg_calculate, {
    req(input$aggregate_by)
    
    # set featureID in count_df to aggregation level
    count_df <- improxy$work_db$asv %>% arrange(featureID)
    
    # copy taxonomy table
    new_featID <- improxy$work_db$tax %>%
      arrange(featureID) %>%
      select(featureID, .data[[input$aggregate_by]]) %>%
      mutate(newID = .data[[input$aggregate_by]],
             newID = ifelse(is.na(newID), paste(input$aggregate_by, 'NA', sep="."), 
                            newID))
    
    # updating count featureID
    count_df$featureID <- new_featID$newID
    sampleID <- colnames(count_df)
    sampleID <- sampleID[sampleID != 'featureID']
    sampleID <- sprintf("`%s`", sampleID)
    
    # build formula
    yvar <- paste(as.character(sampleID), collapse=',')
    f <- sprintf("cbind(%s) ~ featureID", yvar)

    # perform aggregation with base::aggregate  
    aggregate(formula = formula(f), data = count_df, FUN = sum)
  })

  # update taxonomy table-------------------------------------------------------
  aggregated_tax <- eventReactive(input$agg_calculate, {
    req(input$aggregate_by)
    tax_level <- c('Kingdom','Phylum','Class','Order', 'Family','Genus',
                   'Species','featureID')
    
    if(input$aggregate_by != 'featureID') {
      
      # copy tax data over taxa up to aggregated level
      out <- as.data.frame(improxy$work_db$tax)
      
      # set all tax levels lower than aggregated level to NA
      ind <- which(tax_level == input$aggregate_by) + 1
      to_na <- tax_level[ind:length(tax_level)]
      
      
      out[, to_na] <- NA
      
      # set sequence column to NA
      out$sequence <- NA
      
      # update Taxon column
      ind <- which(tax_level == input$aggregate_by)
      
      Taxon <- stringr::str_split(out$Taxon, ";", simplify = TRUE)
      
      if(ind == 1) {
        Taxon <- Taxon[,ind]
      } else {
        Taxon <- apply(Taxon[,1:ind], 1, paste, collapse = ";")  
      }
      
      out$Taxon <- Taxon
      
      # remove redundant entries
      out <- unique(out)
      
      # record how many ASVs aggregated
      n_collapse <- improxy$work_db$tax %>%
        group_by(.data[[input$aggregate_by]]) %>%
        summarise(n_collapse = n())
      
      out <- merge(out, n_collapse, input$aggregate_by)
      
      # set new featureID
      out$featureID <- out[, input$aggregate_by]

    } else {
      out <- as.data.frame(improxy$work_db$tax)
      out$n_collapse <- 1
    }

    # set column order
    out <- out[,c('featureID', 'n_collapse', tax_level[-length(tax_level)], 'Taxon','sequence')]
    out
  })

  # show aggregated tables
  output$agg_preview_tax <- DT::renderDataTable({
    
    out <- aggregated_tax() %>%
      mutate_all(as.character)
    DT::datatable(out,
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv')))
  })

  output$agg_preview_count <- DT::renderDataTable({
    DT::datatable(aggregated_count(),
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  # store data in reactiveValues to pass onto submodules------------------------
  bridge <- reactiveValues()
  observe({
    bridge$work_db <- list(met = improxy$work_db$met,
                           asv = aggregated_count(),
                           tax = aggregated_tax() %>% select(-n_collapse))
    
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
    cross_mod <- callModule(mod_filterfeat_server, "filterfeat_ui_1", bridge)  
  })
  
  # add filtered data to bridge
  bridge$filtered <- reactiveValues()
  observe({
    bridge$filtered <- cross_mod$filtered 
  })
  
  # transform filtered data-----------------------------------------------------
  
  asv_transform <- eventReactive(input$submit_transform, {
    req(input$transform_method)
    
    withBusyIndicatorServer('submit_transform', 'beta_ui_1', {
      asv_df <- as.data.frame(bridge$filtered$asv)
      rownames(asv_df) <- asv_df$featureID
      asv_df <- asv_df[, colnames(asv_df) != 'featureID']
      
      if(input$transform_method == 'clr') {
        ## generate Monte Carlo samples from Dirichlet distribution
        ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
        ## use geometric mean abundance of features
        
        asv_clr <- ALDEx2::aldex.clr(asv_df, conds = bridge$filtered$met$sampleID,
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
        calc <- bridge$filtered$asv %>%
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
  bridge$asv_transform <- reactiveValues()
  observe({
    req(input$submit_transform, input$transform_method)
    bridge$asv_transform <- asv_transform()
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
  
  
  # output$check <- renderPrint({
  # 
  # })
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
    pcoa_result <- callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1", bridge = bridge)
      
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
    pca_result <- callModule(mod_ov_pca_server, "ov_pca_ui_1", bridge = bridge)
  })
  
  # initiate list to pass onto report submodule---------------------------------
  for_report <- reactiveValues()
  observe({
    req(input$agg_calculate)
    for_report$params <- list(
      # sample filter
      met1 = improxy$work_db$met,
      sample_select_prompt = improxy$work_db$sample_select_prompt,
      sample_select = improxy$work_db$sample_select,
      # aggregate features
      aggregate_by = input$aggregate_by,
      aggregated_count = aggregated_count(),
      aggregated_tax = aggregated_tax()
    )
  })
    
    observe({
      req(input$submit_asv)
      #feature filter
      for_report$params$asv_select_prompt <- input$asv_select_prompt
      for_report$params$asv_filter_options <- input$asv_filter_options
      for_report$params$cutoff_method <- cross_mod$params$cutoff_method
      for_report$params$asv_cutoff <- cross_mod$params$asv_cutoff
      for_report$params$prevalence <- cross_mod$params$prevalence
      for_report$params$asv_cutoff_msg <- cross_mod$params$asv_cutoff_msg
      for_report$params$asv_remove <- cross_mod$params$asv_remove
      for_report$params$prev_agg_plot <- cross_mod$params$prev_agg_plot
      for_report$params$prev_read_plot <- cross_mod$params$prev_read_plot
      for_report$params$empty_sample <- cross_mod$params$empty_sample
      for_report$params$empty_asv <- cross_mod$params$empty_asv
      for_report$params$met2 <- cross_mod$filtered$met
      for_report$params$tax2 <- cross_mod$filtered$tax
    })
    
    observe({
      req(input$submit_transform)
      # feature transformation
      for_report$params$transform_method <- input$transform_method
      for_report$params$asv_transform <- asv_transform()
    })
    
    observe({
      req(input$pcoa_calculate)
      # pcoa
      for_report$params$pcoa_dist <- input$pcoa_dist
      for_report$params$pcoa_summary <- pcoa_result$pcoa$pcoa_summary
      for_report$params$p_pcoa <- pcoa_result$pcoa$p_pcoa
  })

  observe({
    req(input$transform_method)
    if(input$transform_method != 'percent') {
      req(input$pca_calculate, input$pca_scale)
      for_report$params$pca_scale <- input$pca_scale
      for_report$params$pca_summary <- pca_result$pca$pca_summary
      for_report$params$p_pca <- pca_result$pca$p_pca
    } 
  })
  
  observe({
    req(input$transform_method)
    if(input$transform_method == 'percent') {
      req(input$diss_grp, input$diss_panel, input$diss_calculate)
      for_report$params$diss_grp <- input$diss_grp
      for_report$params$diss_panel <- input$diss_panel
      for_report$params$validation_msg <- dissimilarity$diss$validation_msg
      for_report$params$diss_msg <- dissimilarity$diss$diss_msg
      for_report$params$diss_result <- dissimilarity$diss$diss_result
      for_report$params$p_diss <- dissimilarity$diss$p_diss
      for_report$params$diss_stat <- dissimilarity$diss$diss_stat
    }
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
 
