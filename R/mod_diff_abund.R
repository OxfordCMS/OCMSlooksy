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
          menuItem('Report', tabName = 'diffabund_report_tab'),
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
          # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
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
              column(
                width=12,
                fluidRow(
                  h1("Differential Abundance with DESeq2"),
                  tags$div(
                    "Compare groups using DESeq2. DESeq2 normalises and transforms data by [...]. DESeq2 assess differential abundance based on the assumption of a negative bionmial distribution..."
                  ), br()  
                ),
                fluidRow(
                  verbatimTextOutput(ns('dds_summary')),
                  br()
                ),
                fluidRow(
                  DT::dataTableOutput(ns('dds_result')) %>%
                    shinycssloaders::withSpinner()
                ),
                fluidRow(
                  hidden(div(
                    id=ns("deseq_sig_div"),
                    column(
                      width=3,
                      br(),
                        wellPanel(
                          h4("Signficance cut-offs"),
                          numericInput(ns('cutoff_pval'), "Adjusted p-value",
                                       value=0.05, min=0, max=1, step=0.05),
                          uiOutput(ns("cutoff_ui"))
                        )                  
                    ),
                    column(
                      width = 9,
                      tabsetPanel(
                        type = 'tabs',
                        tabPanel(
                          "Volcano",
                          column(
                            width=1, style = 'padding:0px;',
                            mod_download_ui(ns("download_volcano"))
                          ),
                          column(
                            width=8, style = 'padding:0px;',
                            plotlyOutput(ns('volcano'), 
                                         width = '100%', height = 'auto') %>%
                              shinycssloaders::withSpinner()
                          ) # end column w=8
                        ), # end volcano plot tabPanel
                        tabPanel(
                          "LogFoldChange",
                          column(
                            width = 1,style = 'padding:0px;',
                            mod_download_ui(ns("download_lfc"))
                          ),
                          column(
                            width = 8,
                            plotlyOutput(ns('lfc_plot'), 
                                         width = '100%', height = 'auto') %>%
                              shinycssloaders::withSpinner()
                          )
                        ) # end logfold change tabPanel
                      ) # end tabsetPanel  
                    ) # end column w=9
                  )) # end deseq_sig_div
                ), # end fluidRow
                fluidRow(
                  column(
                    width = 1, style = "padding:0px;",
                    mod_download_ui(ns("download_boxplot"))
                  ),
                  column(
                    width = 11,
                    plotlyOutput(ns('box_plot'))
                  )
                )
              ) # end column 12
            ), # end tabItem
            tabItem(
              tabName = 'diffabund_report_tab',
              mod_report_ui(ns("diffabund_report_ui"))
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
  observeEvent(input$deseq_submit, {
    show("deseq_sig_div")
  })
  output$variable_ui <- renderUI({
    selectInput(ns('variable'), "Observational Variable",
                choices = colnames(bridge$filtered$met))
  })
  
  output$group1_ui <- renderUI({
    req(input$variable)
    choices <- as.character(unique(as.data.frame(bridge$filtered$met)[,input$variable]))
    
    # check number of samples in each choice
    n_sample <- bridge$filtered$met %>%
      distinct(sampleID, .data[[input$variable]]) %>%
      group_by(.data[[input$variable]]) %>%
      summarise(n_sample = n())
    
    validate(
      need(length(choices) >= 2, 
           "Need at least two groups in selected variable"),
      need(min(n_sample$n_sample) > 1,
           "Not all groups have at least 2 samples")
    )
    
    selectInput(ns('group1'), "Group 1", choices = choices)
  })
  
  output$group2_ui <- renderUI({
    req(input$variable)
    choices <- as.character(unique(as.data.frame(bridge$filtered$met)[,input$variable]))
    
    # check number of samples in each choice
    n_sample <- bridge$filtered$met %>%
      distinct(sampleID, .data[[input$variable]]) %>%
      group_by(.data[[input$variable]]) %>%
      summarise(n_sample = n())
    
    validate(
      need(length(choices) >= 2, 
           "Need at least two groups in selected variable"),
      need(min(n_sample$n_sample) > 1,
           "Not all groups have at least 2 samples")
    )
    
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
    # remove any features with 0s in all samples
    asv <- asv[which(rowSums(asv) != 0),]
    
    tax <- bridge$filtered$tax %>%
      filter(featureID %in% rownames(asv))
    
    list(met = met, asv = asv, tax = tax)
  })
  
  # run deseq2------------------------------------------------------------------
  # put data into deseq object
  withBusyIndicatorServer('deseq_submit', 'mod_diff_abund', {
    dds_obj <- eventReactive(input$deseq_submit, {
      out <- DESeq2::DESeqDataSetFromMatrix(
        countData = as.data.frame(work_ls()$asv),
        colData = work_ls()$met,
        design = as.formula(paste0("~", input$variable))
      )
      DESeq2::DESeq(out)
    })  
  })
  
  output$dds_summary <- renderPrint({
    req(input$variable)
    cat('Model design:\n')
    print(sprintf("~%s", input$variable))
    cat('\nModel summary:\n')
    print(summary(dds_obj()))
    cat('\nEstimated effects of the Model"\n')
    print(DESeq2::resultsNames(dds_obj()))
  })
    
  # deseq results
  dds_result <- eventReactive(input$deseq_submit, {
    DESeq2::results(dds_obj()) %>%
      as.data.frame() %>%
      tibble::rownames_to_column('featureID')
  })
  
  output$dds_result <- DT::renderDataTable({
    DT::datatable(
      dds_result(), 
      filter='top',
      extensions = 'Buttons', 
      options = list(scrollX = TRUE, dom = 'Blfrtip', 
                     buttons = c('copy','csv'))
    ) %>%
      DT::formatRound(column = colnames(dds_result())[2:ncol(dds_result())],
                      digits = 3)
  })
  
  # extract deseq transformed data
  deseq_data <- reactive({
    req(input$deseq_submit)
    rld <- DESeq2::rlog(dds_obj(), blind=TRUE)
    SummarizedExperiment::assay(rld) %>%
      as.data.frame() %>%
      rownames_to_column('featureID')
  })
  
  # render ui-------------------------------------------------------------------
  output$cutoff_ui <- renderUI({
    max_val <- max(dds_result()$log2FoldChange)
    max_val <- floor(max_val)
    print(max_val)
    numericInput(ns('cutoff_fc'), "Log2 fold-change",
                 value=1, min=0, max=max_val, step=1)
  })

  # volcano plot----------------------------------------------------------------
  volcano_pdata <- reactive({
    req(input$cutoff_pval, input$cutoff_fc)
    dds_result() %>%
      mutate(is.sig = ifelse((padj <= input$cutoff_pval & 
                                log2FoldChange >= input$cutoff_fc) | 
                               (padj <= input$cutoff_pval & 
                                  log2FoldChange <= -input$cutoff_fc), 
                             TRUE, FALSE),
             colour = ifelse(is.sig == TRUE, TRUE, FALSE))
  })
  
  p_volcano <- reactive({

    p <- ggplot(volcano_pdata(), 
                aes(x=log2FoldChange, y=-log10(padj),
                    colour = colour, customdata = featureID)) +
      geom_hline(yintercept=-log10(input$cutoff_pval), linetype = 'dashed',
                 colour = 'grey80') +
      geom_vline(xintercept=c(-input$cutoff_fc, input$cutoff_fc),
                 linetype = 'dashed', colour = 'grey80') +
      geom_point(aes(text=sprintf("featureID: %s\npadj: %s", 
                                  featureID, round(padj, 2))),
                 alpha=0.6) +
      scale_colour_manual(values=c('grey60', 'darkred'), guide=FALSE) +
      theme_classic(12)
    
    p
  }) 
  output$volcano <- renderPlotly({

    ggplotly(p_volcano(), source='plotly_volcano') %>%
      layout(showlegend = FALSE,
             dragmode = 'select') 
  })
  
  # logfold change plot---------------------------------------------------------
  lfc_pdata <- reactive({
    volcano_pdata() %>%
      filter(is.sig == TRUE) %>%
      mutate(featureID = fct_reorder(featureID, desc(log2FoldChange)))
  })
  
  p_lfc <- reactive(
    ggplot(lfc_pdata(), 
           aes(x = log2FoldChange, y=featureID, colour = padj)) +
      geom_vline(xintercept = 0, linetype = 'dashed', colour='grey75') +
      geom_point() +
      theme_classic(12) +
      theme(axis.title.y = element_blank())
  )
  
  output$lfc_plot <- renderPlotly(
    ggplotly(p_lfc())
  )
  # selected data from volcano plot---------------------------------------------
  selected_feat <- reactiveVal()
  
  # store selected feature
  observeEvent(event_data("plotly_selected", source='plotly_volcano'), {
     curr_selected<- event_data("plotly_selected", source='plotly_volcano')$customdata
    updated_feat <- unique(c(selected_feat(), curr_selected))
    selected_feat(updated_feat)
  })
    
  # clear selection
  observeEvent(event_data("plotly_deselect", source="plotly_volcano"), {
    selected_feat(NULL)
  })
  
  box_pdata <- reactive({
    deseq_data() %>%
      filter(featureID %in% selected_feat()) %>%
      gather('sampleID','value',-featureID) %>%
      left_join(bridge$filtered$met, 'sampleID')
  })
  
  p_box <- reactive({
    req(input$deseq_submit, input$variable)
    validate(need(!is.null(selected_feat()), "Click and drag (with rectangle or lasso tool) to select points on volcano plot to show feature abundance (double-click to clear)"))
    ggplot(box_pdata(), aes(x = .data[[input$variable]], y = value)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(alpha=0.6) +
      facet_wrap(~featureID, ncol=4) +
      theme_classic() +
      ylab("Regularised log transformed count")
  })
  
  output$box_plot <- renderPlotly({
    
    ggplotly(p_box())
  })
  
  
  output$check <- renderPrint({

  })
  
  
  # download data---------------------------------------------------------------
  for_download <- reactiveValues()
  observe({
    req(input$deseq_submit)
    for_download$figure <- p_volcano()
    for_download$fig_data <- volcano_pdata()
  })
  
  callModule(mod_download_server, "download_volcano", 
             bridge = for_download, 'volcano')
  
  for_download2 <- reactiveValues()
  observe({
    req(input$deseq_submit)
    for_download2$figure <- p_lfc()
    for_download2$fig_data <- lfc_pdata()
  })
  
  callModule(mod_download_server, "download_lfc", 
             bridge = for_download2, 'lfc')
  
  for_download3 <- reactiveValues()
  observe({
    req(input$deseq_submit)
    for_download3$figure <- p_box()
    for_download3$fig_data <- box_pdata()
  })
  
  callModule(mod_download_server, "download_boxplot", 
             bridge = for_download3, 'boxplot')
  
  # update report params--------------------------------------------------------
  observe({
    req(input$deseq_submit)
    #feature filter
    for_report$params$deseq_variable <- input$variable
    for_report$params$deseq_group1 <- input$group1
    for_report$params$deseq_group2 <- input$group2
    for_report$params$dds_result <- dds_result()
    for_report$params$cutoff_pval <- input$cutoff_pval
    for_report$params$cutoff_fc <- input$cutoff_fc
    for_report$params$p_volcano <- p_volcano()
    for_report$params$p_lfc <- p_lfc()
    for_report$params$selected_feat <- selected_feat()
    for_report$params$p_box <- p_box()
  })

  
  # build report
  callModule(mod_report_server, "diffabund_report_ui", bridge = for_report,
             template = "diffabund_report",
             file_name = "diffabund_report")
}
    
## To be copied in the UI
# mod_diff_abund_ui("diff_abund_ui_1")
    
## To be copied in the server
# callModule(mod_diff_abund_server, "diff_abund_ui_1")
 