#' diff_abund UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DESeq2
#' @import SummarizedExperiment
mod_diff_abund_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      inlineCSS("
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"),
      # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
      navlistPanel(
        '',
        id = 'diffabund_menu',
        well=FALSE,
        widths=c(3,9),

        # info tab body-----------------------------------------------------
        tabPanel(
          'Module Info',
          value = 'info_tab_diff',
          icon = icon('info-circle'),
          fluidRow(
            br(),br(),
            column(
              width = 12,
              div(
                h1("Differential Abundance"),
                p("Compare groups to see what taxa are differentially abundant between the groups. This module uses DESeq2 and provides a couple different methods of visualising the results. "),
                p("Module overview:"),
                tags$ul(
                  tags$li(tags$b("Aggregate Features:"), "Select the taxonomic level at which you want to examine the microbiome profiles"),
                  tags$li(tags$b("Filter Features:"), "Filter aggregated features based on feature abundance and prevalence"),
                  tags$li(tags$b("Differential Abundance:"), "Compare groups within an experiment variable and measure the log-fold change and statistical significance of taxa. This module employs ", code("DESeq2"), " to assess differential abundance.")
                )
              )
            )
          )
        ), # end tabPanel
        # aggregate tab body------------------------------------------------
        tabPanel(
          'Aggregate Features',
          value = 'agg_diff_tab',
          fluidRow(
            br(),br(),
            column(
              width = 12,
              mod_aggregate_ui(ns("aggregate_ui_1"))
            )
          )
        ), # end tabPanel
        # filter tab body---------------------------------------------------
        tabPanel(
          'Filter Features',
          value = "filter_diff_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              div(id=ns('filtfeat_mod_div'),
                mod_filterfeat_ui(ns("filterfeat_ui_1"))
              )
            )
          )
        ), # end tabPanel
        tabPanel(
          'Differential Abundance',
          value = "deseq_tab",
          fluidRow(
            br(),br(),
            column(
              width=12,
              fluidRow(
                h1("Differential Abundance with DESeq2"),
                column(
                  width = 3,
                  wellPanel(
                    uiOutput(ns('variable_ui')),
                    uiOutput(ns('group1_ui')),
                    uiOutput(ns('group2_ui')),
                    uiOutput(ns('covariate_ui')),
                    div(
                      id = ns('include_intxn_div'),
                      checkboxInput(ns('include_intxn'),
                                    'Include interaction',
                                    value=FALSE)
                    ),
                    withBusyIndicatorUI(
                      actionButton(ns('deseq_submit'), "Compare Groups")
                    )
                  )
                ),
                column(
                  width = 9,
                  # normalisation and transformation definitions from biostars discussion https://www.biostars.org/p/275010/
                  p("Compare groups using DESeq2. DESeq2 normalises counts by dividing read count by the geometric mean of that taxon across all samples. Normalised counts are then transformed by ", code("log2"), ". DESeq2 assess differential abundance using negative binomial generalized linear models and tests model fit with the Wald test. Details on ", code("DESeq2"), " can be found in the ", a("package vignette", href="https://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html"), ".")
                )
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
                    width=4,
                    br(),
                    wellPanel(
                      uiOutput(ns('contrast_ui')),
                      h4("Signficance cut-offs"),
                      numericInput(ns('cutoff_pval'), "Adjusted p-value",
                                   value=0.05, min=0, max=1, step=0.05),
                      uiOutput(ns("cutoff_ui"))
                    )
                  ),
                  column(
                    width = 8,
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
                  plotlyOutput(ns('box_plot'))  %>%
                    shinycssloaders::withSpinner()
                )
              )
            ) # end column 12
          ) # end fluidRow
        ), # end tabPanel
        tabPanel(
          'Report',
          value = 'diffabund_report_tab',
          fluidRow(
            br(),br(),
            column(
              width = 12,
              mod_report_ui(ns("diffabund_report_ui"))
            )
          )
        )
      ) # end navlistPanel
    ) # end fluidPage
  ) # end taglist
}

#' diff_abund Server Function
#'
#' @noRd
mod_diff_abund_server <- function(input, output, session, improxy){
  ns <- session$ns

  # initiate a reactive to track/permit progress through analysis module--------
  progress <- reactiveValues(complete_agg = 0, complete_featfilt = 0)

  observeEvent(input[['aggregate_ui_1-agg_calculate']], {
    progress$complete_agg <- 1
    progress$complete_featfilt <- 0
  })

  observeEvent(input[['aggregate_ui_1-agg_clear']], {
    progress$complete_agg <- 0
    progress$complete_featfilt <- 0
  })

  observeEvent(input[['filterfeat_ui_1-submit_asv']], {
    progress$complete_featfilt <- 1
  })

  observeEvent(input[['filterfeat_ui_1-clear_asv']], {
    progress$complete_featfilt <- 0
  })

  observe({
    if(progress$complete_featfilt == 0) {
      reset('filtfeat_mod_div')
      hide('filterfeat_ui_1-prev_filter_div')
      hide('filterfeat_ui_1-preview_asv_div')
    }
  })

  # enable tabs sequentially----------------------------------------------------
  observe({
    toggleState(selector = "#diffabund_menu li a[data-value=filter_diff_tab]",
                condition = progress$complete_agg == 1)
  })

  observe({
    toggleState(selector = "#diffabund_menu li a[data-value=deseq_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    toggleState(selector = "#diffabund_menu li a[data-value=diffabund_report_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    req(input$covariate)
    toggle('include_intxn_div', condition = input$covariate != 'none')
  })
  # initiate value to pass into submodules--------------------------------------
  bridge <- reactiveValues()
  observe({
    bridge$qualfilt_db <- improxy$work_db
  })
  # initiate list to pass onto report submodule
  for_report <- reactiveValues()

  # store values to pass to report
  observe({
    for_report$params <- list(
      # sample filter
      met1 = improxy$work_db$met,
      sample_select_prompt = improxy$work_db$sample_select_prompt,
      sample_select = improxy$work_db$sample_select
    )
  })
  # aggregate features----------------------------------------------------------
  agg_output <- callModule(mod_aggregate_server, "aggregate_ui_1", bridge)

  # store data in reactiveValues to pass onto submodules
  observe({
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
    # add aggregate features to report params
    for_report$params$aggregate_by <- input[['aggregate_ui_1-aggregate_by']]
    for_report$params$aggregated_count <- agg_output$output$aggregated_count
    for_report$params$aggregated_tax <- agg_output$output$aggregated_tax
  })

  # filter features-------------------------------------------------------------

  # submodule returns list of filtered met, asv and tax tables

  filter_output <- callModule(mod_filterfeat_server, "filterfeat_ui_1", bridge)

  # add filtered data to bridge
  observe({
    bridge$filtered <- filter_output$filtered
  })

  # update report params
  observe({
    #feature filter
    for_report$params$asv_select_prompt <-
      input[['filterfeat_ui_1-asv_select_prompt']]
    for_report$params$asv_filter_options <-
      input[['filterfeat_ui_1-asv_filter_options']]
    for_report$params$cutoff_method <- input[['filterfeat_ui_1-cutoff_method']]
    for_report$params$asv_cutoff <- input[['filterfeat_ui_1-asv_cutoff']]
    for_report$params$prevalence <- input[['filterfeat_ui_1-prevalence']]
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
  observe({
    req(input$variable)
    toggleState('deseq_submit',
                condition = length(choices()) >= 2 & min(n_sample()$n_sample) > 1)
  })
  observeEvent(input$deseq_submit, {
    show("deseq_sig_div")
  })
  output$variable_ui <- renderUI({
    selectInput(ns('variable'), "Observational Variable",
                choices = colnames(bridge$filtered$met))
  })

  output$covariate_ui <- renderUI({
    req(input$variable)
    covar_choices <- colnames(bridge$filtered$met)[which(colnames(bridge$filtered$met) != input$variable)]
    covar_choices <- c('none', covar_choices)
    selectInput(ns('covariate'), 'Covariate (optional)',
                choices = covar_choices, selected = 'none')
  })

  choices <- reactive({
    req(input$variable)
    as.character(unique(as.data.frame(bridge$filtered$met)[,input$variable]))
  })

  # check number of samples in each choice
  n_sample <- reactive({
    req(input$variable)
    bridge$filtered$met %>%
      distinct(sampleID, .data[[input$variable]]) %>%
      group_by(.data[[input$variable]]) %>%
      summarise(n_sample = n())
  })

  output$group1_ui <- renderUI({
    validate(
      need(length(choices()) >= 2,
           "Need at least two groups in selected variable"),
      need(min(n_sample()$n_sample) > 1,
           "Not all groups have at least 2 samples")
    )

    selectInput(ns('group1'), "Group 1", choices = choices())
  })

  output$group2_ui <- renderUI({
    req(input$variable)

    validate(
      need(length(choices()) >= 2,
           "Need at least two groups in selected variable"),
      need(min(n_sample()$n_sample) > 1,
           "Not all groups have at least 2 samples")
    )

    g2_choices <- choices()[which(choices() != input$group1)]

    selectInput(ns('group2'), "Group 2", choices = g2_choices)
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
  deseq_design <- reactiveVal()

  observe({
    req(input$variable, input$covariate)
    if(input$covariate == 'none') {
      entry <- as.formula(paste0("~", input$variable))
    } else {
      if(input$include_intxn == TRUE) {
        entry <- as.formula(paste0("~", input$covariate, "+", input$variable,
                                   "+", input$covariate, ":", input$variable))
      } else {
        entry <- as.formula(paste0("~", input$covariate, "+", input$variable))
      }
    }
    deseq_design(entry)
  })
  # put data into deseq object
  withBusyIndicatorServer('deseq_submit', 'mod_diff_abund', {
    dds_obj <- eventReactive(input$deseq_submit, {
      req(deseq_design())
      out <- DESeq2::DESeqDataSetFromMatrix(
        countData = as.data.frame(work_ls()$asv),
        colData = work_ls()$met,
        design = deseq_design()
      )
      DESeq2::DESeq(out)
    })
  })

  # deseq contrasts in results
  contrast_avail <- reactive({
    out <- DESeq2::resultsNames(dds_obj())
    out <- out[2:length(out)]
    rev(out)
  })

  output$dds_summary <- renderPrint({
    req(input$variable)
    cat('Model design:\n')
    print(paste(as.character(deseq_design()), collapse=''))

    ## Nick Ilott: I have removed this summary
    ## section until I find out how to reproduce
    ## the output without using summary()
    #cat('\nModel summary:\n')
    #print(summary(dds_obj()))
    cat('\nEstimated effects of the Model"\n')
    print(contrast_avail())
  })


  # deseq results
  dds_result <- eventReactive(input$deseq_submit, {

    out <- c()
    for(i in 1:length(contrast_avail())) {
      entry <- DESeq2::results(dds_obj(), name=contrast_avail()[i]) %>%
        as.data.frame() %>%
        tibble::rownames_to_column('featureID') %>%
        mutate(contrast=contrast_avail()[i]) %>%
        relocate(contrast, .after='featureID')

      out <- rbind(out, entry)
    }

    out
  })

  output$dds_result <- DT::renderDataTable(server = FALSE, {
    DT::datatable(
      dds_result() %>%
        mutate(contrast = factor(contrast, levels = contrast_avail())) %>%
        arrange(contrast, desc(log2FoldChange)),
      filter='top',
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(scrollX = TRUE, dom = 'Blfrtip',
                     buttons = c('copy','csv'))
    ) %>%
      DT::formatRound(column = colnames(dds_result())[3:ncol(dds_result())],
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
  # choose contrast
  output$contrast_ui <- renderUI({
    selectInput(ns('contrast'), "Contrast",
                choices=contrast_avail())
  })

  output$cutoff_ui <- renderUI({
    max_val <- max(dds_result()$log2FoldChange)
    max_val <- floor(max_val)
    numericInput(ns('cutoff_fc'), "Log2 fold-change",
                 value=1, min=0, max=max_val, step=1)
  })

  # volcano plot----------------------------------------------------------------
  volcano_pdata <- reactive({
    req(input$cutoff_pval, input$cutoff_fc)
    dds_result() %>%
      filter(contrast == input$contrast) %>%
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
      scale_colour_manual(values=c('grey60', 'darkred'), guide='none') +
      theme_classic(12)

    p
  })
  output$volcano <- renderPlotly({

    ggplotly(p_volcano(), source='plotly_volcano') %>%
      layout(showlegend = FALSE,
             dragmode = 'select')
  })

  # logfold change plot---------------------------------------------------------
  output$check <- renderPrint({

  })

  lfc_pdata <- reactive({
    volcano_pdata() %>%
      filter(is.sig == TRUE) %>%
      mutate(featureID = fct_reorder(featureID, desc(log2FoldChange)))
  })

  p_lfc <- reactive({
    validate(need(nrow(lfc_pdata()) > 0,
                  "No significant features at current cutoff thresholds"))
    ggplot(lfc_pdata(),
           aes(x = log2FoldChange, y=featureID, colour = padj)) +
      geom_vline(xintercept = 0, linetype = 'dashed', colour='grey75') +
      geom_point() +
      theme_classic(12) +
      theme(axis.title.y = element_blank())
  })

  output$lfc_plot <- renderPlotly(
    ggplotly(p_lfc())
  )
  # selected data from volcano plot---------------------------------------------
  selected_feat <- reactiveVal()

  # store selected feature
  observeEvent(event_data("plotly_selected", source="plotly_volcano"), {
     curr_selected<- event_data("plotly_selected", source='plotly_volcano')$customdata
    updated_feat <- unique(c(selected_feat(), curr_selected))
    selected_feat(updated_feat)
  })

  # clear selection
  observeEvent(event_data("plotly_deselect", source="plotly_volcano"), {
    selected_feat(NULL)
  })

  box_pdata <- reactive({
    out <- deseq_data() %>%
      filter(featureID %in% selected_feat()) %>%
      gather('sampleID','value',-featureID) %>%
      left_join(bridge$filtered$met, 'sampleID')

    # create dummy x axis variable for plotting
    cond <- c(grepl(input$variable, input$contrast),
              grepl(input$covariate, input$contrast))

    if(identical(cond, c(TRUE, FALSE))) {
      out <- out %>%
        mutate(xvar = .data[[input$variable]])
    } else if(identical(cond, c(FALSE, TRUE))) {
      out <- out %>%
        mutate(xvar = .data[[input$covariate]])
    } else if(identical(cond, c(TRUE, TRUE))) {
      # need to create interaction term
      out <- out %>%
        mutate(xvar = paste(.data[[input$covariate]],
                            .data[[input$variable]], sep="_"))
    }


  })

  p_box <- reactive({
    req(input$deseq_submit, input$variable)
    validate(need(!is.null(selected_feat()), "Click and drag (with rectangle or lasso tool) to select points on volcano plot to show feature abundance (double-click to clear)"))


    ggplot(box_pdata(), aes(x = xvar, y = value)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(alpha=0.6) +
      facet_wrap(~featureID, ncol=4) +
      theme_classic() +
      xlab(input$contrast) +
      ylab("Regularised log transformed count")
  })

  output$box_plot <- renderPlotly({
    out <- plotly_build(p_box())
    out$x$data <- lapply(out$x$data, FUN = function(x) {
      x$marker = list(color = "rgba(0,0,0,1)",
                      outliercolor = "rgba(0,0,0,0)",
                      line = list(outliercolor='rgba(0,0,0,0)'))
      return(x)
    })
    out
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

