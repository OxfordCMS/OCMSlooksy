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
        id = 'beta_menu',
        well=FALSE,
        widths=c(3,9),
      # info tab body-----------------------------------------------------
      tabPanel(
        'Module Info',
        value = 'info_tab_beta',
        icon = icon('info-circle'),
        fluidRow(
          column(
            width = 12,
            br(),br(),
            h1("\u03B2-Diversity"),
            div(
              p("This analysis module assesses the microbiome of the entire dataset. \u03B2-Diversity describes the microbial ecosystem across samples (as opposed to \u03B1-diversity which is calculated within one sample at a time). This analysis tasks provides several data visualization tools that describes how similar (or dissimilar) samples are to one another based on their microbiome"),
              p("Task overview:"),
              tags$ul(
                tags$li(tags$b("Aggregate Features:"), "Select the taxonomic level at which you want to examine the microbiome profiles"),
                tags$li(tags$b("Filter Features:"), "Filter aggregated features based on feature abundance and prevalence"),
                tags$li(tags$b("Read Count Transformation:"), "Normalises and Transforms read count. Note some subsequent analyses and parameters are only available under certain normalisation/transformation methods. Details below."),
                tags$li(tags$b('Sample Dissimilarity:'), "Calculates pair-wise sample dissimilarity within sample groups and performs statistical tests where applicable. Only available with percent abundance normalisation"),
                tags$li(tags$b("PCoA:"), "Principal Coordinate Analysis, a multivariate analysis that uses distance metrics to cluster samples based on their sample similarity"),
                tags$li(tags$b("PCA:"), "Principal Component Analysis, a multivariate analysis that clusters samples based on their variance. Unavailable with percent abundance normalisation."),
                tags$li(tags$b("PERMANOVA:"), "Permutational Multivariate Analysis of Variance assesses sample dissimilarity based on distance measures and evaluates the contribution of experiment (metadata) variables to the overall variance observed. It does this by partitioning variance into variables within an experimental design.")
              ) # end tags$ul
            ) # end div
          ) # end column
        ) # end fluidRow
      ), # end tabPanel
      # aggregate tab body------------------------------------------------
      tabPanel(
        'Aggregate Features',
        value = 'agg_beta_tab',
        fluidRow(
          br(),br(),
          column(
            width = 12,
            mod_aggregate_ui(ns("aggregate_ui_1"))
          )
        )
      ), # end tabItem
      # filter tab body---------------------------------------------------
      tabPanel(
        'Filter Features',
        value = "filter_beta_tab",
        fluidRow(
          br(),br(),
          column(
            width = 12,
            div(id=ns('filtfeat_mod_div'),
              mod_filterfeat_ui(ns("filterfeat_ui_1"))
            )
          )
        )
      ), # end tabItem
      # transform tab body------------------------------------------------
      tabPanel(
        'Read Count Transformation',
        value = 'transform_tab',
        fluidRow(
          br(),br(),
          column(
            width=12,
            div(id=ns('transform_mod_div'),
              mod_transform_ui(ns("transform_ui_1"))
            )
          )
        )
      ), # end tabPanel
      # Dissimilarity body------------------------------------------------
      tabPanel(
        'Sample Dissimilarity',
        value = "diss_tab",
        fluidRow(
          br(),br(),
          column(
            width = 12,
            div(id=ns('diss_mod_div'),
              mod_ov_diss_ui(ns("ov_diss_ui_1"))
            )
          )
        )
      ), # end tabPanel
      # PCoA body----------------------------------------------------------
      tabPanel(
        'PCoA',
        value = "pcoa_tab",
        fluidRow(
          br(),br(),
          column(
            width=12,
            div(id=ns('pcoa_mod_div'),
              mod_ov_pcoa_ui(ns("ov_pcoa_ui_1"))
            )
          )
        )
      ),
      # PCA body----------------------------------------------------------
      tabPanel(
        'PCA',
        value = 'pca_tab',
        fluidRow(
          br(),br(),
          column(
            width = 12,
            div(id=ns('pca_mod_div'),
              mod_ov_pca_ui(ns("ov_pca_ui_1"))
            )
          )
        )
      ),
      tabPanel(
        'PERMANOVA',
        value = 'permanova_tab',
        fluidRow(
          br(),br(),
          column(
            width = 12,
            div(id=ns('permanova_mod_div'),
              mod_ov_permanova_ui(ns("ov_permanova_ui_1"))
            )
          )
        )
      ),
      tabPanel(
        'Report',
        value = 'beta_report_tab',
        fluidRow(
          br(),br(),
          column(
            width=12,
            mod_report_ui(ns("beta_report_ui"))
          )
        )
      )
      ) # navlistPanel
    ) # end fluidPage
  ) # end taglist
}

#' beta Server Function
#'
#' @noRd
mod_beta_server <- function(input, output, session, improxy){
  ns <- session$ns

  output$check <- renderPrint({
  })
  # initiate a reactive to track/permit progress through analysis module--------
  progress <- reactiveValues(complete_agg = 0, complete_featfilt = 0,
                             complete_transform = 0)

  observeEvent(input[['aggregate_ui_1-agg_calculate']], {
    progress$complete_agg <- 1
    if(progress$complete_featfilt == 1) {
      progress$complete_featfilt <- 0
    }
    if(progress$complete_transform == 1) {
      progress$complete_transform <- 0
    }

  })

  observeEvent(input[['aggregate_ui_1-agg_clear']], {
    progress$complete_agg <- 0
    if(progress$complete_featfilt == 1) {
      progress$complete_featfilt <- 0
    }
    if(progress$complete_transform == 1) {
      progress$complete_transform <- 0
    }
  })

  observeEvent(input[['filterfeat_ui_1-submit_asv']], {
    progress$complete_featfilt <- 1
    if(progress$complete_transform == 1) {
      progress$complete_transform <- 0
    }
  })

  observeEvent(input[['filterfeat_ui_1-clear_asv']], {
    progress$complete_featfilt <- 0
    if(progress$complete_transform == 1) {
      progress$complete_transform <- 0
    }
  })

  observe({
    if(progress$complete_featfilt == 0) {
      reset('filtfeat_mod_div')
      hide('filterfeat_ui_1-prev_filter_div')
      hide('filterfeat_ui_1-preview_asv_div')
    }
  })

  observe({
    if(progress$complete_transform == 0) {
      reset('transform_mod_div')
      reset('diss_mod_div')
      hide('ov_diss_ui_1-diss_result_div')
      reset('pcoa_mod_div')
      hide('ov_pcoa_ui_1-pcoa_body_div')
      reset('pca_mod_div')
      hide('ov_pca_ui_1-pca_summary_div')
      hide('ov_pca_ui_1-pca_body_div')
      reset('permanova_mod_div')
      hide('ov_permanova_ui_1-permanova_result_div')
    }
  })

  observeEvent(input[['transform_ui_1-transform_submit']], {
    progress$complete_transform <- 1
  })

  observeEvent(input[['transform_ui_1-transform_clear']], {
    progress$complete_transform <- 0
  })

  # enable tabs sequentially----------------------------------------------------
  observe({
    toggleState(selector = "#beta_menu li a[data-value=filter_beta_tab]",
                condition = progress$complete_agg == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=transform_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=diss_tab]",
                condition = progress$complete_transform == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=pcoa_tab]",
                condition = progress$complete_transform == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=pca_tab]",
                condition = progress$complete_transform == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=permanova_tab]",
                condition = progress$complete_transform == 1)
  })

  observe({
    toggleState(selector = "#beta_menu li a[data-value=beta_report_tab]",
                condition = progress$complete_transform == 1)
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
    req(input[['aggregate_ui_1-agg_calculate']])

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
  observeEvent(input[['filterfeat_ui_1-submit_asv']], {
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

  # transform filtered data-----------------------------------------------------
  transform_output <- callModule(mod_transform_server, "transform_ui_1", bridge)

  # add transformed data to reactive values
  observe({
    req(input[['transform_ui_1-transform_submit']])
    # add to bridge
    bridge$transform_method <- input[['transform_ui_1-transform_method']]
    bridge$asv_transform <- transform_output$output$asv_transform


  })

  observe({
    req(input[['transform_ui_1-transform_submit']])
    # add to report params
    for_report$params$transform_method <- input[['transform_ui_1-transform_method']]
    for_report$params$asv_transform <- transform_output$output$asv_transform
  })

  # render dissimilarity and pca tabsitem--------------------------------------
  observe({
    req(input[['transform_ui_1-transform_method']])
    toggle(condition=input[['transform_ui_1-transform_method']] == 'percent',
           selector = "#beta_menu li a[data-value=diss_tab]")
    toggle(condition=input[['transform_ui_1-transform_method']] != 'percent',
           selector = "#beta_menu li a[data-value=pca_tab]")
  })

  # Dissimilarity server--------------------------------------------------------
  dissimilarity <- callModule(mod_ov_diss_server, "ov_diss_ui_1", bridge)

  # dissimilarity
  observe({
    req(input[['transform_ui_1-transform_method']],
        input[['ov_diss_ui_1-diss_calculate']])
    if(input[['transform_ui_1-transform_method']] == 'percent') {
      for_report$params$diss_grp <- dissimilarity$output$diss_grp
      for_report$params$diss_panel <- dissimilarity$output$diss_panel
      for_report$params$validation_msg <- dissimilarity$output$validation_msg
      for_report$params$diss_msg <- dissimilarity$output$diss_msg
      for_report$params$diss_result <- dissimilarity$output$diss_result
      for_report$params$p_diss <- dissimilarity$output$p_diss
      for_report$params$diss_stat <- dissimilarity$output$diss_stat
    }
  })

  # PCoA server-----------------------------------------------------------------
    pcoa_output <- callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1",
                              bridge = bridge)

  # pcoa
  observe({
    req(input[['ov_pcoa_ui_1-pcoa_calculate']])
    for_report$params$pcoa_dist <- input[['ov_pcoa_ui_1-pcoa_dist']]
    for_report$params$pcoa_summary <- pcoa_output$output$pcoa_summary
    for_report$params$p_pcoa <- pcoa_output$output$p_pcoa
  })

  # PCA server------------------------------------------------------------------
    pca_output <- callModule(mod_ov_pca_server, "ov_pca_ui_1", bridge)

  # pca
  observe({
    req(input[['ov_pca_ui_1-pca_calculate']])
      for_report$params$pca_scale <- input[['ov_pca_ui_1-pca_scale']]
      for_report$params$pca_summary <- pca_output$output$pca_summary
      for_report$params$p_biplot <- pca_output$output$p_biplot
      for_report$params$p_score <- pca_output$output$p_score
      for_report$params$p_load <- pca_output$output$p_load
  })

  # PERMANOVA ------------------------------------------------------------------
  permanova_output <- callModule(mod_ov_permanova_server, "ov_permanova_ui_1",
                                 bridge = bridge)

  # permanova
  observe({
    req(input[['ov_permanova_ui_1-pcoa_calculate']])
    for_report$params$permanova_stratify <- input[['ov_permanova_ui_1-stratify']]
    for_report$params$permanova_dist <- input[['ov_permanova_ui_1-permanova_dist']]
    for_report$params$permanova_terms <- input[['ov_permanova_ui_1-formula_terms']]
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

