#' profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import heatmaply

mod_profile_ui <- function(id){
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
        id = 'profile_menu',
        well=FALSE,
        widths=c(3,9),
        # info tab----------------------------------------------------------
        tabPanel(
          'Module Info',
          value = "info_tab_profile",
          icon = icon('info-circle'), selected = TRUE,
          fluidRow(
            br(),br(),
            column(
              width = 12,
              h1('Microbiome Profile'),
              div(
                p("Examine the relative abundance microbiome profile at various taxonomic levels."),
                p("Module overview:"),
                tags$ul(
                  tags$li(tags$b("Aggregate Features:"), "Select the taxonomic level at which you want to examine the microbiome profiles"),
                  tags$li(tags$b("Filter Features:"), "Filter aggregated features based on feature abundance and prevalence"),
                  tags$li(tags$b("Microbiome Profile:"), "Visualise the microbiome in samples or sample groups."),
                  tags$li(tags$b("Profile Sparsity:"), "Examine the zero-content of microbiome profiles. Data sets that are very sparse are prone to spurious correlations during differential abundance testing and may require analysis methods that account for zero-inflation within a dataset.")
                )
              )
            )
          )
        ), # end info tab
        # aggregate tab body------------------------------------------------
        tabPanel(
          'Aggregate Features',
          value = 'agg_prof_tab',
          fluidRow(
            column(
              width = 12,
              br(),br(),
              mod_aggregate_ui(ns("aggregate_ui_1"))
            )
          )

        ), # end tabItem
        # filter tab body---------------------------------------------------
        tabPanel(
          'Filter Features',
          value = "filter_prof_tab",
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
        # profile tab-------------------------------------------------------
        tabPanel(
          'Microbiome Profile',
          value = "profile_tab",
          fluidRow(
            br(),br(),
            h1('Microbiome Profile'),
            column(
              width = 4,
              # Bar plot controls---------------------------------------------------
              wellPanel(
                id=ns('profile_param_div'),
                tags$div(style = 'text-align: center',
                         tags$b('Plot Parameters')),
                uiOutput(ns('bar_x_ui')),
                uiOutput(ns('bar_panel_ui')),
                radioButtons(ns('bar_y'), 'y-axis',
                             c('Relative abundance' = 'rel_abund',
                               'Read count' = 'cnt_abund')),
                uiOutput(ns('tax_level_ui')),
                withBusyIndicatorUI(
                  actionButton(ns('submit_bar'), "Apply changes"))
              ),
            ),
            column(
              width = 8,
              p("Observing relative abundance (%) or sequence abundance (count) based on metadata variables. The mean relative abundance is shown when selected group contains multiple samples. You can also panel the bar plot by one metadata variable. Finally, you man choose the taxonomic level at which to visualise the microbiome profiles. Click 'Apply changes' to update plots.")
            ),
            hr()
          ),
          hidden(div(id=ns('profile_result_div'),
            fluidRow(
              h3(textOutput(ns('bar_title'))),
              DT::dataTableOutput(ns('bar_table'))  %>%
                shinycssloaders::withSpinner()
            ),
            fluidRow(
              column(
                width = 1, style = 'padding:0px;',
                mod_download_ui(ns("download_bar"))
              ),
              column(
                width = 11, style = 'padding:0px;',
                plotlyOutput(ns('bar_plot'), width = '100%') %>%
                  shinycssloaders::withSpinner()
              )
            )
          )) # end div
        ), # end profile tab
        # sparsity tab------------------------------------------------------
        tabPanel(
          "Profile Sparsity",
          value = "sparsity_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              h1("Profile Sparsity"),
              tags$div("It is a good idea to check the zero content of microbiome data. The proportion of zeros in the data set is an indication of whether features are prevalent throughout samples (low sparsity) or are rarely observed in the dataset (high sparsity). Sparsity is evaluated at the level of count aggregation in the first step of this analysis module")
            )
          ),
          fluidRow(
            h2('Histogram of the zero content of samples'),
            column(
              width=1, style='padding:0px;',
              mod_download_ui(ns("download_sparse"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('zero_content')) %>%
                shinycssloaders::withSpinner()
            )
          ),
          fluidRow(
            br(),
            h2('Presence/Absence heatmap'),
            tags$div(
              "Count data converted into binary, where 0 = absent and 1 = present can be shown as a heatmap. Samples and features are clustered based on Jaccard distance, and complete linkage."
            ),
            column(
              width=1, style='padding:0px;',
              mod_download_ui(ns("download_binary_hmap"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              shinyjqui::jqui_resizable(
                plotlyOutput(ns('binary_hmap'), height='auto')
              )
            )
          )
        ),
        tabPanel(
          'Report',
          value = "profile_report_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              mod_report_ui(ns("profile_report_ui"))
            )
          )
        ) # end report tab
      ) # end navlistPanel
    ) # end fluidPage Page
  ) # end taglist
}

#' profile Server Function
#'
#' @noRd
mod_profile_server <- function(input, output, session, improxy){
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
      reset('profile_param_div')
      hide('profile_result_div')
    }

  })

  # output$check <- renderPrint({
  #
  # })
  # enable tabs sequentially----------------------------------------------------
  observe({
    toggleState(selector = "#profile_menu li a[data-value=filter_prof_tab]",
                condition = progress$complete_agg == 1)
  })

  observe({
    toggleState(selector = "#profile_menu li a[data-value=profile_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    toggleState(selector = "#profile_menu li a[data-value=sparsity_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    toggleState(selector = "#profile_menu li a[data-value=profile_report_tab]",
                condition = progress$complete_featfilt == 1)
  })

  # toggle/show/hide ui elements------------------------------------------------
  observeEvent(input$submit_bar, {
    show('profile_result_div')
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

  # render controls bar plot----------------------------------------------------
  output$bar_x_ui <- renderUI({
    selectInput(ns('bar_x'), "x-axis",
                choices = colnames(bridge$filtered$met),
                selected = 'sampleID')
  })

  output$bar_panel_ui <- renderUI({
    selectInput(ns('bar_panel'), "panel by",
                choices = c('none', colnames(bridge$filtered$met)),
                selected = 'none')
  })

  output$tax_level_ui <- renderUI({
    choices <- c('Kingdom','Phylum','Class','Order','Family','Genus','Species','featureID')
    selectInput(ns('tax_level'), 'Taxonomic level',
                choices= choices[1:which(choices == input[['aggregate_ui_1-aggregate_by']])],
                selected = input[['aggregate_ui_1-aggregate_by']])
  })

  # calculate output bar plot---------------------------------------------------
  bar_data <- eventReactive(input$submit_bar, {

    out <- bridge$filtered$asv %>%
      gather('sampleID','read_count', -featureID) %>%
      left_join(bridge$filtered$tax, 'featureID') %>%
      # sample total read count
      group_by(sampleID) %>%
      mutate(sample_total = sum(read_count)) %>%
      # aggregate on taxon within each sample
      group_by(sampleID, !!sym(input$tax_level)) %>%
      summarise(sample_tax_cnt = sum(read_count),
                sample_tax_rel = sample_tax_cnt / sample_total) %>%
      left_join(bridge$filtered$met, 'sampleID')

    if(input$bar_panel == 'none') {
      out <- out   %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_x), !!sym(input$tax_level))
    } else {
      out <- out %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_panel), !!sym(input$bar_x),
                 !!sym(input$tax_level))
    }
    out %>%
      summarise(cnt_abund = mean(sample_tax_cnt),
                rel_abund = mean(sample_tax_rel)) %>%
      distinct()
  })

  # check number of samples in each group
  n_samp <- eventReactive(input$submit_bar, {
    if(input$bar_panel == 'none') {
      n_samp <- bridge$filtered$met %>%
        group_by(!!sym(input$bar_x)) %>%
        summarise(panel_num = 1, n_samp = n())
    } else {
      n_samp <- bridge$filtered$met %>%
        group_by(!!sym(input$bar_panel)) %>%
        mutate(panel_num = cur_group_id()) %>%
        group_by(!!sym(input$bar_x), .add=TRUE) %>%
        mutate(n_samp = n()) %>%
        distinct(!!sym(input$bar_panel),!!sym(input$bar_x),panel_num, n_samp)
    }
    n_samp
  })


  output$bar_title  <- renderText({
    if(max(n_samp()$n_samp, na.rm = TRUE) == 1) {
      y_title <- ""
    } else {
      y_title <- "Mean "
    }

    if(input$bar_y == 'rel_abund') {
      sprintf('%sRelative Abundance (%%), %s',
              y_title, input$tax_level)
    }
    else {
      sprintf('%sCumulative Read Count, %s',
              y_title, input$tax_level)
    }
  })

  bar_table <- eventReactive(input$submit_bar, {
    if(input$bar_panel == 'none') {
      out <- bar_data() %>%
        distinct(!!sym(input$bar_x), !!sym(input$bar_y),
                 !!sym(input$tax_level)) %>%
        spread(!!sym(input$bar_x), !!sym(input$bar_y))
    } else {
      out <- bar_data() %>%
        distinct(!!sym(input$bar_x), !!sym(input$bar_panel),
                 !!sym(input$bar_y), !!sym(input$tax_level)) %>%
        unite(x, !!sym(input$bar_x), !!sym(input$bar_panel)) %>%
        spread(x, !!sym(input$bar_y))
    }

    out
  })

  output$bar_table <- DT::renderDataTable(server = FALSE, {
    x_name <- colnames(bar_table())

    out <- DT::datatable(bar_table(), colnames = x_name,
                         extensions = 'Buttons',
                         rownames = FALSE,
                         options = list(
                           scrollX = TRUE,
                           dom = 'Blfrtip',
                           buttons = c('copy','csv')))
    if(input$bar_y == 'rel_abund') {
      out <- out %>%
        DT::formatRound(column = x_name[which(x_name != input$tax_level)],
                        digits = 3)
    }
    out
  })

  p_bar <- eventReactive(input$submit_bar, {
    p <- ggplot(bar_data(), aes_string(x = input$bar_x, y = input$bar_y,
                                       fill = input$tax_level)) +
      geom_col() +
      # geom_bar(stat = 'identity') +
      xlab(input$bar_x) +
      scale_fill_discrete(name = input$tax_level) +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))

    if(input$bar_y == 'rel_abund') {
      p <- p +
        ylab(sprintf('Mean Relative Abundance (%%), %s', input$tax_level))
    }
    else {
      p <- p +
        ylab(sprintf('Mean Read Count, %s', input$tax_level))
    }

    if(input$bar_panel != 'none') {
      panel_formula = formula(paste("~", input$bar_panel))
      p <- p + facet_wrap(panel_formula, scales='free', ncol=1)
    }
    p
  })

  output$bar_plot <- renderPlotly({
    h <- max(n_samp()$panel_num, na.rm=TRUE) * 500
    ggplotly(p_bar(), height = h)
  })

  # download data
  for_download1 <- reactiveValues()
  observe({
    req(input$submit_bar)
    for_download1$figure <- p_bar()
    for_download1$fig_data <- bar_data()
  })

  callModule(mod_download_server, "download_bar", bridge = for_download1, 'bar')

  # send to report
  observe({
    req(input$submit_bar)
    for_report$params$bar_y <- input$bar_y
    for_report$params$bar_x <- input$bar_x
    for_report$params$bar_table = bar_table()
    for_report$params$p_bar <- p_bar()
  })
  # sparsity--------------------------------------------------------------------

  # convert count data into binary present-absent
  # assess proportion of zeros in data
  binary_mat <- reactive({
    mat <- as.data.frame(bridge$filtered$asv) %>%
      tibble::column_to_rownames('featureID')
    out <- mat != 0 # False means is zero
    out <- out * 1 # convert boolean to 0 and 1
    out
  })

  zero_content <- reactive({
    # check if is zero
    out <- colSums(!binary_mat())  # true means is zero
    out <- tibble::enframe(out, 'sampleID', 'num_of_zero')
    out$prop_of_zero <- out$num_of_zero / nrow(binary_mat())
    out
  })

  overall_zero <- reactive({
    sum(zero_content()$num_of_zero) / (nrow(binary_mat()) * ncol(binary_mat()))
  })

  p_zero <- reactive({

    p <- ggplot(data = zero_content(), aes(x = prop_of_zero)) +
      geom_histogram(bins = 50, fill=NA, colour='black') +
      labs(subtitle = sprintf("Data set is %s%% zeros",
                              round(overall_zero()*100, 1)),
           x = "\nProportion of zeros in a given sample",
           y = "Number of Samples") +
      theme_classic(12)

    p
  })

  output$zero_content <- renderPlotly({
    ggplotly(p_zero()) %>%
      layout(title=list(text = paste0('<br><sup>',
                                      'Data set is ',
                                      round(overall_zero()*100, 1), "% zeroes",
                                      '</sup>')))
  })

  # download data
  for_download2 <- reactiveValues()
  observeEvent(bridge$filtered,{
    req(input$agg_calculate, input$submit_asv)
    for_download2$figure <- p_zero()
    for_download2$fig_data <- zero_content()
  })

  callModule(mod_download_server, "download_sparse",
             bridge = for_download2, 'sparsity')

  # parameterizing heat map object
  hmap <- reactive({
    heatmaply::heatmapr(
      x = binary_mat(),
      distfun = vegan::vegdist,
      dist_method = 'jaccard',
      hclust_method = 'complete',
      dendrogram = 'both',
      show_dendrogram = c(TRUE, TRUE),
      show_grid = TRUE
    )
  })

  binary_hmap <- reactive({
    heatmaply::heatmaply(hmap(), node_type = 'heatmap',
                         key.title = NULL)
  })
  # plot heat map
  output$binary_hmap <- renderPlotly({
    binary_hmap()
  })

  # download data
  for_download3 <- reactiveValues()
  observeEvent(bridge$filtered,{
    req(input$agg_calculate, input$submit_asv)
    for_download3$figure <- binary_hmap()
    for_download3$fig_data <- as.data.frame(binary_mat()) %>%
      tibble::rownames_to_column('featureID')
  })

  callModule(mod_download_server, "download_binary_hmap",
             bridge = for_download3, 'sparsity_hmap',
             dl_options=c('html','csv','RDS','zip'))


  # send to report
  observeEvent(bridge$filtered, {
    for_report$params$p_zero <- p_zero()
    for_report$params$binary_hmap <- binary_hmap()
  })

  # build report
  callModule(mod_report_server, "profile_report_ui", bridge = for_report,
             template = "profile_report",
             file_name = "profile_report")
}

## To be copied in the UI
# mod_profile_ui("profile_ui_1")

## To be copied in the server
# callModule(mod_profile_server, "profile_ui_1")

