#' alpha UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @import tibble
mod_alpha_ui <- function(id){
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
        id = 'alpha_menu',
        well=FALSE,
        widths=c(3,9),
        # info tab body-----------------------------------------------------
        tabPanel(
          'Module Info',
          value = 'info_tab_alpha',
          icon = icon('info-circle'),
          fluidRow(
            br(), br(),
            column(
              width = 12,
              h1("\u03B1-Diversity"),
              div(
                p("This analysis module evalutes the microbiome on the basis of u03B1-diversity. u03B1-Diversity is measured on a per-sample basis and is quantified using an u03B1-diversity index, such as Shannon's D. More detail is provided of different u03B1-diversity indeces in the 'u03B1-Diversity Analysis' tab."),
                p("Module overview:"),
                tags$ul(
                  tags$li(tags$b("Aggregate Features:"), "Select the taxonomic level at which you want to examine the microbiome profiles"),
                  tags$li(tags$b("Filter Features:"), "Filter aggregated features based on feature abundance and prevalence"),
                  tags$li(tags$b("\u03B1-Diversity Analysis:"), "Calculates \u03B1-diversity and performs group-wise statistical testing where applicable")
                )
              )
            )
          )
        ), # end tabPanel
        # aggregate tab body------------------------------------------------
        tabPanel(
          'Aggregate Features',
          value = 'agg_alpha_tab',
          fluidRow(
            br(),br(),
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
          value = "filter_asv_alpha",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              mod_filterfeat_ui(ns("filterfeat_ui_1"))
            )
          )
        ), # end tabPanel
        # alpha tab body----------------------------------------------------
        tabPanel(
          '\u03B1-Diversity Analysis',
          value = 'alpha_tab',
          fluidRow(
            br(),br(),
            h1("\u03B1-Diversity"),
            p("Alpha diversity assesses the diversity of a community (within one sample). Species richness is the number of unique species. Species evenness is a measure of the consistency of species abundances (uneven data sets have community members that dominate in abundance). Entropy measures such as Shannon's index (H) and Simpson index are measures of uncertainty in the species identity of a sample (", a("Jost 2006", href="https://doi.org/10.1111/j.2006.0030-1299.14714.x"), ")."),
            p("Diversity measures, such as Shannon's Diversity and Inverse Simpson's Index, takes into account of the abundance of species in the community. In fact, when all species in a community are equally common, entropy and diversity measures are equivalent. Entropy indices can be converted to diversity by mathematical transformation."),
            p("Diversity indecies are calculated with", a(code("vegan::diversity"), href="https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf"), "Shannon's D index is calculated as ", code("exp(Shannon's Index)"), "Richness is calculated with", a(code("vegan::specnum"), href="https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf"), ", and evenness is calculated as ", code("Shannon's Index/log(Richness)"), ".")
          ),
          fluidRow(
              DT::dataTableOutput(ns('alpha_table'))  %>%
                shinycssloaders::withSpinner()
          ), br(),
          fluidRow(
            DT::dataTableOutput(ns('alpha_test'))  %>%
              shinycssloaders::withSpinner()
          ), br(),
          fluidRow(
            column(
              width = 3, br(), br(),
              wellPanel(
                uiOutput(ns('alpha_metric_ui')),
                uiOutput(ns('alpha_grp_ui'))
              )
            ),
            column(
              width = 9,
              column(
                width = 1, style = 'padding:0px;',
                mod_download_ui(ns('download_alpha')),
              ),
              column(
                width = 11, style = 'padding:0px;',
                shinyjqui::jqui_resizable(
                  plotlyOutput(ns('alpha_plot'), width = '100%',
                               height= 'auto') %>%
                    shinycssloaders::withSpinner()
                )
              )
            )
          ) # end fluidRow
        ), # end tabPanel
        # report------------------------------------------------------------
        tabPanel(
          'Report',
          value = "alpha_report_tab",
          fluidRow(
            br(), br(),
            column(
              width = 12,
              mod_report_ui(ns("alpha_report_ui"))
            )
          )

        ) # end tabPanel
      ) # end navlistPanel
    ) # end fluidPage
  ) # end taglist
}

#' alpha Server Function
#'
#' @noRd
mod_alpha_server <- function(input, output, session, improxy){
  ns <- session$ns

  output$check <- renderPrint({
  })

  # enable tabs sequentially----------------------------------------------------
  observe({
    toggleState(selector = "#alpha_menu li a[data-value=filter_asv_alpha]",
           condition = !is.null(agg_output$output) &&
             agg_output$output$agg_calculate > 0)
  })

  observe({
    toggleState(selector = "#alpha_menu li a[data-value=alpha_tab]",
           condition = filter_output$params$filter_submit > 0)
  })

  observe({
    toggleState(selector = "#alpha_menu li a[data-value=alpha_report_tab]",
           condition = filter_output$params$filter_submit > 0)
  })
  # initiate value to pass into submodules--------------------------------------
  bridge <- reactiveValues(dummy=NULL)
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
    for_report$params$aggregate_by <- agg_output$output$aggregate_by
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
    for_report$params$asv_select_prompt <- filter_output$params$asv_select_prompt
    for_report$params$asv_filter_options <- filter_output$params$asv_filter_options
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

  # render controls - alpha diversity-------------------------------------------
  output$alpha_metric_ui <- renderUI({
    checkboxGroupInput(ns('alpha_metric'), 'Diversity Index',
                          choices=c("Shannon's Index (H)" = 'shannon',
                                    "Shannon's D Index (D'H)" = 'shannon_d',
                                    "Simpson Index" = 'simpson',
                                    "Inverse Simpson Index" = 'invsimpson'),
                       selected = 'shannon_d')
  })

  output$alpha_grp_ui <- renderUI({
    selectInput(ns('alpha_grp'), "Compare Sample Groups",
                 choices = colnames(bridge$filtered$met), selected = 'sampleID')
  })

  # calculate alpha diversity---------------------------------------------------

  alpha_result <- reactive({
    req(input$alpha_grp)
    alpha_data <- bridge$filtered$asv %>%
      column_to_rownames('featureID')

    shannon <- vegan::diversity(alpha_data,index = 'shannon',
                                base = 2, MARGIN = 2)
    shannon_d <- exp(shannon)
    richness <- vegan::specnumber(alpha_data, MARGIN = 2)
    evenness <- shannon / log(richness)
    invsimpson <- vegan::diversity(alpha_data,index = 'invsimpson',
                                   base = 2, MARGIN = 2)
    simpson <- vegan::diversity(alpha_data, index = 'simpson',
                                base = 2, MARGIN = 2)

    out <- data.frame(sampleID = names(shannon),
                      shannon = shannon,
                      simpson = simpson,
                      shannon_d = shannon_d,
                      richness = richness,
                      evenness = evenness,
                      invsimpson = invsimpson)
    out
  })

  # determine valid stat test
  grp_tally <- reactive({
    req(input$alpha_grp)
    out <- table(bridge$filtered$met[,input$alpha_grp])
    if(length(out) == 0) out <- 0
    out
  })

  stat_test <- reactive({
    if(length(grp_tally()) == 2) 'wilcox.test'
    else 'kruskal.test'
  })

  alpha_stat <- eventReactive(input$alpha_grp, {

    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )

    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      inner_join(bridge$filtered$met %>%
                   gather('meta_variable','grouping', -sampleID),
                 'sampleID') %>%
      filter(meta_variable == input$alpha_grp)

    out <- ggpubr::compare_means(formula = alpha_value~grouping, data = out,
                                 group.by = c('alpha_metric'),
                                 method = stat_test(), p.adjust.method = 'BH')
    out
  })

  # show tables
  output$alpha_table <- DT::renderDataTable(server = FALSE, {
    out <- bridge$filtered$met %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID')
    DT::datatable(out, extensions = 'Buttons',
                  rownames = FALSE,
                  options = list(scrollX = TRUE, dom = 'Blfrtip',
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(alpha_result())[2:ncol(alpha_result())], digits = 3)
  })

  validation_msg <- reactive({
    if(max(grp_tally()) == 1) {
      "Only one observation per group. Group-wise comparisons not performed"
    } else if(length(grp_tally()) == 1) {
      "All observations are in the same group. Group-wise comparisons not performed"
    } else {
      'valid'
    }
  })

  output$alpha_test <- DT::renderDataTable(server = FALSE, {
    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )

    DT::datatable(alpha_stat() %>%
                    select(-.y., -p.format, ), extensions = 'Buttons',
                  rownames = FALSE,
                  options = list(scrollX = TRUE, dom = 'Blfrtip',
                                 buttons = c('copy','csv')))
  })

  # plot alpha diversity
  pdata_alpha <- reactive({
    req(input$alpha_grp, input$alpha_metric)
    # set xorder based on shannon_d
    xorder <- bridge$filtered$met %>%
      mutate_all(as.character) %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID') %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(shannon_d)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]],
                                      desc(alpha_avg)))

    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      filter(alpha_metric %in% c('evenness','richness',input$alpha_metric)) %>%
      inner_join(bridge$filtered$met %>% mutate_all(as.character), 'sampleID') %>%
      group_by(alpha_metric, .data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_metric, alpha_value, alpha_avg) %>%
      ungroup()

    if(validation_msg() == 'valid') {
      # re-calculate comparison statistic
      compare_stat <- alpha_result() %>%
        gather('alpha_metric', 'alpha_value', -sampleID) %>%
        inner_join(bridge$filtered$met %>%
                     gather('meta_variable','grouping', -sampleID),
                   'sampleID') %>%
        filter(meta_variable == input$alpha_grp)

      compare_stat <- ggpubr::compare_means(formula = alpha_value~grouping,
                                            data = compare_stat,
                                            group.by = c('alpha_metric'),
                                            method = stat_test(),
                                            p.adjust.method = 'BH')

      out <- out %>%
        left_join(compare_stat %>% select(alpha_metric, p, p.adj),
                  'alpha_metric') %>%
        mutate(panel = sprintf("%s\np=%0.3f, p.adj=%0.3f",
                               alpha_metric, p, p.adj))
    }

    out
  })
  p_alpha <- reactive({
    req(input$alpha_grp)

    p <- ggplot(pdata_alpha(), aes(x = .data[[input$alpha_grp]],
                                   y = alpha_value,
                                   group = .data[[input$alpha_grp]]))

    if(validation_msg() == 'valid') {
      p <- p + facet_wrap(~panel, scales = 'free')
    } else {
      p <- p + facet_wrap(~alpha_metric, scales = 'free')
    }

    if(min(grp_tally()) > 5) {
      p <- p +
        geom_boxplot(outlier.fill=NA) +
        geom_point(position = position_jitter(width = 0.25, seed = 1),
                   alpha = 0.6)
    }
    else {
      p <- p +
        geom_point(alpha = 0.6)
    }

    p <- p +
      theme_bw() +
      xlab(input$alpha_grp) +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank())

    p
  })

  output$alpha_plot <- renderPlotly({
    out <- plotly_build(p_alpha())
    out$x$data <- lapply(out$x$data, FUN = function(x) {
      x$marker = list(color = "rgba(0,0,0,1)",
                      outliercolor = "rgba(0,0,0,0)",
                      line = list(outliercolor='rgba(0,0,0,0)'))
      return(x)
    })
    out
  })


  # download data
  for_download <- reactiveValues()
  observe({
    req(input$alpha_grp)
    for_download$figure <- p_alpha()
    for_download$fig_data <- pdata_alpha()
  })

  callModule(mod_download_server, "download_alpha", bridge = for_download, 'alpha')


  # initiate list to pass onto report submodule
  observe({
    for_report$params$alpha_result <- alpha_result()
    for_report$params$validation_msg <- validation_msg()
    for_report$params$p_alpha <- p_alpha()
  })

  observe({
    req(validation_msg())
    if(validation_msg() == 'valid') {
      for_report$params[['alpha_stat']] <- alpha_stat()
    }
  })

  # build report
  callModule(mod_report_server, "alpha_report_ui", bridge = for_report,
             template = "alpha_report",
             file_name = "alpha_report")
}

## To be copied in the UI
# mod_alpha_ui("alpha_ui_1")

## To be copied in the server
# callModule(mod_alpha_server, "alpha_ui_1")

