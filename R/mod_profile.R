#' profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
      navlistPanel(
        '',
        id = 'menu',
        well=FALSE,
        widths=c(3,9),
        # info tab----------------------------------------------------------
        tabPanel(
          'Task Info',
          id = "info_tab_profile",
          icon = icon('info-circle'), selected = TRUE,
          fluidRow(
            br(),br(),
            column(
              width = 12,
              h1('Microbiome Profile'),
              p('Examine the relative abundance microbiome profile at various taxonomic levels.')  
            )
          )
        ), # end info tab
        # aggregate tab body------------------------------------------------
        tabPanel(
          'Aggregate Features',
          id = 'agg_prof_tab',
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
          'Filter features',
          id = "filter_prof_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              mod_filterfeat_ui(ns("filterfeat_ui_1"))  
            )
          )
        ), # end tabItem
        # profile tab-------------------------------------------------------
        tabPanel(
          'Microbiome Profile',
          id = "profile_tab",
          fluidRow(
            br(),br(),
            h1('Microbiome Profile'),
            column(
              width = 4,
              # Bar plot controls---------------------------------------------------
              wellPanel(
                br(), 
                tags$div(style = 'text-align: center', tags$b('Plot Parameters')),
                uiOutput(ns('bar_x_ui')),
                uiOutput(ns('bar_panel_ui')),
                radioButtons(ns('bar_y'), 'Response measure:',
                             c('Relative abundance' = 'rel_abund',
                               'Read count' = 'cnt_abund')),
                withBusyIndicatorUI(
                  actionButton(ns('submit_bar'), "Apply changes"))
              ),
            ),
            column(
              width = 8,
              h3(textOutput(ns('bar_title'))),
              p("Observing relative abundance or sequence abundance based on metadata variables. Abundance values can be aggregated at different taxonomic levels. The mean relative abundance is shown when selected group variable contains multiple samples")
            ),
            hr()
          ),
          fluidRow(
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
              plotlyOutput(ns('bar_plot'), width = '100%', height = '100%') %>%
                shinycssloaders::withSpinner()
            )
          )
        ), # end profile tab
        # sparsity tab------------------------------------------------------
        tabPanel(
          "Profile Sparsity",
          id = "sparsity_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              h1("Profile Sparsity"),
              tags$div("It is a good idea to check the zero content of microbiome data. The proportion of zeros in the data set is an indication of whether features are prevalent throughout samples (low sparsity) or are rarely observed in the dataset (high sparsity).")    
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
              plotlyOutput(ns('binary_hmap'), height='auto') %>%
                shinycssloaders::withSpinner()
            )  
          )
        ),
        tabPanel(
          'Report',
          id = "profile_report_tab",
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


  # calculate output bar plot---------------------------------------------------

  bar_data <- reactiveVal()

  observeEvent(input$submit_bar, {

    out <- bridge$filtered$asv %>%
      gather('sampleID','read_count', -featureID) %>%
      left_join(bridge$filtered$tax, 'featureID') %>%
      left_join(bridge$filtered$met, 'sampleID')

    if(input$bar_panel == 'none') {
      out <- out %>%
        # sample total read count
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        # aggregate on taxon within each sample
        group_by(sampleID, featureID) %>%
        summarise(!!input$bar_x := .data[[input$bar_x]],
                  tax_cnt = sum(read_count),
                  tax_rel = tax_cnt / sample_total) %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_x), featureID) %>%
        summarise(cnt_abund = mean(tax_cnt),
                  rel_abund = mean(tax_rel)) %>%
        distinct()
    } else {
      out <- out %>%
        # sample total read count
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        # aggregate on taxon within each sample
        group_by(sampleID, featureID) %>%
        summarise(!!input$bar_x := .data[[input$bar_x]],
                  !!input$bar_panel := .data[[input$bar_panel]],
                  tax_cnt = sum(read_count),
                  tax_rel = tax_cnt / sample_total) %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_x), featureID) %>%
        summarise(!!input$bar_panel := .data[[input$bar_panel]],
                  cnt_abund = mean(tax_cnt),
                  rel_abund = mean(tax_rel)) %>%
        distinct()
    }
    bar_data(out)
  })

  output$bar_title  <- renderText({
    req(input$submit_bar)
    if(input$bar_y == 'rel_abund') {
      sprintf('Mean Relative Abundance (%%), %s', input$aggregate_by)
    }
    else {
      sprintf('Mean Cumulative Read Count, %s', input$aggregate_by)
    }
  })

  bar_table <- eventReactive(input$submit_bar, {
    if(input$bar_panel == 'none') {
      out <- bar_data() %>%
        distinct(!!sym(input$bar_x), !!sym(input$bar_y), featureID) %>%
        spread(!!sym(input$bar_x), !!sym(input$bar_y))
    } else {
      out <- bar_data() %>%
        distinct(!!sym(input$bar_x), !!sym(input$bar_panel),
                 !!sym(input$bar_y), featureID) %>%
        unite(x, !!sym(input$bar_x), !!sym(input$bar_panel)) %>%
        spread(x, !!sym(input$bar_y))
    }
    out
  })

  output$bar_table <- DT::renderDataTable({
    req(input$aggregate_by, input$submit_bar)
    out <- bar_table()
    x_name <- colnames(out)
    ind <- which(x_name == 'featureID')
    x_name[ind] <- input$aggregate_by

    out <- DT::datatable(out, colnames = x_name,
                         extensions = 'Buttons',
                         options = list(
                           scrollX = TRUE,
                           dom = 'Blfrtip',
                           buttons = c('copy','csv')))
    if(input$bar_y == 'rel_abund') {
       out %>%
        DT::formatRound(column = x_name[which(x_name != input$aggregate_by)],
                        digits = 3)
    }
    else {
      out
    }
  })

  p_bar <- eventReactive(input$submit_bar, {
    p <- ggplot(bar_data(), aes_string(x = input$bar_x, y = input$bar_y,
                                       fill = 'featureID')) +
      geom_bar(stat = 'identity') +
      xlab(input$bar_x) +
      scale_fill_discrete(name = input$aggregate_by) +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))

    if(input$bar_y == 'rel_abund') {
      p <- p +
        ylab(sprintf('Mean Relative Abundance (%%), %s', input$aggregate_by))
    }
    else {
      p <- p +
        ylab(sprintf('Mean Read Count, %s', input$aggregate_by))
    }

    if(input$bar_panel != 'none') {
      panel_formula = formula(paste("~", input$bar_panel))
      p <- p + facet_wrap(panel_formula, scales='free')
    }
    p
  })

  output$bar_plot <- renderPlotly({
    ggplotly(p_bar())
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
  
  
  output$check <- renderPrint({
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

