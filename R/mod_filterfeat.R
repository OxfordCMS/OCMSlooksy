#' filterfeat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal bridgeeters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs
mod_filterfeat_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('filterfeat check'), br(), verbatimTextOutput(ns('check'))),
    column(width = 12,
           h1('Filter Features'),
           tags$div(
             "It may be desirable to perform analysis on a subset of features (e.g. sequences) if certain taxa are considered contamination, or are no longer relevant to the current research question.",
             br(),br(),
             tags$em(
               tags$b("NB:"), "Filtering sequences based on sequence quality and minimum count threshold has already been performed during quality control processing of the dataset. Filtering features at this stage should only be done if you have additional reasoning for omitting certain sequences or features"),
             br())
    ), # end column 1 description text
    div(
      id = ns('asv_filter_div'),
      column(
        width = 12, # begin asv_option_count column
        hidden(div(
          id = ns('asv_option_count'),
          br(),
          p("There are three methods by which sequences can be filtered. For all three methods, the cut-off threshold is taken into consideration with the prevalence of sequences across the samples*."), br(),
          p("1) 'Read count' sets the filter threshold at a specific read count, such that a given sequence must be observed greater than or equal to the cut-off count."),
          p("2) 'Percent of sample total' looks at read counts as abundances relative to the sample total. This is useful for when you want to keep features that make up at least x% in your samples."),
          p("3) 'Percent of dataset total' looks at read counts as abundances relative to the dataset total. This is useful for when you want to keep features that make up at least x% in your dataset."), br(),
          p("*Sequence prevalence is calculated as the number of samples in which sequence abundance is greater than or equal to the cut-off threshold."),
          column(
            width = 3, br(),
            wellPanel(
              h3('Filter features based on read count threshold'),
              radioButtons(
                ns('cutoff_method'), 'Set filter cutoff by:',
                c('Read count' = 'abs_count',
                  'Percent of total read count of each sample' = 'percent_sample',
                  'Percent of total read count of dataset' = 'percent_total'),
                selected = 'abs_count')
            )
          ),
          column(
            width = 4, br(),
            hidden(div(
              id = ns('cutoff_limit'),
              wellPanel(
                tags$div('min: 0', br(),
                         'max:',
                         textOutput(ns('cutoff_max'), inline = TRUE)),
                br(),
                uiOutput(ns('asv_cutoff_ui')),
                uiOutput(ns('prevalence_ui')),
                tags$b(textOutput(ns('asv_cutoff_msg')) %>%
                         shinycssloaders::withSpinner()),
                checkboxInput(ns("show_prev_plot"), "Show read prevalence plot")
              )
            )) # end cutoff_limit
          ),
          column(
            width = 5, br(),
            hidden(div(
              id = ns('prev_read_plot_div'),
              tabsetPanel(
                type = "tabs",
                tabPanel("Aggregated view",
                         plotlyOutput(ns('prev_agg_plot')) %>%
                           shinycssloaders::withSpinner()),
                tabPanel("Expanded view",
                         plotlyOutput(ns('prev_read_plot')) %>%
                           shinycssloaders::withSpinner())
              )
            ))
          )
        )) # end asv_option_count
      ), # end asv_option_count column
    ), # end asv_filter_div
    column(
      width = 12,
      hidden(div(
        id = ns('asv_option_select'),
        h3('Filter features based on selection'),
        p("Select the features that you wish to", strong("exclude"),"from subsequent analyses. You can filter the rows using the search fields at the top of each column, or using the search bar at the top right corner of the table. Multiple rows can be selected at once by clicking the first row, then holding the shift button while clicking the last row. The number of columns shown is limited to 50 samples, be default. To show more samples, click on 'Column visibility' to get a dropdown menu of the samples and select the ones you wish to display. Click 'Filter features' to apply changes."), br(),
        DT::dataTableOutput(ns('asv_table_select')) %>%
          shinycssloaders::withSpinner()
      )) # end asv_option_select
    ),
    column(
      width = 12, br(),
      hidden(div(
        id = ns('asv_remove_div'),
        wellPanel(textOutput(ns('asv_remove')))
      )) # end asv_remove_div
    ),
    column(
      width = 12,
      hidden(div(
        id = ns('secondary_check_div'),
        wellPanel(
          tags$b(textOutput(ns('secondary_filter_sample'))),
          htmlOutput(ns('secondary_filter_sample_ui')),
          tags$b(textOutput(ns('secondary_filter_asv'))),
          htmlOutput(ns('secondary_filter_asv_ui'))
        )
      ))
    ),
    column(
      width = 12,
      h3(textOutput(ns('preview_asv_title'))),
      DT::dataTableOutput(ns('preview_asv'))  %>%
        shinycssloaders::withSpinner()
    )
  )
}
    
#' filterfeat Server Function
#'
#' @param input,output,session Internal bridgeeters for {shiny}.
#' @param bridge reactiveValues. contains objects to be passed from outer module
#' 
#' @noRd 
mod_filterfeat_server <- function(input, output, session, bridge){
  ns <- session$ns
  
  # unpack data from parent module----------------------------------------------
  met <- reactive(bridge$work_db$met)
  asv <- reactive(bridge$work_db$asv)
  tax <- reactive(bridge$work_db$tax)
  wip <- reactive(asv() %>% gather('sampleID','read_count', -featureID))

  # control UI based on filter method--------------------------------------
  observeEvent(bridge$filter_input$asv_select_prompt, {
    toggle('asv_filter_div', condition = bridge$filter_input$asv_select_prompt == 'some')
  })

  # main panel
  # by read count
  observeEvent(bridge$filter_input$asv_filter_options, {
    toggle(id = 'asv_option_count',
           condition = grepl('asv_by_count', bridge$filter_input$asv_filter_options))
    show(id = "cutoff_limit")
  })

  observeEvent(input$show_prev_plot, {
    toggle(id = 'prev_read_plot_div', condition = input$show_prev_plot == TRUE)
  })

  # by selected ASV
  observeEvent(bridge$filter_input$asv_filter_options, {
    toggle(id = 'asv_option_select',
           condition = grepl('asv_by_select', bridge$filter_input$asv_filter_options))
    toggle(id = 'asv_remove_div',
           condition = grepl('asv_by_select', bridge$filter_input$asv_filter_options))
  })

  observeEvent(bridge$filter_input$submit_asv, {
    toggle(id = 'secondary_check_div', condition = (secondary_check_sample() == TRUE | secondary_check_asv() == TRUE))
  })

  # ui outputs for asv filter---------------------------------------------------
  # ui for prevalence threshold
  output$prevalence_ui <- renderUI({
    nsample <- length(unique(wip()$sampleID))
    default <- round(nsample * 0.05)
    if(default < 1) default <- 1
    numericInput(ns('prevalence'),
                 "Feature prevalence (# of samples):",
                 min = 1, max = nsample, value = default)
  })
  # ui for  cut-off threshold
  ui_entry <- eventReactive(input$cutoff_method, {
    if(input$cutoff_method == 'abs_count') {
      label <- 'Read count cut-off:'
      max_cutoff <- max(wip()$read_count)
      step <- 1000
      default_value <- 1

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2 in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }
    if(input$cutoff_method == 'percent_sample') {
      label <- 'Read count cut-off (% of sample total):'
      max_cutoff <- 100
      step <- 0.01
      default_value = 0.01

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of sample total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }
    if(input$cutoff_method == 'percent_total') {
      label <- 'Read count cut-off (% of dataset total):'
      max_cutoff <- 100
      step <- 0.01
      default_value = 0.01

      msg <- 'Keeping REPLACE1/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of dataset total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'
    }

    list('label' = label, 'max_cutoff' = max_cutoff, 'step' = step,
         'default_value' = default_value, 'msg' = msg)
  })

  output$asv_cutoff_ui <- renderUI({
    req(input$cutoff_method)
    numericInput(ns('asv_cutoff'), label = ui_entry()$label,
                 value = ui_entry()$default_value, step = ui_entry()$step,
                 min = 0, max = ui_entry()$max_cutoff)
  })

  output$cutoff_max <- renderText({
    req(input$cutoff_method)
    ui_entry()$max_cutoff})

  output$asv_cutoff_msg <- renderText({
    req(input$cutoff_method)
    tot_feat <- wip() %>% filter(read_count > 0)
    tot_feat <- length(unique(tot_feat$featureID))
    rel_feat <- length(to_keep()) / tot_feat * 100
    rel_feat <- round(rel_feat, 1)
    tot_samp <- length(unique(met()$sampleID))
    rel_samp <- as.numeric(input$prevalence) / tot_samp * 100
    rel_samp <- round(rel_samp, 1)
    out <- gsub("REPLACE1", as.character(length(to_keep())), ui_entry()$msg)
    out <- gsub("TOT_FEAT", as.character(tot_feat), out)
    out <- gsub("REL_FEAT", as.character(rel_feat), out)
    out <- gsub("REL_FEAT", as.character(rel_feat), out)
    out <- gsub('REPLACE2', as.character(input$asv_cutoff), out)
    out <- gsub("REPLACE3", as.character(input$prevalence), out)
    out <- gsub("TOT_SAMP", as.character(tot_samp), out)
    out <- gsub("REL_SAMP", as.character(rel_samp), out)
  })

  # by selecting ASVs
  output$asv_table_select <- DT::renderDataTable({
    req(bridge$filter_input$asv_filter_options)


    out <- tax() %>%
      left_join(wip(), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      spread(sampleID, read_count) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # by default, only show first 50 samples + 8 tax columns
    if(ncol(out) <= 58) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(out) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 59:ncol(out) # index of columns to hide
      vis_val <- FALSE
    }

    DT::datatable(out, filter = 'top',
                  extensions = list(c('Buttons', 'FixedColumns')),
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv'),
                                   list(extend = 'colvis')),
                    filter = 'top',
                    columnDefs = list(
                      list(targets = col_ind, visible = vis_val)
                    )))
  })

  # prepare asv dataframe-------------------------------------------------------
  working_asv <- reactive({

    if(bridge$filter_input$asv_select_prompt == 'some') {
      dataset_total <- sum(wip()$read_count)

      out <- wip() %>%
        # calculate relative abundance as dataset total
        mutate(ds_rel_abund = read_count / dataset_total * 100) %>%
        # calculate relative abundance as sample total
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        group_by(sampleID, featureID) %>%
        mutate(samp_rel_abund = read_count / sample_total * 100) %>%
        ungroup()
      out

    }
    else {
      wip()
    }
  })

  # define ASVs to keep---------------------------------------------------------
  to_keep <- reactive({
    req(bridge$filter_input$asv_filter_options)
    if(bridge$filter_input$asv_filter_options == 'asv_by_count') {
      req(input$cutoff_method, input$asv_cutoff, input$prevalence)

      # count cut-off
      if(input$cutoff_method == 'abs_count') {

        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(read_count >= input$asv_cutoff)) %>%
          filter(read_count >= input$asv_cutoff & prev_observed >= input$prevalence)
      }
      # cut-off based on percent of sample total
      if(input$cutoff_method == 'percent_sample') {
        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(samp_rel_abund >= input$asv_cutoff)) %>%
          filter(samp_rel_abund >= input$asv_cutoff & prev_observed >= input$prevalence)

      }
      # cut-off based on percent of dataset total
      if(input$cutoff_method == 'percent_total') {
        out <- working_asv() %>%
          group_by(featureID) %>%
          mutate(prev_observed = sum(ds_rel_abund >= input$asv_cutoff)) %>%
          filter(ds_rel_abund >= input$asv_cutoff & prev_observed >= input$prevalence)
      }
    }

    # filter ASV based on selection---------------------------------------------
    if(bridge$filter_input$asv_filter_options == 'asv_by_select') {
      req(input$asv_table_select_rows_selected)

      out <- tax() %>%
        left_join(wip(), 'featureID') %>%
        select(-Taxon, -sequence) %>%
        spread(sampleID, read_count) %>%
        arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
        slice(-c(input$asv_table_select_rows_selected)) %>%
        gather('sampleID', 'read_count', -featureID)
    }

    unique(out$featureID)
  })

  # show ASVs removed-----------------------------------------------------------
  output$asv_remove <- renderText({
    req(bridge$filter_input$asv_filter_options, input$asv_table_select_rows_selected)
    featID <- unique(wip()$featureID)
    sprintf("Removing %s Features", length(featID[!featID %in% to_keep()]))
  })


  # giving preview on read and prevalence
  output$prev_agg_plot <- renderPlotly({
    scaleFUN <- function(x) sprintf("%.0f", x)

    req(bridge$filter_input$asv_filter_options)

    if(input$cutoff_method == 'abs_count') {
      y = 'agg_count'
      ylab <- 'Aggregated Read count'
    }
    if(input$cutoff_method == 'percent_total') {
      y = 'agg_ds_rel'
      ylab <- 'Aggregated Relative abundance\n(% of total reads)'
    }
    if(input$cutoff_method == 'percent_sample') {
      y = 'agg_samp_rel'
      ylab <- 'Aggregated Relative abundance\n(% of sample)'
    }

    pdata <- working_asv() %>%
      group_by(featureID) %>%
      # number of samples in which asv meets read cutoff
      mutate(prev_observed = sum(read_count >= input$asv_cutoff)) %>%
      group_by(featureID) %>%
      summarise(prev_observed = prev_observed,
                agg_count = sum(read_count),
                agg_samp_rel = sum(samp_rel_abund),
                agg_ds_rel = sum(ds_rel_abund)) %>%
      mutate(colour = ifelse(featureID %in% to_keep(),
                             'to keep', 'to remove')) %>%
      distinct()

    p <- ggplot(pdata, aes_string(x = 'prev_observed', y = y, colour = 'colour')) +
      geom_point(aes(text=sprintf("featureID: %s", featureID)), alpha = 0.5, size = 2) +
      xlab('ASV prevalence (# of samples)') +
      ylab(ylab) +
      scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met()))) +
      scale_y_continuous(trans = 'log10') +
      theme_bw(12) +
      theme(legend.position = 'bottom')

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = -0.5, y = -1))
  })

  output$prev_read_plot <- renderPlotly({
    scaleFUN <- function(x) sprintf("%.0f", x)

    req(bridge$filter_input$asv_filter_options)

    if(input$cutoff_method == 'abs_count') {
      y = 'read_count'
      ylab <- 'Read count'
    }
    if(input$cutoff_method == 'percent_total') {
      y = 'ds_rel_abund'
      ylab <- 'Relative abundance\n(% of total reads)'
    }
    if(input$cutoff_method == 'percent_sample') {
      y = 'samp_rel_abund'
      ylab <- 'Relative abundance\n(% of sample)'
    }

    pdata <- working_asv() %>%
      group_by(featureID) %>%
      # number of samples in which asv meets read cutoff
      mutate(prev_observed = sum(read_count >= input$asv_cutoff),
             colour = ifelse(featureID %in% to_keep(), 'to keep', 'to remove')) %>%
      distinct()

    p <- ggplot(pdata, aes_string(x = 'prev_observed', y = y, colour = 'colour')) +
      geom_point(aes(text=sprintf("featureID: %s<br>sampleID: %s",
                                  featureID, sampleID)),
                 alpha = 0.5, size = 2) +
      xlab('ASV prevalence (# of samples)') +
      ylab(ylab) +
      scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met()))) +
      scale_y_continuous(trans = 'log10') +
      theme_bw(12) +
      theme(legend.position = 'bottom')
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = -0.5, y = -1))
  })

  # filter ASVs based on set cutoff/selection-----------------------------------
  asv_filtered <- eventReactive(bridge$filter_input$submit_asv, {

    if(bridge$filter_input$asv_select_prompt == 'some') {
      working_asv() %>%
        filter(featureID %in% to_keep())  %>%
        select(-ds_rel_abund, -samp_rel_abund, -sample_total)
    }
    else {
      working_asv()
    }
  })


  # secondary check for samples with no reads-----------------------------------
  sample_total <- reactive({
    asv_filtered() %>%
      group_by(sampleID) %>%
      summarise(sample_total = sum(read_count))
  })

  asv_total <- reactive({
    asv_filtered() %>%
      group_by(featureID) %>%
      summarise(asv_total = sum(read_count))
  })
  secondary_check_sample <- reactive({
    any(sample_total()$sample_total == 0)
  })

  secondary_check_asv <- reactive({
    any(asv_total()$asv_total == 0)
  })

  # identify empty samples
  empty_sample <- reactive({
    out <- sample_total() %>%
      filter(sample_total == 0)
    out$sampleID
  })

  n_empty_sample <- reactive({length(empty_sample())})

  output$secondary_filter_sample <- renderText({
    sprintf("%s samples contained 0 reads after ASV filtering. The following samples have been removed:", n_empty_sample())
  })

  output$secondary_filter_sample_ui <- renderUI({
    HTML(paste(empty_sample(), collapse = '<br/>'))
  })


  # identify empty asvs
  empty_asv <- reactive({
    out <- asv_total() %>%
      filter(asv_total == 0)
    out$featureID
  })

  n_empty_asv <- reactive({length(empty_asv())})

  output$secondary_filter_asv <- renderText({
    sprintf("%s asvs contained 0 reads in all samples after ASV filtering. The following asvs have been removed:", n_empty_asv())
  })

  output$secondary_filter_asv_ui <- renderUI({
    HTML(paste(empty_asv(), collapse = '<br/>'))
  })


  # update asv_filtered
  asv_filtered2 <- eventReactive(bridge$filter_input$submit_asv, {

    withBusyIndicatorServer('submit_asv', "setup_ui_1", {
      if(secondary_check_sample()) {
        asv_filtered() %>%
          filter(!sampleID %in% empty_sample())
      }
      else if(secondary_check_asv()) {
        asv_filtered() %>%
          filter(!featureID %in% empty_asv())
      }
      else {
        asv_filtered()
      }
    })

  })

  # update met
  met_filtered <- eventReactive(bridge$filter_input$submit_asv, {
    if(secondary_check_sample()) {
      met() %>%
        filter(!sampleID %in% empty_sample())
    }
    else {
      met()
    }
  })

  output$preview_asv_title <- renderText({
    req(bridge$filter_input$submit_asv)
    'Features included in analysis'
  })

  output$preview_asv <- DT::renderDataTable({
    req(bridge$filter_input$submit_asv)
    out <- tax() %>%
      right_join(asv_filtered2() %>%
                   spread(sampleID, read_count), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # by default, only show first 50 samples + 8 tax columns
    if(ncol(out) <= 58) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(out) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 59:ncol(out) # index of columns to hide
      vis_val <- FALSE
    }
    DT::datatable(out, extensions = list(c('Buttons', 'FixedColumns')),
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv'),
                                   list(extend = 'colvis')),
                    fixedColumns=list(leftColumns = 2),
                    columnDefs = list(
                      list(targets = col_ind, visible = vis_val)
                    )))
  })

  # add filtered tables to bridge reactive-------------------------------------
  cross_mod <- reactiveValues()
  observe({
    req(bridge$filter_input$submit_asv)
    cross_mod$filtered <- list(met = met_filtered(),
                            asv = asv_filtered2() %>%
                              spread(sampleID, read_count),
                            tax = tax() %>%
                              filter(featureID %in% asv_filtered2()$featureID)
    )
  })

  # # check
  # output$check <- renderPrint({
  # })
  
  return(cross_mod)
}
    
## To be copied in the UI
# mod_filterfeat_ui("filterfeat_ui_1")
    
## To be copied in the server
# callModule(mod_filterfeat_server, "filterfeat_ui_1")
 
