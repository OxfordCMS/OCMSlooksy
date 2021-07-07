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
      # wellPanel(width = 12, h3('filterfeat check'), br(), verbatimTextOutput(ns('check'))),
      fluidRow(
        column(
          width = 12,
          h1('Filter Features'),
          tags$div(
           "It may be desirable to perform analysis on a subset of features (e.g. sequences) if certain taxa are considered contamination, or are no longer relevant to the current research question.",
           br(),br(),
           tags$em(
             tags$b("NB:"), "Filtering sequences based on sequence quality and minimum count threshold has already been performed during quality control processing of the dataset. Filtering features at this stage should only be done if you have additional reasoning for omitting certain sequences or features"),
 br())
        ) # end column 1 description text
      ), # end fluidRow
      fluidRow(
        # filter menu controls------------------------------------------------
        wellPanel(
          fluidRow(
            column(
              width=3,
              radioButtons(ns('asv_select_prompt'),
                           "Features to exclude from analysis:",
                           choices = c('Use all features' = 'all',
                                       'Filter features' = 'some'),
                           selected = 'all'),

              hidden(div(
                id = ns('asv_filter_options_div'),
                radioButtons(
                  ns('asv_filter_options'), 'Filter features by:',
                  choices = c('read count' = 'asv_by_count',
                              'selection' = 'asv_by_select'),
                  selected = "asv_by_count")
              )) # end hidden div filter options
            ), # end left column of wellPanel
            hidden(div(
              id = ns('asv_count_div'),
              column(
              width = 9,
                tags$b('Filter features based on read count threshold'),
                fluidRow(
                  column(
                    width = 6, br(),
                    radioButtons(
                      ns('cutoff_method'), 'Set filter cutoff by:',
                      c('Read count' = 'abs_count',
                        'Read count as percent of sample' = 'percent_sample',
                        'Read count as percent of dataset' = 'percent_total'),
                      selected = 'abs_count')
                  ),
                  column(
                    width = 6, br(),
                    tags$div('min: 0', br(),
                             'max:',
                             textOutput(ns('cutoff_max'), inline = TRUE)),
                    br(),
                    uiOutput(ns('asv_cutoff_ui')),
                    uiOutput(ns('prevalence_ui'))
                  )
                ) # need fluid row to get columns in wellPanel
              ) # end right column of wellPanel
            )), # end asv_count_div hidden div
            column(
              width = 12,
              div(
                style="display:inline-block;",
                withBusyIndicatorUI(
                  actionButton(ns('submit_asv'), "Filter features"))
              ),
              div(
                style="display:inline-block;",
                withBusyIndicatorUI(
                  actionButton(ns('clear_asv'),
                               tags$div(title = "Reset feature filtering",
                                        icon("undo-alt"))
                  )
                )
              )
            ) # end column for submit/clear button
          ) # end fluidRow in wellPanel
        ) # end wellPanel
      ), # end fluidRow
      hidden(div(
        id = ns('asv_hist_div'),
        fluidRow(
          column(
            width = 6,
            p("There are three methods by which sequences can be filtered. For all three methods, the cut-off threshold is taken into consideration with the prevalence of sequences across the samples*."), br(),
            p("1) 'Read count' sets the filter threshold at a specific read count, such that a given sequence must be observed greater than or equal to the cut-off count."),
            p("2) 'Percent of sample total' looks at read counts as abundances relative to the sample total. This is useful for when you want to keep features that make up at least x% in your samples."),
            p("3) 'Percent of dataset total' looks at read counts as abundances relative to the dataset total. This is useful for when you want to keep features that make up at least x% in your dataset."), br(),
            p("*Sequence prevalence is calculated as the number of samples in which sequence abundance is greater than or equal to the cut-off threshold.")
          ), # end left side column 6
          column(
            width = 6,
            tabsetPanel(
              type = "tabs",
              tabPanel("Prevalence",
                       plotlyOutput(ns('hist_prev_plot')) %>%
                         shinycssloaders::withSpinner()),
              tabPanel("Read count",
                       tags$p("default view shows first 10 bins. Double click to zoom out and see full plot area."),
                       plotlyOutput(ns('hist_readcount_plot')) %>%
                         shinycssloaders::withSpinner()),
              tabPanel("% of Sample",
                       tags$p("default view shows first 15 bins. Double click to zoom out and see full plot area."),
                       plotlyOutput(ns('hist_samptot_plot')) %>%
                         shinycssloaders::withSpinner()),
              tabPanel("% of Data set",
                       tags$p("default view shows first 20 bins. Double click to zoom out and see full plot area."),
                       plotlyOutput(ns('hist_datatot_plot')) %>%
                         shinycssloaders::withSpinner())
            ) # end tabsetPanel
          ) # end rightside column 6
        ) # end fluidRow
      )), # end hidden div asv_hist_div
      hidden(div(
        id = ns('asv_select_div'),
        fluidRow(
          h3('Filter features based on selection'),
          p("Select the features that you wish to", strong("exclude"),"from subsequent analyses. You can filter the rows using the search fields at the top of each column, or using the search bar at the top right corner of the table. Multiple rows can be selected at once by clicking the first row, then holding the shift button while clicking the last row. Click 'Filter features' to apply changes."), br(),
          DT::dataTableOutput(ns('asv_table_select')) %>%
            shinycssloaders::withSpinner()
        ) # end fluidRow
      )), # end asv_select_div
      hidden(div(
        id = ns('prev_filter_div'),
        fluidRow(
          column(
            width=7,
            tabsetPanel(
              type = "tabs",
              tabPanel("Aggregated view",
                       plotlyOutput(ns('prev_agg_plot')) %>%
                         shinycssloaders::withSpinner()),
              tabPanel("Expanded view",
                       plotlyOutput(ns('prev_read_plot')) %>%
                         shinycssloaders::withSpinner())
            )
          ),
          column(
            width = 5,
            uiOutput(ns('secondary_filter_ui')) %>%
              shinycssloaders::withSpinner()
          )
        ) # end fluidRow
      )), # end prev_filter_div hidden div
      fluidRow(
        uiOutput(ns('secondary_check_ui'))
      ),
      hidden(div(
        id = ns('preview_asv_div'),
        fluidRow(
          h3('Features included in analysis'),
          DT::dataTableOutput(ns('preview_asv'))  %>%
            shinycssloaders::withSpinner()
        ) # end fluidRow
      )) # end hidden div preview asv div
  ) # end taglist
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
  observeEvent(input$asv_select_prompt, {
    toggle("asv_filter_options_div",
           condition = input$asv_select_prompt == 'some')
  })

  observe({
    # by read count
    toggle('asv_hist_div',
           condition = input$asv_select_prompt == 'some' &
             grepl('asv_by_count', input$asv_filter_options))
    toggle(id = 'asv_count_div',
           condition = input$asv_select_prompt == 'some' &
             grepl('asv_by_count',input$asv_filter_options))
    # by selected ASV
    toggle(id = 'asv_select_div',
           condition = input$asv_select_prompt == 'some' &
             grepl('asv_by_select', input$asv_filter_options))
  })

  observeEvent(input$submit_asv, {
    show("preview_asv_div")
    toggle(id = 'prev_filter_div',
           condition = input$asv_select_prompt == 'some' &
             grepl('asv_by_count', input$asv_filter_options))
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
  ui_entry <- reactive({
    switch(
      input$cutoff_method,
      abs_count = list(label = 'Read count cut-off:',
                       max_cutoff = max(wip()$read_count),
                       step = 2,
                       default_value = 1,
                       msg = 'Keeping N_KEEP/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2 in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'),
      percent_sample = list(label = 'Read count cut-off (% of sample total):',
                            max_cutoff = 100,
                            step = 0.002,
                            default_value = 0.001,
                            msg = 'Keeping N_KEEP/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of sample total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples'),
      percent_total = list(label = 'Read count cut-off (% of dataset total):',
                           max_cutoff = 100,
                           step = 0.0002,
                           default_value = 0.0001,
                           msg = 'Keeping N_KEEP/TOT_FEAT (REL_FEAT%) features with read counts >= REPLACE2% of dataset total read count in >= REPLACE3/TOT_SAMP (REL_SAMP%) samples')
    )
  })

  output$asv_cutoff_ui <- renderUI({
    numericInput(ns('asv_cutoff'), label = ui_entry()$label,
                 value = ui_entry()$default_value, step = ui_entry()$step,
                 min = 0, max = ui_entry()$max_cutoff)
  })

  output$cutoff_max <- renderText({
    ui_entry()$max_cutoff
  })


  asv_table_select <- reactive({
    tax() %>%
      left_join(asv(), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)
  })

  # by selecting ASVs
  output$asv_table_select <- DT::renderDataTable(server = FALSE, {
    DT::datatable(asv_table_select(), filter = 'top',
                  rownames = FALSE,
                  options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    dom = 'lfrtip',
                    filter = 'top'))
  })
  # # check
  # output$check <- renderPrint({
  # })
  # prepare asv dataframe-------------------------------------------------------
  asv_mat <- reactive({
    as.data.frame(asv()) %>%
    column_to_rownames('featureID')
  })

  # get sample prevalence for features
  prev_data <- reactive({
    # convert asv counts to binary
    binary <- (asv_mat() != 0) * 1 # true means present
    rowSums(binary)
  })

  # relative abundance based on sample total
  relab_samptot <- reactive({
    # convert to relative abundance
    apply(asv_mat(), 2, function(x) x/sum(x))
  })

  relab_datatot <- reactive({
    data_total <- sum(asv_mat())
    asv_mat() / data_total
  })

  # generate histograms to help picking read count and prevalence cutoffs-------
  hist_prev_pdata <- reactive({
    enframe(prev_data(), 'featureID','prevalence')
  })

  hist_samptot_pdata <- reactive({
    as.data.frame(relab_samptot()) %>%
      rownames_to_column('featureID') %>%
      gather('sampleID', 'relab_samptot', -featureID)
  })

  hist_datatot_pdata <- reactive({
    as.data.frame(relab_datatot()) %>%
      rownames_to_column('featureID') %>%
      gather('sampleID', 'relab_datatot', -featureID)
  })

  vline <- function(x = 0, color = "darkred") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, linetype='dashed')
    )
  }

  # render histogram outputs
  output$hist_prev_plot <- renderPlotly({
    plot_ly(nbinsx=nrow(met())) %>%
      add_histogram(x=hist_prev_pdata()$prevalence) %>%
      layout(shapes=list(vline(input$prevalence)),
             bargap=0.1,
             xaxis = list(title = "Prevalence (# of samples)"),
             yaxis = list(title = 'Number of features'))
    # ggplotly(hist_prev_plot())
  })
  output$hist_readcount_plot <- renderPlotly({
    # set default zoom to first 10 bins
    out <- plot_ly(nbinsx=ceiling(max(wip()$read_count)/2)) %>%
      add_histogram(x=wip()$read_count) %>%
      layout(bargap=0.1,
             xaxis=list(range=c(0,20),
                        title = "Read Count (bin size = 1)"),
             yaxis = list(title = 'Number of features'))

    if(input$cutoff_method == 'abs_count') {
      out <- out %>%
        layout(shapes=list(vline(input$asv_cutoff)),
               xaxis=list(title = "Read Count (bin size = 1)"),
               yaxis = list(title = 'Number of features'))
    }

    out
    # ggplotly(hist_readcount_plot())
  })
  output$hist_samptot_plot <- renderPlotly({
    # set default zoom to first 10 bins
    out <- plot_ly(nbinsx=1000) %>%
      add_histogram(x=hist_samptot_pdata()$relab_samptot) %>%
      layout(bargap=0.1,
             xaxis=list(range=c(0,0.015),
                        title = "Relative abundance (bin size = 1000)"),
             yaxis = list(title = 'Number of features'))

    if(input$cutoff_method == 'percent_sample') {
      out <- out %>%
        layout(shapes=list(vline(input$asv_cutoff)),
               xaxis = list(title = "Relative abundance (bin size = 0.001%)"),
               yaxis = list(title = 'Number of features'))
    }

    out
    # ggplotly(hist_samptot_plot())
  })
  output$hist_datatot_plot <- renderPlotly({
    # set default zoom to first 10 bins
    out <- plot_ly(nbinsx=1000) %>%
      add_histogram(x=hist_datatot_pdata()$relab_datatot) %>%
      layout(bargap=0.1,
             xaxis=list(range=c(0,0.001),
                        title = "Relative abundance (bin size = 0.00005%)"),
             yaxis = list(title = 'Number of features'))

    if(input$cutoff_method == 'percent_total') {
      out <- out %>%
        layout(shapes=list(vline(input$asv_cutoff)),
               xaxis = list(title = "Relative abundance (bin size = 1000)"),
               yaxis = list(title = 'Number of features'))
    }

    out
    #ggplotly(hist_datatot_plot())
  })

  working_asv <- reactive({

    if(input$asv_select_prompt == 'some') {
      dataset_total <- sum(wip()$read_count)

      out <- wip() %>%
        # calculate relative abundance as dataset total
        mutate(ds_rel_abund = read_count / dataset_total * 100) %>%
        # calculate relative abundance as sample total
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        group_by(sampleID, featureID) %>%
        mutate(samp_rel_abund = read_count / sample_total * 100) %>%
        group_by(featureID) %>%
        mutate(prev_observed = sum(read_count >= input$asv_cutoff)) %>%
        ungroup()
      out

    }
    else {
      wip()
    }
  })



  # define ASVs to keep---------------------------------------------------------

  calculate_keep <- eventReactive(input$submit_asv, {
    if(input$asv_select_prompt == 'some') {
      if(input$asv_filter_options == 'asv_by_count') {
        out <- switch(
          input$cutoff_method,
          # count cut-off
          abs_count = asv_mat()[rowSums(asv_mat() >= input$asv_cutoff) >= input$prevalence,],
          # cut-off based on percent of sample total
          percent_sample = relab_samptot()[rowSums(relab_samptot() >= input$asv_cutoff) >= input$prevalence,],
          # cut-off based on percent of dataset total
          percent_total = relab_datatot()[rowSums(relab_datatot() >= input$asv_cutoff) >= input$prevalence,]
        )
      }

      # filter ASV based on selection-------------------------------------------
      if(input$asv_filter_options == 'asv_by_select') {
        req(input$asv_table_select_rows_selected)

        out <- asv_table_select() %>%
          slice(-c(input$asv_table_select_rows_selected)) %>%
          column_to_rownames('featureID')
      }
    } else {
      out <- asv_mat()
    }
    rownames(out)
  })

  filter_result <- reactiveValues(to_keep = NULL, to_remove = NULL)
  observe({
    req(calculate_keep())
    filter_result$to_keep <- calculate_keep()
    featID <- rownames(asv_mat())
    filter_result$asv_remove <- featID[!featID %in% calculate_keep()]
  })

  observeEvent(input$clear_asv, {
    filter_result$to_keep <- NULL
    filter_result$asv_remove <- NULL
    selectRows(dataTableProxy('asv_table_select'), NULL)
  })

  # show ASVs removed-----------------------------------------------------------

asv_cutoff_msg <- eventReactive(input$submit_asv, {
  req(filter_result$to_keep)
  validate(need(input$asv_select_prompt == 'some',
                "No filtering performed"),
           need(!is.null(filter_result$to_keep),
                "No filtering performed"))

  req(input$cutoff_method, input$prevalence, input$asv_cutoff)
  tot_feat <- wip() %>% filter(read_count > 0)
  tot_feat <- length(unique(tot_feat$featureID))
  rel_feat <- length(filter_result$to_keep) / tot_feat * 100
  rel_feat <- round(rel_feat, 1)
  tot_samp <- length(unique(met()$sampleID))
  rel_samp <- as.numeric(input$prevalence) / tot_samp * 100
  rel_samp <- round(rel_samp, 1)
  out <- gsub("N_KEEP", as.character(length(filter_result$to_keep)),
              ui_entry()$msg)
  out <- gsub("TOT_FEAT", as.character(tot_feat), out)
  out <- gsub("REL_FEAT", as.character(rel_feat), out)
  out <- gsub("REL_FEAT", as.character(rel_feat), out)
  out <- gsub('REPLACE2', as.character(input$asv_cutoff), out)
  out <- gsub("REPLACE3", as.character(input$prevalence), out)
  out <- gsub("TOT_SAMP", as.character(tot_samp), out)
  out <- gsub("REL_SAMP", as.character(rel_samp), out)
})


# giving preview on read and prevalence
prev_agg_plot <- eventReactive(input$submit_asv, {
  req(filter_result$to_keep)
  validate(need(input$asv_select_prompt == 'some',
                "Using all features"),
           need(!is.null(filter_result$to_keep), "No filtering performed"))

  scaleFUN <- function(x) sprintf("%.0f", x)

  if(input$cutoff_method == 'abs_count') {
    pdata <- wip() %>%
      group_by(featureID) %>%
      summarise(y = mean(read_count))
    ylab <- 'Mean Read count'
  }
  if(input$cutoff_method == 'percent_total') {
    pdata <- hist_datatot_pdata() %>%
      group_by(featureID) %>%
      summarise(y = mean(relab_datatot))
    ylab <- 'Mean Relative abundance\n(% of total reads)'
  }
  if(input$cutoff_method == 'percent_sample') {
    pdata <- hist_samptot_pdata() %>%
      group_by(featureID) %>%
      summarise(y = mean(relab_samptot))
    ylab <- 'Mean Relative abundance\n(% of sample)'
  }

  pdata <- pdata %>%
    left_join(hist_prev_pdata(), 'featureID') %>%
    mutate(colour = ifelse(featureID %in% filter_result$to_keep,
                           'to keep', 'to remove'))

  p <- ggplot(pdata, aes(x = prevalence, y = y, colour = colour)) +
    geom_point(aes(text=sprintf("featureID: %s", featureID)),
               alpha = 0.5, size = 2) +
    xlab('ASV prevalence (# of samples)') +
    ylab(ylab) +
    scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met()))) +
    scale_y_continuous(trans = 'log10') +
    theme_bw(12) +
    theme(legend.position = 'bottom')
  p
})

output$prev_agg_plot <- renderPlotly({
  ggplotly(prev_agg_plot()) %>%
    layout(legend = list(orientation = "h", x = -0.5, y = 1.2))
})

prev_read_plot <- eventReactive(input$submit_asv, {
  req(filter_result$to_keep)
  validate(need(input$asv_select_prompt == 'some',
                "Using all features"),
           need(!is.null(filter_result$to_keep), "No filtering performed"))
  scaleFUN <- function(x) sprintf("%.0f", x)

  if(input$cutoff_method == 'abs_count') {
    pdata <- wip() %>%
      rename(y = 'read_count')
    ylab <- 'Read count'
  }
  if(input$cutoff_method == 'percent_total') {
    pdata <- hist_datatot_pdata() %>%
      rename(y = 'relab_datatot')
    ylab <- 'Relative abundance\n(% of total reads)'
  }
  if(input$cutoff_method == 'percent_sample') {
    pdata <- hist_samptot_pdata() %>%
      rename(y = 'relab_samptot')
    ylab <- 'Mean Relative abundance\n(% of sample)'
  }

  pdata <- pdata %>%
    left_join(hist_prev_pdata(), 'featureID') %>%
    mutate(colour = ifelse(featureID %in% filter_result$to_keep,
                           'to keep', 'to remove'))

  p <- ggplot(pdata, aes(x = prevalence, y = y, colour = colour)) +
    geom_point(aes(text=sprintf("featureID: %s<br>sampleID: %s",
                                featureID, sampleID)),
               alpha = 0.5, size = 2) +
    xlab('ASV prevalence (# of samples)') +
    ylab(ylab) +
    scale_x_continuous(labels = scaleFUN, limits = c(0, nrow(met()))) +
    scale_y_continuous(trans = 'log10') +
    theme_bw(12) +
    theme(legend.position = 'bottom')
})

output$prev_read_plot <- renderPlotly({
  ggplotly(prev_read_plot()) %>%
    layout(legend = list(orientation = "h", x = -0.5, y = -1))
})

  # filter ASVs based on set cutoff/selection-----------------------------------
  wip_filtered <- reactiveVal()
withBusyIndicatorServer('submit_asv', 'filterfeat_ui_1', {
  observeEvent(input$submit_asv, {
    req(filter_result$to_keep)
    entry <- wip() %>%
      filter(featureID %in% filter_result$to_keep)
    wip_filtered(entry)
  })
})

  # secondary check for samples with no reads-----------------------------------
  sample_total <- reactive({
    wip_filtered() %>%
      group_by(sampleID) %>%
      summarise(sample_total = sum(read_count))
  })

  asv_total <- reactive({
    wip_filtered() %>%
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

  # identify empty asvs
  empty_asv <- reactive({
    out <- asv_total() %>%
      filter(asv_total == 0)
    out$featureID
  })

  output$secondary_filter_ui <- renderUI({
    req(filter_result$asv_remove)
    validate(need(!is.null(filter_result$to_keep), "No filtering performed"))
    wellPanel(
      tags$b(asv_cutoff_msg()),
      br(),
      tags$b(sprintf("Removing %s Features", length(filter_result$asv_remove))),
      br(),br(),
      tags$b(sprintf("%s samples contained 0 reads after ASV filtering. The following samples have been removed:", length(empty_sample()))),
      br(),
      HTML(paste(empty_sample(), collapse = '<br/>')),
      br(),
      tags$b(sprintf("%s asvs contained 0 reads in all samples after ASV filtering. The following asvs have been removed:", length(empty_asv()))),
      br(),
      HTML(paste(empty_asv(), collapse = '<br/>'))
    )
  })

  # update asv_filtered
  asv_filtered <- reactive({
    withBusyIndicatorServer('submit_asv', "setup_ui_1", {
      if(secondary_check_sample()) {
        wip_filtered() %>%
          filter(!sampleID %in% empty_sample())
      }
      else if(secondary_check_asv()) {
        wip_filtered() %>%
          filter(!featureID %in% empty_asv())
      }
      else {
        wip_filtered()
      }
    })

  })

  # update met
  met_filtered <- eventReactive(input$submit_asv, {
    if(secondary_check_sample()) {
      met() %>%
        filter(!sampleID %in% empty_sample())
    }
    else {
      met()
    }
  })

  output$preview_asv <- DT::renderDataTable(server = FALSE, {
    validate(need(!is.null(filter_result$to_keep), "No filtering performed"))
    out <- tax() %>%
      right_join(asv_filtered() %>%
                   spread(sampleID, read_count), 'featureID') %>%
      select(-Taxon, -sequence) %>%
      arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)

    DT::datatable(out, extensions = 'Buttons',
                  rownames = FALSE,
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy','csv'),
                    fixedColumns=list(leftColumns = 2)
                    ))
  })

  # add filtered tables to bridge reactive-------------------------------------
  cross_mod <- reactiveValues()
  observe({
    cross_mod$filtered <- list(met = met_filtered(),
                               asv = asv_filtered() %>%
                                 spread(sampleID, read_count),
                               tax = tax() %>%
                                 filter(featureID %in% asv_filtered()$featureID))
  })
  observe({
    cross_mod$params <- list(
      filter_submit = input$submit_asv,
      asv_select_prompt = input$asv_select_prompt
    )
  })
  observe({
    cross_mod$params$asv_filter_options <- input$asv_filter_options
    cross_mod$params$cutoff_method <- input$cutoff_method
    cross_mod$params$asv_cutoff <- input$asv_cutoff
    cross_mod$params$prevalence <- input$prevalence
    cross_mod$params$asv_cutoff_msg <- asv_cutoff_msg()
    cross_mod$params$to_keep <- filter_result$to_keep
    cross_mod$params$asv_remove <- filter_result$asv_remove
    cross_mod$params$prev_agg_plot <- prev_agg_plot()
    cross_mod$params$prev_read_plot <- prev_read_plot()
    cross_mod$params$empty_sample <- empty_sample()
    cross_mod$params$empty_asv <- empty_asv()
  })
  return(cross_mod)
}

## To be copied in the UI
# mod_filterfeat_ui("filterfeat_ui_1")

## To be copied in the server
# callModule(mod_filterfeat_server, "filterfeat_ui_1")

