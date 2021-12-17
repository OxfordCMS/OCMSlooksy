#' prop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import propr
mod_prop_ui <- function(id){
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
      navlistPanel(
        '',
        id = 'prop_menu',
        well=FALSE,
        widths=c(3,9),
        # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
        # info tab body-----------------------------------------------------
        tabPanel(
          "Module info",
          value = 'info_tab_prop',
          icon = icon('info-circle'),
          fluidRow(
            br(),br(),
            column(
              width = 12,
              div(
                h1("Feature Comparison"),
                p("Module overview:"),
                tags$ul(
                  tags$li(tags$b("Aggregate Features:"), "Select the taxonomic level at which you want to examine the microbiome profiles"),
                  tags$li(tags$b("Filter Features:"), "Filter aggregated features based on feature abundance and prevalence"),
                  tags$li(tags$b("Pairwise Feature Comparison:"), "Evaluates correlation of taxa in a pairwise manner using a proportionality index, \u03C1. This package employs the ", code("propr"), " package to assess proportionality.")
                )
              )
            )
          )
        ), # end tabPanel
        # aggregate tab body------------------------------------------------
        tabPanel(
          'Aggregate Features',
          value = 'agg_prop_tab',
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
          value = "filter_prop_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              div(id=ns('filtfeat_mod_div'),
                mod_filterfeat_ui(ns("filterfeat_ui_1"))
              )
            )
          )
        ),
        # Proportionality body----------------------------------------------
        tabPanel(
          "Pairwise Feature Comparison",
          value = "prop_tab",
          fluidRow(
            br(),br(),
            column(
              width = 12,
              h1('Pairwise Feature Comparison'),
              tags$div(
                'One approach to analyzing the microbiome is to look at features in a pairwise fashion, and evaluate their correlation with one another. Given the compositional nature of 16S gene sequencing, proportionality is used instead of correlation to evaluate the association between two features. The proportionality index, rho (\u03C1), ranges from -1 to 1, where the more extreme values indicate stronger associations. The R package', a(code('propr'), href="https://cran.r-project.org/web/packages/propr/"), 'is used to calculate proportionality.', br(),
                "Proportionality is calculated on CLR-transformed counts. Please note that the CLR-transformation applied does not impute zeroes. Rather, all zero counts are replaced with 1, and then CLR-transformed. This is different than CLR-transformation performed by ", code('ALDEx2'), "applied elsewhere in the app, which imputes zero values from Monte-Carlo sampling of a Dirichlet distribution. Please take caution that spurious correlations may occur with zero counts if your dataset is sparse (high proportion of zeros)" ,
                br()
              ),
              br(),
              column(
                width = 8,
                verbatimTextOutput(ns('prop_fdr_summary')) %>%
                  shinycssloaders::withSpinner()
              ),
              column(
                width = 4,
                tags$div(
                  "When comparing features in a pairwise manner, caution must be taken to account for the accumulation false positives. To account the this, the false discovery rate (FDR) is estimated for varying rho values. It is recommended that you choose the largest rho cutoff that limits the FDR to below 0.05. Empirically, a good starting place for 16S data is rho \u2265 |0.6|.")
              )
            ) # end column 12
          ), # end fulidRow
          fluidRow(
            width = 12,
            column(
              width = 4,
              h3("Summary of Rho Values"),
              radioButtons(ns('rho_filter'), NULL,
                           choices = c('Show all pairs' = 'all',
                                       'Filter by rho' = 'filter'),
                           inline = TRUE, selected = 'all'),
              hidden(div(
                id = ns('rho_slider_div'),
                tags$style(HTML(".irs-bar {background: none; border: none}")),
                tags$style(HTML("irs-grid-pol.small {height: 0px;}")),
                tags$style(HTML(".irs-grid-text { font-size: 11pt; }")),
                sliderInput(ns('rho_cutoff'), "Rho cutoff",
                            min = -1, max = 1,
                            value = c(-0.6, 0.6), step = 0.01),
                radioButtons(
                  ns("rho_operator"), "Keep Rho values",
                  choices = c(
                    'inside range (weak associations)'='inside',
                    'outside range (strong associations)' = 'outside'),
                  selected = 'outside')
              )), # end hidden div rho_slider_div
              actionButton(ns('show_rho'), "Show feature pairs")
            ),
            column(
              width = 4,
              br(),
              plotOutput(ns('rho_range'), height = "150px") %>%
                shinycssloaders::withSpinner()
            ),
            column(
              width = 4, tags$div(
                br(),
                "Pairwise feature comparision results in a matrix of rho values, to show the proportionaity of a given feature with all other features. The rho values are summarised in the histogram in the centre. You can the relationship between all feature pairs, but in cases where there are many features, this may result in an unreasonable number of feature pairs to visualise. You can choose to filter rho values based a set of rho cutoffs. You can choose 'outside range' to examine stronger associations (more extreme values of rho), or the 'inside range' to examine the weaker associations. Click `show feature pairs` to update the plots."
              )
            )
          ), # end fluidRow 12
          fluidRow(
            column(
              width = 8,
              plotOutput(ns('rho_summary')) %>%
                shinycssloaders::withSpinner()
            ),
            column(
              width = 4,
              DT::dataTableOutput(ns('prop_summary')) %>%
                shinycssloaders::withSpinner()
            )
          ), # end fluidRow 12
          fluidRow(
            hidden(div(
              id = ns('prop_hmap_div'),
              column(
                width = 12,
                h3('Proportionality Matrix'),
                wellPanel(
                  tags$b("Heirchical Cluster Parameters"),
                  fluidRow(
                    column(
                      width = 3,
                      selectInput(
                        ns('hclust_method'), "Linkage method",
                        choices = c('complete','ward.D2','average','centroid'),
                        selected = 'ward.D2'),
                      selectInput(
                        ns('dist_method'), "Distance method",
                        choices = c('manhattan','euclidean','chisq',
                                    'binomial','manahalanobis','chord'),
                        selected = 'manhattan')),
                    column(
                      width = 3,
                      checkboxGroupInput(
                        ns('show_dendro'), 'Show dendrogram',
                        choices = c('x-axis' = 'show_dendro_x',
                                    'y-axis' = 'show_dendro_y'),
                        selected = c('show_dendro_x', 'show_dendro_y'))),
                    column(
                      width = 3,
                      uiOutput(ns("hmap_tax_label_ui"))
                    )
                  ) # end fluid row
                ) # end well panel
              ), # end column 12
              column(
                width = 12,
                column(
                  width = 1, style = 'padding:0px;',
                  mod_download_ui(ns("download_prop_hmap"))
                ),
                column(
                  width = 11, style = 'padding:0px;',
                  shinyjqui::jqui_resizable(
                    plotlyOutput(ns('hmap_prop'),
                                 width = '100%', height = 'auto'))
                )
              )) # end hidden div - prop_hmap_div
            ) # end column 12
          ), # end fluid row

          fluidRow(
            width = 12,
            hidden(div(
              id = ns('prop_vispair_div'),
              column(
                width = 12,
                h3('Visualizing Feature Pairs'),
                DT::dataTableOutput(ns('prop_table')),
                tabsetPanel(
                  type = 'tabs',
                  tabPanel(
                    "Feature Pairs",
                    column(
                     width = 1, style = 'padding:0px;',
                     mod_download_ui(ns("download_pairs"))
                    ),
                    column(
                     width = 11, style = 'padding:0px;',
                     shinyjqui::jqui_resizable(
                       plotlyOutput(ns("plot_pair"), width = '100%',
                                    height = 'auto'))
                     )
                  ), # end tabPanel
                  tabPanel(
                    "By Metadata",
                    column(
                      width = 12,
                      uiOutput(ns("meta_x_ui"))),
                    column(
                      width = 12,
                      column(
                       width = 1, style = 'padding:0px;',
                       mod_download_ui(ns("download_pairs_meta"))
                      ),
                      column(
                       width = 11, style = 'padding:0px',
                       shinyjqui::jqui_resizable(
                         plotlyOutput(ns("plot_meta"), width = '100%',
                                      height = 'auto')
                       )
                      )
                    )
                  ) # end tabPanel
                ) # end prop tabset
              ) # end column 12
            )) # end hidden div
          ) # end fluidRow 12
        ), # end tabPanel
        tabPanel(
          'Report',
          value = 'prop_report_tab',
          fluidRow(
            column(
              width = 12,
              mod_report_ui(ns("prop_report_ui"))
            )
          )
        ) # end tabPanel
      ) # end navlistpanel
    ) # end fluidPage
  ) # end taglist
}

#' prop Server Function
#'
#' @noRd
mod_prop_server <- function(input, output, session, improxy){
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
    toggleState(selector = "#prop_menu li a[data-value=filter_prop_tab]",
                condition = progress$complete_agg == 1)
  })

  observe({
    toggleState(selector = "#prop_menu li a[data-value=prop_tab]",
                condition = progress$complete_featfilt == 1)
  })

  observe({
    toggleState(selector = "#prop_menu li a[data-value=prop_report_tab]",
                condition = progress$complete_featfilt == 1)
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

  # feature proportionality-----------------------------------------------------
  # show/hide ui component

  observeEvent(input$rho_filter, {
    if(input$rho_filter == 'filter') {
      show('rho_slider_div')
    }
    else {
      hide('rho_slider_div')
    }
  })

  # show/hide fdr summary-------------------------------------------------------
  observeEvent(input$show_rho, {
    show('prop_hmap_div')
  })

  observeEvent(input$show_rho, {
    show('prop_vispair_div')
  })

  #generate UI------------------------------------------------------------------
  output$meta_x_ui <- renderUI({
    selectInput(ns('meta_x'), 'x-axis', choices = colnames(bridge$filtered$met),
                selected = 'sampleID')
  })

  output$hmap_tax_label_ui <- renderUI({
    choices <- c('featureID','Taxon', input[['aggregate_ui_1-aggregate_by']])
    selectInput(
      ns('hmap_tax_label'), 'Label taxa by:',
      choices = choices, selected = 'featureID')
  })


  # calculate rho---------------------------------------------------------------
  propr_obj <- reactive({
    # propr package uses propr S4 class to store info -- see propr manual

    count_mat <- bridge$filtered$asv %>%
      tibble::column_to_rownames("featureID")

    # features in columns
    # default setting for ivar is clr transform
    out <- propr::propr(t(count_mat), metric = 'rho')

    # calculate fdr at different cutoffs
    out <- propr::updateCutoffs(out, cutoff = seq(0.05, 0.95, 0.15))
    out
  })

  # showing fdr calculations----------------------------------------------------
  output$prop_fdr_summary <- renderPrint({
    propr_obj()@fdr
  })

  # identify pairs that satisfy filter------------------------------------------
  pairs_keep <- reactive({
    req(input$rho_filter)
    if(input$rho_filter == 'filter') {
      req(input$rho_cutoff, input$rho_operator)
      # keep inside range
      if(input$rho_operator == 'inside') {
        lesser <- propr_obj()["<=", max(input$rho_cutoff)]@pairs
        greater <- propr_obj()[">=", min(input$rho_cutoff)]@pairs
        out <- intersect(lesser, greater)
      }
      # keep outside range
      else {
        lesser <- propr_obj()["<=", min(input$rho_cutoff)]@pairs
        greater <- propr_obj()[">=", max(input$rho_cutoff)]@pairs
        out <- c(lesser, greater)
      }
    }
    # keep all (no filter)
    else if(input$rho_filter == 'all') {
      out <- propr_obj()["<=", 1]@pairs
    }
    out
  })

  rho_range <- reactive({
    req(input$rho_filter)
    rho_val <- propr:::proprPairs(propr_obj()@matrix)

    if(input$rho_filter != 'all') {
      req(input$rho_cutoff, input$rho_operator)
      p_initial <- ggplot(rho_val, aes(x = prop)) +
        geom_histogram(alpha=0.6, fill=NA, colour='black')

      hist_val <- ggplot_build(p_initial)

      if(input$rho_operator == 'inside') {
        pdata <- hist_val$data[[1]] %>%
          mutate(fill = ifelse(x <= max(input$rho_cutoff) &
                                 x >= min(input$rho_cutoff), TRUE, FALSE))

        p <- ggplot(pdata) +
          geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=y, fill=fill),
                       colour='black', alpha = 0.6)
      }
      if(input$rho_operator == 'outside') {
        pdata <- hist_val$data[[1]] %>%
          mutate(fill = ifelse(x >= max(input$rho_cutoff) |
                                 x <= min(input$rho_cutoff), TRUE, FALSE))

        p <- ggplot(pdata) +
          geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=y, fill = fill),
                       colour = 'black', alpha = 0.6)
      }

      p <- p +
        scale_fill_manual(values=c('white','cornflowerblue'),
                            guide='none') +
        labs(subtitle=sprintf('%s feature pairs meet threshold',
                              length(pairs_keep())))
    }

    if(input$rho_filter == 'all') {
      p <- ggplot(rho_val, aes(x = prop)) +
        geom_histogram(alpha=0.6, fill='cornflowerblue', colour='black') +
        labs(subtitle=sprintf('%s feature pairs meet threshold', length(pairs_keep())))
    }
    p +
      theme_classic(12) +
      xlab('rho') +
      ylab('Number of Feature Pairs')
  })

  output$rho_range <- renderPlot({
    rho_range()
  })

  # apply filters to propr object
  work_obj <- eventReactive(input$show_rho, {
    validate(
      need(pairs_keep() > 0, "No feature pairs meet threshold")
    )
    out <- propr_obj()
    out@pairs <- pairs_keep()
    out <- propr::simplify(out)
    out
  })

  # extract rho values
  rho_df <- eventReactive(input$show_rho, {
    propr:::proprPairs(work_obj()@matrix)
  })


  rho_histogram <- eventReactive(input$show_rho, {
    p <- ggplot(rho_df(), aes(x = prop)) +
      geom_histogram(alpha = 0.6, fill = 'cornflowerblue', colour = 'black') +
      theme_classic(14) +
      xlab('rho') +
      ylab('Number of Feature Pairs') +
      labs(subtitle=sprintf('%s feature pairs meet threshold\n%s feature pairs in filtered proportionality matrix', length(work_obj()@pairs), nrow(rho_df())))
    p
  })
  output$rho_summary <- renderPlot({
    rho_histogram()
  })

  prop_summary <- reactive({
    req(input$show_rho)
    tibble::enframe(summary(rho_df()$prop),
                    name = 'quartile',
                    value='rho') %>%
      mutate(rho = round(rho, digits=3))
  })

  output$prop_summary <- DT::renderDataTable(server = FALSE, {
    DT::datatable(prop_summary(), filter='none', rownames=FALSE,
                  extensions = 'Buttons',
                  options = list(dom = 'Brti', buttons = c('copy','csv'),
                                 paging=FALSE, searching=FALSE),
                  caption="Rho distribution in filtered pair-wise proportionality matrix")
  })

  # heatmap of subset-----------------------------------------------------------
  hmap_data <- reactive({
    out <- work_obj()@matrix
    convert <- data.frame(featureID = rownames(out))
    convert <- convert %>%
      left_join(bridge$filtered$tax %>%
                  select(featureID, Taxon,
                         .data[[input[['aggregate_ui_1-aggregate_by']]]]),
                'featureID')
    rownames(out) <- convert[, input$hmap_tax_label]
    colnames(out) <- convert[, input$hmap_tax_label]
    out
  })
  # parameterizing heat map object
  hmap <- reactive({
    if(is.null(input$show_dendro)) {
      show_value <- 'none'
    }
    else {
      show_value <- input$show_dendro
    }
    heatmaply::heatmapr(
      x = hmap_data(),
      distfun = vegan::vegdist,
      dist_method = input$dist_method,
      hclust_method = input$hclust_method,
      dendrogram = 'both',
      show_dendrogram = c('show_dendro_y' %in% show_value,
                          'show_dendro_x' %in% show_value),
      Colv = "Rowv",
      dsigits = 3,
      show_grid = TRUE
    )
  })

  hmaply_prop <- reactive({
    heatmaply::heatmaply(hmap(), node_type = 'heatmap',
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red",
                midpoint = 0,
                limits = c(-1, 1)),
              key.title = 'Rho')
  })
  # plot heat map
  output$hmap_prop <- renderPlotly({
    hmaply_prop()
  })

  # download data
  for_download1 <- reactiveValues()
  observe({
    req(input$show_rho)
    for_download1$figure <- hmaply_prop()
    for_download1$fig_data <- hmap_data() %>%
      as.data.frame() %>%
      mutate(featureID = rownames(hmap_data()))
  })

  callModule(mod_download_server, "download_prop_hmap", bridge = for_download1,
             'heatmap', dl_options = c('html','csv','pdf','zip'))

  # select ASV pairs from table
  output$prop_table <- DT::renderDataTable(server = FALSE, {
    DT::datatable(rho_df() %>%
                    mutate(prop = round(prop, digits = 3)),
                  rownames = FALSE,
                  colnames = c('Feature 1','Feature 2','Rho'),
                  options = list(searchHighlight = TRUE, scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')),
                  filter = 'top', extensions = 'Buttons',
                  selection = list(mode = "multiple", selected = 1))
  })

  # visualize selected ASV pairs------------------------------------------------
  selected <- reactive({
    req(input$prop_table_rows_selected)
    out <- rho_df()[input$prop_table_rows_selected,]
    out <- split(out, seq(nrow(out)))
    out
  })

  pair_pdata <- reactive({
    curr <- work_obj()@logratio
    out <- c()
    for(i in selected()) {
      curr_row <- c(i$feature1, i$feature2, round(i$prop, digits = 3))
      entry <- data.frame(pairID = paste(curr_row[1:2], collapse = "_"),
                          feature1 = curr_row[1],
                          feature2 = curr_row[2],
                          prop = curr_row[3],
                          x = curr[,curr_row[1]],
                          y = curr[,curr_row[2]])
      entry$panel <- sprintf("%s, rho= %s", entry$pairID, curr_row[3])
      out <- rbind(out, entry)
    }
    out
  })

  p_pair <- reactive({
    ggplot(pair_pdata(), aes(x = x, y = y)) +
      geom_point(aes(text=sprintf("x: %s\ny: %s", feature1, feature2))) +
      geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE) +
      facet_wrap(~panel, scales = 'free', ncol = 3) +
      theme_bw(12) +
      xlab('Feature 1') +
      ylab('Feature 2')
  })

  output$plot_pair <- renderPlotly({
    ggplotly(p_pair())
  })


  # download data
  for_download2 <- reactiveValues()
  observe({
    req(input$show_rho,
        input$prop_table_rows_selected)
    for_download2$figure <- p_pair()
    for_download2$fig_data <- pair_pdata()
  })

  callModule(mod_download_server, "download_pairs", bridge = for_download2, 'prop')

  # put metadata with selected features
  meta_pdata <- reactive({

    logratio <- work_obj()@logratio
    logratio$sampleID <- rownames(logratio)
    logratio <- logratio %>%
      gather('featureID', 'logratio', -sampleID) %>%
      inner_join(bridge$filtered$met, 'sampleID')

    pair_pdata() %>%
      distinct(pairID, panel, prop, feature1, feature2) %>%
      gather('variable','featureID', -pairID, -panel, -prop) %>%
      left_join(logratio, 'featureID')
  })

  add_line <- reactive({
    req(input$meta_x)
    meta_pdata() %>%
      group_by(pairID, .data[[input$meta_x]]) %>%
      summarise(npoint=n())
  })
  output$check <- renderPrint({

  })
  p_meta <- reactive({
    req(input$meta_x)
    p <- ggplot(meta_pdata(), aes(x = .data[[input$meta_x]], y = logratio))

    if(any(add_line()$npoint > 2)) {
      dodge_val <- apply(expand.grid(unique(meta_pdata()$featureID),
                                     unique(meta_pdata()[, input$meta_x])),
                         1, paste0, collapse='')
      dodge_val <- 1-(1/length(dodge_val))

      p <- p +
        geom_point(aes(colour = featureID,
                       group = interaction(featureID, .data[[input$meta_x]])),
                   position = position_jitterdodge(jitter.width = 0.1,
                                                   dodge.width=0.75,
                                                   seed = 1),
                   alpha = 0.8)
        # geom_boxplot(aes(colour = featureID,
        #                  group = interaction(featureID, .data[[input$meta_x]])),
        #              outlier.fill = NA, fill=NA, outlier.colour = NA)
    }
    else {
      p <- p +
        geom_point(aes(colour = featureID)) +
        geom_line(aes(colour = featureID, group = featureID))

    }
    p <- p +
      facet_wrap(~panel, scales = 'free', ncol = 3) +
      theme_bw(12) +
      theme(legend.position='none',
            axis.text.x = element_text(hjust=1, vjust=0, angle=-90)) +
      ylab('CLR(counts)')

    p
  })

  output$plot_meta <- renderPlotly({
    # if(any(add_line()$npoint > 2)) {
      ggplotly(p_meta())
        # layout(boxmode='group')
    # }
    # else {
    #   ggplotly(p_meta())
    # }
    #
  })

  # download data
  for_download3 <- reactiveValues()
  observe({
    req(input$show_rho,
        input$prop_table_rows_selected)
    for_download3$figure <- p_meta()
    for_download3$fig_data <- meta_pdata()
  })

  callModule(mod_download_server, "download_pairs_meta", bridge = for_download3, 'prop')

  observe({
    req(input$show_rho)

    for_report$params$prop_fdr_summary <- propr_obj()@fdr
    for_report$params$rho_cutoff <- input$rho_cutoff
    for_report$params$rho_filter <- input$rho_filter
    for_report$params$rho_operator <- input$rho_operator
    for_report$params$rho_range <- rho_range()
    for_report$params$rho_summary <- rho_histogram()
    for_report$params$prop_summary <- prop_summary()
    for_report$params$prop_hclust_method <- input$hclust_method
    for_report$params$prop_dist_method <- input$dist_method
    for_report$params$hmap_tax_label <- input$hmap_tax_label
    for_report$params$hmap_prop <- hmaply_prop()
    for_report$params$prop_table <- rho_df()
    for_report$params$pairs_to_vis <- selected()
    for_report$params$prop_meta_x <- input$meta_x
    for_report$params$p_pair <- p_pair()
    for_report$params$prop_plot_meta <- p_meta()
  })

  # build report
  callModule(mod_report_server, "prop_report_ui", bridge = for_report,
             template = "feat_report",
             file_name = "feat_report")
}

## To be copied in the UI
# mod_prop_ui("prop_ui_1")

## To be copied in the server
# callModule(mod_prop_server, "prop_ui_1")

