# Module UI

#' @title   mod_ov_pca_ui and mod_ov_pca_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ov_pca
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import htmlwidgets
mod_ov_pca_ui <- function(id){
  ns <- NS(id)
  tagList(
    # wellPanel(width = 12, h3('Sub check'), br(), verbatimTextOutput(ns('check'))),
    fluidRow(
      h1('Principle Component Analysis'),
      column(
        width = 3,
        # PCA controls--------------------------------------------------------
        wellPanel(
          tags$div(style = "text-align: center", tags$b('PCA Parameters')),
          uiOutput(ns('pca_scale_ui')),
          actionButton(ns('pca_calculate'), "Calculate")
        )
      ),
      column(
        width = 9,
        p("PCA is a non-supervised multivariate analysis that provides a good 'first look' at microbiome data. Since feature values (even when transformed) can span multiple order of magnitudes, it is recommended that feature values are scaled for PCA so all features are weighted equally."),
        p("1) Unit variance scaling mean centres the value and divides by the standard deviation of the feature. After unit variance scaling, all features have the same mean and standard deviation. (x - mean(x)) / sd(x))"), br(),
        p("Pareto scaling divides mean-centred values by the square root of the feature values. Pareto scaling diminishes the effects of features that exhibit large fold so the changes in features with different magnitudes are weighted more evenly. (x - mean(x)) / sqrt(x))"), br(),
        p("Vast scaling (or variable stability scaling) uses the ratio of the mean and the standard deviation to in order to prioritise features that are more stable, while placind lesser importance on features with greater relative standard devieation. ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))"),
        br(),
        p('Definitions obtained from', a("van den Berg et al., 2006", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1534033/"))
      ),
      hr()
    ), # end fluidRow
    hidden(div(
      id = ns('pca_summary_div'),
      fluidRow(
        h2('Summary of PCA'),
        DT::dataTableOutput(ns('summary_pca'))  %>%
          shinycssloaders::withSpinner()
      )
    )),
    hidden(div(
      id = ns('pca_body_div'),
      fluidRow(
        h2('PCA Plot'),

        wellPanel(
          tags$div(style = 'text_align: center', h4("Plot Parameters")),
          fluidRow(
            # Plot controls
            column(width = 3,
                   uiOutput(ns('xPC_ui')),
                   uiOutput(ns('yPC_ui'))),
            column(
              width = 3,
              checkboxInput(ns('show_loading'), "Show loadings", FALSE),
              checkboxInput(ns('load_arrow'), 'Show loading arrows', FALSE),
              checkboxInput(ns('show_ellipse'),"Show Ellipse", value = TRUE)
            ),

            conditionalPanel(
              condition = paste0("input['", ns('show_ellipse'), "'] == true"),
              column(
                width = 3,
                h4("Ellipse aesthetics"),
                selectInput(ns('pca_ell_type'), "Type of ellipse",
                            choices = c('t-distribution' = 't',
                                        'normal distribution' = 'norm',
                                        'Euclidean distance' = 'euclid'),
                            selected = 'norm')
              ),
              column(
                width = 3,
                selectInput(ns('pca_ell_line'), "Linetype",
                            choices = c('solid','dashed','longdash','dotdash'),
                            selected = 'solid'),
                numericInput(ns('pca_ell_ci'), "Confidence Interval",
                             min = 0.1, max = 0.99, value = 0.95,
                             step = 0.05)
              )
            ) # end conditionalPanel ellipses parameters
          ), # end fluidRow inside wellPanel
          fluidRow(
            column(
              width = 3,
              # score point aesthetics
              h4("Score points aesthetics"),
              uiOutput(ns('score_pt_colour_ui')),
              uiOutput(ns('score_pt_shape_ui')),
              sliderInput(ns('score_pt_size'), 'Point size:',
                         min = 0.1, max = 5, value = 3, step = 0.5,
                         ticks = FALSE),
              sliderInput(ns('score_pt_alpha'), 'Point transparency:',
                          min = 0.1, max = 1, value = 1, step = 0.1)
            ),
            # score label aesthetics
            column(
              width = 3,
              h4("Score labels aesthetics"),
              uiOutput(ns('score_label_ui')),
              uiOutput(ns('score_lab_colour_ui')),
              sliderInput(ns('score_lab_size'), 'Label size:',
                         min = 0.1, max = 5, value = 3, step = 0.5),
              sliderInput(ns('score_lab_alpha'), 'Label transparency:',
                          min = 0.1, max = 1, value = 1, step = 0.1)
             ),
             conditionalPanel(
               condition = paste0("input['", ns('show_loading'), "'] == true"),
               column(
                 width = 3,
                 # loading point aesthetics
                 h4('Loading points aesthetics'),
                 uiOutput(ns('load_pt_colour_ui')),
                 uiOutput(ns('load_pt_shape_ui')),
                 sliderInput(ns('load_pt_size'), 'Point size:',
                             min = 0.1, max = 5, value = 3, step = 0.5),
                 sliderInput(ns('load_pt_alpha'), 'Point transparency:',
                             min = 0.1, max = 1, value = 1, step = 0.1)
               ),
               # loading label aesthetics
               column(
                 width = 3,
                 h4('Loading labels aethetics'),
                 uiOutput(ns('load_label_ui')),
                 uiOutput(ns('load_lab_colour_ui')),
                 sliderInput(ns('load_lab_size'), 'Label size:',
                             min = 0.1, max = 5, value = 3, step = 0.5),
                 sliderInput(ns('load_lab_alpha'), 'Label transparency:',
                             min = 0.1, max = 1, value = 1, step = 0.1)
               )
             ) # end conditionalPanel for loading aesthethics
          ) # end fluidRow inside wellPanel
        ) # end wellPanel
      ) # end fluidRow
    )), # end pca hidden div
    fluidRow(
      column(
        width = 1, style = 'padding:0px;',
        mod_download_ui(ns("download_pca"))
      ),
      column(
        width = 11, style = 'padding:0px;',
        shinyjqui::jqui_resizable(
          plotlyOutput(ns('plot_pca'), width = '100%', height = 'auto')
        )
      )
    ), # end fluidRow
    fluidRow(
      DT::dataTableOutput(ns('table_selected'))
    )
  ) # end taglist
}

# Module Server

#' @rdname mod_ov_pca
#' @export
#' @keywords internal

mod_ov_pca_server <- function(input, output, session, bridge){
  ns <- session$ns

  # toggle div for input controls-----------------------------------------------
  observeEvent(input$pca_calculate, {
    show('pca_summary_div')
    show('pca_body_div')
  })

  ## render controls - PCA------------------------------------------------------
  output$pca_scale_ui <- renderUI({
    req(bridge$asv_transform)
    if(any(bridge$asv_transform < 0)) {
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
  ### choose PCs to plot
  output$xPC_ui <- renderUI({
    numericInput(ns('xPC'), 'x-axis PC', value = 1,
                 min = 1, max = nrow(d_pcx()$x), step = 1)
  })
  output$yPC_ui <- renderUI({
    numericInput(ns('yPC'), 'y-axis PC', value = 2,
                 min = 1, max = nrow(d_pcx()$x), step = 1)
  })
  ### score point aesthetics
  output$score_pt_colour_ui <- renderUI({
    selectInput(ns('score_pt_colour'), 'Point colour:',
                choices = c('none', colnames(bridge$filtered$met)), selected = 'none')
  })
  output$score_pt_shape_ui <- renderUI({
    selectInput(ns('score_pt_shape'), 'Point shape:',
                choices = c('none', colnames(bridge$filtered$met)), selected = 'none')
  })

  ### score label aethetics
  output$score_label_ui <- renderUI({
    selectInput(ns('score_label_by'), 'Label scores by:',
                choices = c('none', colnames(bridge$filtered$met)), selected = 'none')
  })
  output$score_lab_colour_ui <- renderUI({
    selectInput(ns('score_lab_colour'), 'Label colour:',
                choices = c('none', colnames(bridge$filtered$met)), selected = 'none')
  })

  ### loading points aesthetics
  output$load_pt_colour_ui <- renderUI({
    selectInput(ns('load_pt_colour'), 'Point colour:',
                choices = c('none', colnames(bridge$filtered$tax)), selected = 'none')
  })
  output$load_pt_shape_ui <- renderUI({
    selectInput(ns('load_pt_shape'), 'Point shape:',
                choices = c('none', colnames(bridge$filtered$tax)), selected = 'none')
  })
  ### loading labels aesthetics
  output$load_label_ui <- renderUI({
    selectInput(ns('load_label_by'), 'Label loadings by:',
                choices = c('none', colnames(bridge$filtered$tax)), selected = 'none')
  })
  output$load_lab_colour_ui <- renderUI({
    selectInput(ns('load_lab_colour'), 'Label colour:',
                choices = c('none', colnames(bridge$filtered$tax)), selected = 'none')
  })
  output$load_lab_shape_ui <- renderUI({
    selectInput(ns('load_lab_shape'), 'Label shape:',
                choices = c('none', colnames(bridge$filtered$tax)), selected = 'none')
  })

  # output$check <- renderPrint({
  #
  # })
  # calculate pca---------------------------------------------------------------

  # centre and scale
  asv_scale <- eventReactive(input$pca_calculate, {
    req(input$pca_scale)
    if(input$pca_scale == 'UV') {
      apply(bridge$asv_transform, 1, function(x) (x - mean(x)) / sd(x))
    }
    else if(input$pca_scale == 'pareto') {
      apply(bridge$asv_transform, 1, function(x) (x - mean(x)) / sqrt(x))
    }
    else if(input$pca_scale == 'vast') {
      apply(bridge$asv_transform, 1, function(x) ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))
    }
    else {
      t(bridge$asv_transform)
    }
  })

  # performing pca
  d_pcx <- eventReactive(input$pca_calculate, {
    ## samples in rows
    ## centring and scaling done outside of prcomp
    prcomp(asv_scale(), center = FALSE, scale. = FALSE)
  })

  xPC <- reactiveVal('PC1')
  yPC <- reactiveVal('PC2')
  observeEvent(input$xPC, {
    xPC(paste0('PC', input$xPC))
  })

  observeEvent(input$yPC, {
    yPC(paste0('PC', input$yPC))
  })


  # scale rotations and score to be on same axis as score
  ##  by st. dev of given PC ^ n_samples

  lam <- reactive({
    d_pcx()$sdev * sqrt(nrow(d_pcx()$x))
  })

  score_data <- reactive({

    out <- t(apply(d_pcx()$x, 1, function(x) x / lam()))
    # out <- data.frame(d_pcx()$x)
    out <- as.data.frame(out) %>%
      mutate(sampleID = rownames(d_pcx()$x)) %>%
      select(sampleID, xPC(), yPC()) %>%
      left_join(bridge$filtered$met, 'sampleID')
    out <- out[,c(xPC(), yPC(), colnames(bridge$filtered$met))]
    out
  })

  load_data <- reactive({
    # out <- t(apply(d_pcx()$rotation, 1, function(x) x * lam()))
    out <- as.data.frame(d_pcx()$rotation) %>%
      mutate(featureID = rownames(d_pcx()$rotation)) %>%
      select('featureID', xPC(), yPC()) %>%
      left_join(bridge$filtered$tax, 'featureID')
    out <- out[, c(xPC(), yPC(), colnames(bridge$filtered$tax))]
    out
  })


  # summary of pca
  pcx_summary <- reactive({
    summary_pcx <- summary(d_pcx())
    summary_df <- unclass(summary_pcx)[['importance']]

    # check variation explained by each PC
    variance_pc <- summary_pcx$sdev**2
    variance_pc <- variance_pc/sum(variance_pc)*100

    summary_wip <- rbind(summary_df, variance_pc)
    rownames(summary_wip)[4] <- 'Variance Explained'
    summary_wip
  })

  output$summary_pca <- DT::renderDataTable(server = FALSE, {
    DT::datatable(pcx_summary(), extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(pcx_summary()), digits = 3)
  })

  # pca plot parameters
  ## initiate parameters as objects
  score_pt_colour <- reactiveVal('black')
  score_pt_shape <- reactiveVal(1)
  score_label_by <- reactiveVal(NULL)
  score_label <- reactiveVal(FALSE)
  score_lab_colour <- reactiveVal('black')

  ## score point parameters
  observeEvent(input$score_pt_colour, {
    if(input$score_pt_colour == 'none') score_pt_colour('black')
    else score_pt_colour(input$score_pt_colour)
  })

  observeEvent(input$score_pt_shape, {
    if(input$score_pt_shape == 'none') score_pt_colour(1)
    else score_pt_colour(input$score_pt_shape)
  })

  ## score label parameters
  observeEvent(input$score_label_by, {
    if(input$score_label_by == 'none') score_label_by(NULL)
    else  score_label_by(input$score_label_by)
  })

  observeEvent(input$score_label_by, {
    if(input$score_label_by == 'none') FALSE
    else score_label <- TRUE
  })

  observeEvent(input$score_lab_colour, {
    if(input$score_lab_colour == 'none') score_lab_colour('black')
    else score_lab_colour(input$score_lab_colour)
  })

  ## loading point parameters
  # initiate parameters
  load_pt_colour <- reactiveVal('darkred')
  load_pt_shape <- reactiveVal(2)
  load_label_by <- reactiveVal(NULL)
  show_load_label <- reactiveVal(FALSE)
  load_lab_colour <- reactiveVal('darkred')
  # show_loading <- reactiveVal(FALSE)
  # observeEvent(input$show_loading, {
  #   show_loading(input$show_loading)
  # })
  #
  observeEvent(input$load_pt_colour, {
    if(input$load_pt_colour == 'none') load_pt_colour('darkred')
    else load_pt_colour(input$load_pt_colour)
  })

   observeEvent(input$load_pt_shape, {
    if(input$load_pt_shape == 'none') load_pt_shape(2)
    else load_pt_shape(input$load_pt_shape)
  })

  ## loading label parameters
  observeEvent(input$load_label_by, {
    if(input$load_label_by == 'none') load_label_by(NULL)
    else load_label_by(input$load_label_by)
  })

  observeEvent(input$load_label_by, {
    if(input$load_label_by == 'none') show_load_label(FALSE)
    else show_load_label <- show_load_label(TRUE)
  })

  observeEvent(input$load_lab_colour, {
    if(input$load_lab_colour == 'none') load_lab_colour('darkred')
    else observe(input$load_lab_colour)
  })

  p_biplot <- reactive({
    req(input$xPC, input$yPC)
    p_biplot <- cms_biplot(
      score_data(), load_data(),
      xPC = input$xPC, yPC = input$yPC,
      # ellipse
      frame = input$show_ellipse, frame.type = input$pca_ell_type,
      frame.line = input$pca_ell_line, frame.colour = score_pt_colour(),
      frame.level = input$pca_ell_ci,
      # score point
      colour = score_pt_colour(), shape = score_pt_shape(),
      size = input$score_pt_size, alpha = input$score_pt_alpha,
      # score label
      label = score_label(), label.label = score_label_by(),
      label.colour = score_lab_colour(), label.size = input$score_lab_size,
      label.alpha = input$score_lab_alpha, label.repel = FALSE,
      # loading point
      loadings = input$show_loading, loadings.colour = load_pt_colour(),
      loadings.shape = load_pt_shape(), loadings.arrow = input$load_arrow,
      loadings.alpha = input$load_pt_alpha, loadings.size = input$load_pt_size,
      # loading label
      loadings.label = show_load_label(), loadings.label.label = load_label_by(),
      loadings.label.colour = load_lab_colour(), loadings.label.repel = FALSE,
      loadings.label.size = input$load_lab_size, loadings.label.alpha = input$load_lab_alpha
    )

    p_biplot <- p_biplot +
      theme_bw(12) +
      xlab(sprintf("PC%s (%s%%)",
                   input$xPC,
                   round(pcx_summary()['Variance Explained', input$xPC]), 2)) +
      ylab(sprintf("PC%s (%s%%)",
                   input$yPC,
                   round(pcx_summary()['Variance Explained', input$yPC]), 2))

    p_biplot
  })

  output$plot_pca <- renderPlotly({
    ggplotly(p_biplot(), source='plotly_pca') %>%
      layout(dragmode = 'select')
  })

  # download data
  for_download <- reactiveValues()
  observe({
    req(input$pca_scale, input$pca_calculate)
    for_download$figure <- p_biplot()
    for_download$fig_data <- plyr::rbind.fill(score_data(), load_data())
  })

  callModule(mod_download_server, "download_pca", bridge = for_download, 'pca')

  # metadata of selected samples------------------------------------------------
  selected_samp <- reactiveVal()

  # store selected feature
  observeEvent(event_data("plotly_selected", source="plotly_pca"),
               suspended = input$pca_calculate == 0, {
    curr_selected<- event_data("plotly_selected", source='plotly_pca')$customdata
    updated_samp <- unique(c(selected_samp(), curr_selected))
    selected_samp(updated_samp)
  })

  # clear selection
  observeEvent(event_data("plotly_deselect", source="plotly_pca"),
               suspended = input$pca_calculate == 0, {
    selected_samp(NULL)
  })

  output$table_selected <- DT::renderDataTable(server=FALSE, {
    req(input$pca_calculate, selected_samp)
    validate(need(!is.null(selected_samp()), "Click and drag (with rectangle or lasso tool) to select points on pca plot to show sample metadata (double-click to clear)"))

    out <- bridge$filtered$met %>% filter(sampleID %in% selected_samp())
    DT::datatable(out,
                  extensions = 'Buttons', filter='top', rownames = FALSE,
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  # initiate return list--------------------------------------------------------
  cross_module <- reactiveValues()
  observe({
    cross_module$output <- list(
      pca_scale = input$pca_scale,
      pca_summary = pcx_summary(),
      p_pca = p_biplot()
    )
  })

  return(cross_module)
}

## To be copied in the UI
# mod_ov_pca_ui("ov_pca_ui_1")

## To be copied in the server
# callModule(mod_ov_pca_server, "ov_pca_ui_1")

