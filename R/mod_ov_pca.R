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
    # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    
    h1('Principle Component Analysis'),
    tags$div("PCA is a non-supervised multivariate analysis that provides a good 'first look' at microbiome data. Since feature values (even when transformed) can span multiple order of magnitudes, it is recommended that feature values are scaled for PCA so all features are weighted equally."),
    p("1) Unit variance scaling mean centres the value and divides by the standard deviation of the feature. After unit variance scaling, all features have the same mean and standard deviation. (x - mean(x)) / sd(x))"), br(), 
    p("Pareto scaling divides mean-centred values by the square root of the feature values. Pareto scaling diminishes the effects of features that exhibit large fold so the changes in features with different magnitudes are weighted more evenly. (x - mean(x)) / sqrt(x))"), br(),
    p("Vast scaling (or variable stability scaling) uses the ratio of the mean and the standard deviation to in order to prioritise features that are more stable, while placind lesser importance on features with greater relative standard devieation. ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))"),
    br(),
    p('Definitions obtained from', a("van den Berg et al., 2006", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1534033/")),
    
    hidden(div(
      id = ns('pca_summary_div'), 
      h2('Summary of PCA'),
      DT::dataTableOutput(ns('summary_pca'))  %>%
        shinycssloaders::withSpinner()
    )),
    
    hidden(div(
      id = ns('pca_body_div'),
      h2('PCA Plot'),
      wellPanel(
       tags$div(style = 'text_align: center', h3("Plot Parameters")),
       fluidRow(
         # Plot controls
         column(width = 3,
                uiOutput(ns('xPC_ui')),
                uiOutput(ns('yPC_ui'))),
         column(
           width = 3,
           checkboxInput(ns('show_loading'), "Show loadings", TRUE),
           checkboxInput(ns('load_arrow'), 'Show loading arrows', TRUE),
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
                          step = 0.05))
          ),
       ),
       fluidRow(
         conditionalPanel(
           condition = paste0("input['", ns('show_loading'), "'] == true"),
           column(width = 3,
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
           column(width = 3,
                  h4('Loading labels aethetics'),
                  uiOutput(ns('load_label_ui')),
                  uiOutput(ns('load_lab_colour_ui')),
                  sliderInput(ns('load_lab_size'), 'Label size:',
                              min = 0.1, max = 5, value = 3, step = 0.5),
                  sliderInput(ns('load_lab_alpha'), 'Label transparency:',
                              min = 0.1, max = 1, value = 1, step = 0.1)
           )
         ),
         column(width = 3,
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
         column(width = 3,
                h4("Score labels aesthetics"),
                uiOutput(ns('score_label_ui')),
                uiOutput(ns('score_lab_colour_ui')),
                sliderInput(ns('score_lab_size'), 'Label size:',
                            min = 0.1, max = 5, value = 3, step = 0.5),
                sliderInput(ns('score_lab_alpha'), 'Label transparency:',
                            min = 0.1, max = 1, value = 1, step = 0.1)
         )
      ))
    )),
    column(
      width = 12,
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
    )
  )
}
    
# Module Server
    
#' @rdname mod_ov_pca
#' @export
#' @keywords internal
    
mod_ov_pca_server <- function(input, output, session, bridge){
  ns <- session$ns
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(pca_calculate(), {
    show('pca_summary_div')
    show('pca_body_div')
  })
  
  ## render controls - PCA------------------------------------------------------
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
  
  
  # unpack data from parent module----------------------------------------------
  # unpack pca inputs
  pca_scale <- reactive(bridge$pca_input$pca_scale)
  pca_calculate <- reactive(bridge$pca_input$pca_calculate)
  
  # output$check <- renderPrint({
  # 
  # })
  # calculate pca---------------------------------------------------------------

  # centre and scale
  asv_scale <- eventReactive(pca_calculate(), {
    req(pca_scale())
    if(pca_scale() == 'UV') {
      apply(bridge$asv_transform, 1, function(x) (x - mean(x)) / sd(x))
    }
    else if(pca_scale() == 'pareto') {
      apply(bridge$asv_transform, 1, function(x) (x - mean(x)) / sqrt(x))
    }
    else if(pca_scale() == 'vast') {
      apply(bridge$asv_transform, 1, function(x) ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))
    }
    else {
      t(bridge$asv_transform)
    }
  })

  # performing pca
  d_pcx <- reactive({
    ## samples in rows
    ## centring and scaling done outside of prcomp
    prcomp(asv_scale(), center = FALSE, scale. = FALSE)
  })

  xPC <- reactive(paste0('PC', input$xPC))
  yPC <- reactive(paste0('PC', input$yPC))

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

  output$summary_pca <- DT::renderDataTable({
    DT::datatable(pcx_summary(), extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(pcx_summary()), digits = 3)
  })

  # pca plot parameters
  ## initiate parameters as objects
  load_pt_colour <- NULL
  load_pt_shape <- NULL
  load_pt_size <- NULL
  load_pt_alpha <- NULL
  load_arrow <- FALSE
  show_load_label <- FALSE
  load_lab_colour <- NULL
  load_lab_size <- NULL
  load_lab_alpha <- NULL

  ## score point parameters
  score_pt_colour <- eventReactive(input$score_pt_colour, {
    if(input$score_pt_colour == 'none') 'black'
    else input$score_pt_colour
  })

  score_pt_shape <- eventReactive(input$score_pt_shape, {
    if(input$score_pt_shape == 'none') 1
    else input$score_pt_shape
  })

  score_pt_size <- reactive(input$score_pt_size)
  score_pt_alpha <- reactive(input$score_pt_alpha)

  ## score label parameters
  score_label_by <- eventReactive(input$score_label_by, {
    if(input$score_label_by == 'none') NULL
    else  input$score_label_by
  })

  score_label <- eventReactive(input$score_label_by, {
    if(input$score_label_by == 'none') FALSE
    else score_label <- TRUE
  })

  score_lab_colour <- eventReactive(input$score_lab_colour, {
    if(input$score_lab_colour == 'none') 'black'
    else input$score_lab_colour
  })


  score_lab_size <- reactive(input$score_lab_size)
  score_lab_alpha <- reactive(input$score_lab_alpha)

  ## loading point parameters
  load_pt_colour <- eventReactive(input$load_pt_colour, {
    req(input$show_loading)
    if(input$load_pt_colour == 'none') 'darkred'
    else input$load_pt_colour
  })

  load_pt_shape <- eventReactive(input$load_pt_shape, {
    if(input$load_pt_shape == 'none') 2
    else input$load_pt_shape
  })

  load_pt_size <- reactive({
    input$load_pt_size
  })
  load_pt_alpha <- reactive({
    input$load_pt_alpha
  })
  load_arrow <- reactive({
    input$load_arrow
  })

  ## loading label parameters
  load_label_by <- eventReactive(input$load_label_by, {
    if(input$load_label_by == 'none') NULL
    else input$load_label_by
  })

  show_load_label <- eventReactive(input$load_label_by, {
    if(input$load_label_by == 'none') FALSE
    else show_load_label <- TRUE
  })

  load_lab_colour <- eventReactive(input$load_lab_colour, {
    if(input$load_lab_colour == 'none') 'darkred'
    else input$load_lab_colour
  })

  load_lab_size <- reactive({
    input$load_lab_size
  })

  load_lab_alpha <- reactive({
    input$load_lab_alpha
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
      size = score_pt_size(), alpha = score_pt_alpha(),
      # score label
      label = score_label(), label.label = score_label_by(),
      label.colour = score_lab_colour(), label.size = score_lab_size(),
      label.alpha = score_lab_alpha(), label.repel = FALSE,
      # loading point
      loadings = input$show_loading, loadings.colour = load_pt_colour(),
      loadings.shape = load_pt_shape(), loadings.arrow = load_arrow(),
      loadings.alpha = load_pt_alpha(), loadings.size = load_pt_size(),
      # loading label
      loadings.label = show_load_label(), loadings.label.label = load_label_by(),
      loadings.label.colour = load_lab_colour(), loadings.label.repel = FALSE,
      loadings.label.size = load_lab_size(), loadings.label.alpha = load_lab_alpha()
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
    ggplotly(p_biplot())
  })
  
  # download data
  for_download <- reactiveValues()
  observe({
    req(bridge$pca_input$pca_scale, bridge$pca_input$pca_calculate)
    for_download$figure <- p_biplot()
    for_download$fig_data <- plyr::rbind.fill(score_data(), load_data())
  })
  
  callModule(mod_download_server, "download_pca", bridge = for_download, 'pca')

}
    
## To be copied in the UI
# mod_ov_pca_ui("ov_pca_ui_1")
    
## To be copied in the server
# callModule(mod_ov_pca_server, "ov_pca_ui_1")
 
