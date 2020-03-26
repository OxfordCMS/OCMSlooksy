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
mod_ov_pca_ui <- function(id){
  ns <- NS(id)
  tagList(
    # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    
    h1('Principle Component Analysis'),
    tags$div("PCA is a non-supervised multivariate analysis that provides a good 'first look' at microbiome data."),
    
    hidden(div(
      id = ns('pca_summary_div'), 
      h2('Summary of PCA'),
      DT::dataTableOutput(ns('summary_pca'))
    )),
    
    hidden(div(
      id = ns('pca_body_div'),
      h2('PCA Plot'),
      wellPanel(
       tags$div(style = 'text_align: center', h3("Plot Parameters")),
       fluidRow(
         # Plot controls
         column(width = 6,
                div(style = "display: inline-block;vertical-align: top",
                    uiOutput(ns('xPC_ui'))),
                div(style = "display: inline-block;vertical-align: top",
                    uiOutput(ns('yPC_ui'))),
                div(style = "display: inline-block;vertical-align: top",
                    checkboxInput(ns('show_loading'), "Show loadings", TRUE)),
                div(style = "display: inline-block;vertical-align: top",
                    checkboxInput(ns('load_arrow'), 'Show loading arrows', TRUE))
         ),
         column(width = 12, hr()),
         column(width = 3,
                # score point aesthetics
                h3("Score points aesthetics"),
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
                h3("Score labels aesthetics"),
                uiOutput(ns('score_label_ui')),
                uiOutput(ns('score_lab_colour_ui')),
                sliderInput(ns('score_lab_size'), 'Label size:',
                            min = 0.1, max = 5, value = 3, step = 0.5),
                sliderInput(ns('score_lab_alpha'), 'Label transparency:',
                            min = 0.1, max = 1, value = 1, step = 0.1)
         ),
         
         hidden(div(id=ns('loading_div'),
                    column(width = 3,
                           # loading point aesthetics
                           h3('Loading points aesthetics'),
                           uiOutput(ns('load_pt_colour_ui')),
                           uiOutput(ns('load_pt_shape_ui')),
                           sliderInput(ns('load_pt_size'), 'Point size:',
                                       min = 0.1, max = 5, value = 3, step = 0.5),
                           sliderInput(ns('load_pt_alpha'), 'Point transparency:',
                                       min = 0.1, max = 1, value = 1, step = 0.1)
                    ),
                    # loading label aesthetics
                    column(width = 3,
                           h3('Loading labels aethetics'),
                           uiOutput(ns('load_label_ui')),
                           uiOutput(ns('load_lab_colour_ui')),
                           sliderInput(ns('load_lab_size'), 'Label size:',
                                       min = 0.1, max = 5, value = 3, step = 0.5),
                           sliderInput(ns('load_lab_alpha'), 'Label transparency:',
                                       min = 0.1, max = 1, value = 1, step = 0.1)
                    )
         ))
      ))
    )),
    column(
      width = 12,
      column(
        width = 1, style = 'padding:0px;', dropdown(
        size = 'xs', icon = icon('save'), inline = TRUE, 
        style = 'material-circle', width = 160,
        animate = animateOptions(
         enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
         exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
        
        downloadBttn(ns('dl_pca_original'), 
                    list(icon('file-image'), "Original plot"),
                    size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_pca_html'), 
                    list(icon('file-code'), "Interactive plot"),
                    size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_pca_data'), 
                    list(icon('file-alt'), "Plot data"),
                    size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_pca_rds'), 
                    list(icon('file-prescription'), "RDS"),
                    size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_pca_all'), 
                    list(icon('file-archive'), "All"),
                    size = 'xs', style = 'minimal')
        )),
        column(width = 11, style = 'padding:0px;',
            shinyjqui::jqui_resizable(
              plotlyOutput(ns('plot_pca'), width = '100%', height = 'auto')))
      )
  )
}
    
# Module Server
    
#' @rdname mod_ov_pca
#' @export
#' @keywords internal
    
mod_ov_pca_server <- function(input, output, session, param){
  ns <- session$ns
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(pca_calculate(), {
    show('pca_summary_div')
  })
  observeEvent(pca_calculate(), {
    show('pca_body_div')
  })
  
  observeEvent(input$show_loading, {
    toggle('loading_div')
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
                choices = c('none', colnames(met())), selected = 'none')
  })
  output$score_pt_shape_ui <- renderUI({
    selectInput(ns('score_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  
  ### score label aethetics
  output$score_label_ui <- renderUI({
    selectInput(ns('score_label_by'), 'Label scores by:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  output$score_lab_colour_ui <- renderUI({
    selectInput(ns('score_lab_colour'), 'Label colour:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  
  ### loading points aesthetics
  output$load_pt_colour_ui <- renderUI({
    selectInput(ns('load_pt_colour'), 'Point colour:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_pt_shape_ui <- renderUI({
    selectInput(ns('load_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  ### loading labels aesthetics
  output$load_label_ui <- renderUI({
    selectInput(ns('load_label_by'), 'Label loadings by:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_lab_colour_ui <- renderUI({
    selectInput(ns('load_lab_colour'), 'Label colour:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_lab_shape_ui <- renderUI({
    selectInput(ns('load_lab_shape'), 'Label shape:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  
  
  # unpack data from parent module----------------------------------------------
  met <- reactive(param$met)
  asv <- reactive(param$asv)
  tax <- reactive(param$tax)
  asv_transform <- reactive(param$asv_transform)
  
  # unpack pca inputs
  pca_scale <- reactive(param$pca_input$pca_scale)
  pca_calculate <- reactive(param$pca_input$pca_calculate)
  
  # output$check <- renderPrint({
  # 
  # })
  # calculate pca---------------------------------------------------------------

  # centre and scale
  asv_scale <- eventReactive(pca_calculate(), {
    if(pca_scale() == 'UV') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sd(x))
    }
    else if(pca_scale() == 'pareto') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sqrt(x))
    }
    else if(pca_scale() == 'vast') {
      apply(asv_transform(), 2, function(x) ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))
    }
    else {
      asv_transform()
    }
  })

  # performing pca
  d_pcx <- eventReactive(pca_calculate(), {
    ## samples in rows
    ## centring and scaling done outside of prcomp
    prcomp(t(asv_scale()), center = FALSE, scale. = FALSE)
  })

  xPC <- reactive(paste0('PC', input$xPC))
  yPC <- reactive(paste0('PC', input$yPC))

  score_data <- reactive({
    out <- as.data.frame(d_pcx()$x)
    out$sampleID <- rownames(out)
    out <- out %>%
      select(sampleID, xPC(), yPC()) %>%
      left_join(met(), 'sampleID')
    out <- out[,c(xPC(), yPC(), colnames(met()))]
    out
  })

  load_data <- reactive({
    out <- as.data.frame(d_pcx()$rotation)
    out$featureID <- rownames(out)
    out <- out %>% select('featureID', xPC(), yPC()) %>%
      left_join(tax(), 'featureID')
    out <- out[, c(xPC(), yPC(), colnames(tax()))]
    out
  })


  # summary of pca
  pcx_summary <- eventReactive(pca_calculate(), {
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
    pcx_summary() %>%
      DT::datatable(extensions = 'Buttons',
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
    req(input$show_loading)
    if(input$load_pt_shape == 'none') 2
    else input$load_pt_shape
  })

  load_pt_size <- reactive({
    req(input$show_loading)
    input$load_pt_size
  })
  load_pt_alpha <- reactive({
    req(input$show_loading)
    input$load_pt_alpha
  })
  load_arrow <- reactive({
    req(input$show_loading)
    input$load_arrow
  })

  ## loading label parameters
  load_label_by <- eventReactive(input$load_label_by, {
    req(input$show_loading)
    if(input$load_label_by == 'none') NULL
    else input$load_label_by
  })

  show_load_label <- eventReactive(input$load_label_by, {
    req(input$show_loading)
    if(input$load_label_by == 'none') FALSE
    else show_load_label <- TRUE
  })

  load_lab_colour <- eventReactive(input$load_lab_colour, {
    req(input$show_loading)
    if(input$load_lab_colour == 'none') 'darkred'
    else input$load_lab_colour
  })

  load_lab_size <- reactive({
    req(input$show_loading)
    input$load_lab_size
  })

  load_lab_alpha <- reactive({
    req(input$show_loading)
    input$load_lab_alpha
  })

  p_biplot <- reactive({
    req(pca_calculate())

    p_biplot <- OCMSExplorer:::cms_biplot(
      score_data(), load_data(),
      xPC = input$xPC, yPC = input$yPC,
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

  output$dl_pca_original <- downloadHandler(
    fname <- function() {"ov_pca.tiff"},
    content <- function(file) {ggsave(file, plot=p_biplot())}
  )

  output$dl_pca_html <- downloadHandler(
    fname <- function() {"ov_pca.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_biplot())), file)
    }
  )

  output$dl_pca_data <- downloadHandler(
    fname <- function() {"ov_pcadata.zip"},
    content <- function(file) {
      # put together pca data to write to file
      to_save <- d_pcx()
      to_save[['pca_score']] <- score_data()
      to_save[['pca_load']] <- load_data()

      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- sprintf("ov_pca-%s.csv", names(to_save))
      for(i in 1:length(to_zip)) {
        write.csv(to_save, to_zip[i])
      }

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }

  )

  output$dl_pca_rds <- downloadHandler(
    fname <- function() {"ov_pca.rds"},
    content <- function(file) {
      saveRDS(p_biplot(), file)
    }
  )

  output$dl_pca_all <- downloadHandler(
    fname <- function() {"ov_pca.zip"},
    content <- function(file) {

      # put together pca data to write to file
      to_save <- d_pcx()
      to_save[['pca_score']] <- score_data()
      to_save[['pca_load']] <- load_data()

      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_pca.tiff", "ov_pca.html","ov_pca.rds",
                  sprintf("ov_pca-%s.csv", names(to_save)))
      ggsave(to_zip[1], plot=p_biplot())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_biplot())), to_zip[2])
      saveRDS(p_biplot(), to_zip[3])
      for(i in 1:length(to_save)) {
        write.csv(to_save, to_zip[i+3])
      }

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
}
    
## To be copied in the UI
# mod_ov_pca_ui("ov_pca_ui_1")
    
## To be copied in the server
# callModule(mod_ov_pca_server, "ov_pca_ui_1")
 
