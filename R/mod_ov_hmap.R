# Module UI

#' @title   mod_ov_hmap_ui and mod_ov_hmap_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ov_hmap
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import cowplot
#' @import htmlwidgets
#' @import shinyWidgets

mod_ov_hmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1('Heirarchical Clustering'),
    tags$div("Heirarchical clustering is influenced by the linkage method used to measure the distance between clusters of observations. The linkage methods differ in the criteria that is used to determine the distance of sets of observations. The criteria are based on the distance between individual observations within a set. Choice in distance method also affects the clustering outcome, which measures the distance between a pair of observations. Distance metrics fall into three categories: agglomerative, divisive, and dissimilarity."),
    # wellPanel(verbatimTextOutput(ns('check'))),
    hidden(div(
      id = ns('hmap_body_div'),
      column(
        width = 12,

        column(
          width = 3, br(), br(),
          wellPanel(
            numericInput(ns('hmap_samp_k'), "Number of clusters, k",
                        value = 1, min = 1, step = 1),
            uiOutput(ns('hmap_samp_label_ui')),
            uiOutput(ns('hmap_samp_colour_ui')))),
        column(
          width = 2,
          plotOutput(ns('sample_dendro_leg'))),
        column(
          width = 7,
          h3('Sample dendrogram'),
          mod_download_ui(ns('download_samdendro')),
          shinyjqui::jqui_resizable(
            plotlyOutput(ns('sample_dendro_plot'), width = '100%')
          ))),
      column(
        width = 12,
        column(
          width = 3, br(), br(),
          wellPanel(
            numericInput(ns('hmap_asv_k'), "Number of clusters, k",
                        value = 1, min = 1, step = 1),
            uiOutput(ns('hmap_asv_label_ui')),
            uiOutput(ns('hmap_asv_colour_ui')))),
        column(
          width = 2,
          plotOutput(ns('asv_dendro_leg'))),
        column(
          width = 7,
          h3('Taxonomy dendrogram'),
          mod_download_ui(ns('download_asvdendro')),
          shinyjqui::jqui_resizable(
            plotlyOutput(ns('asv_dendro_plot'), width = '100%')
          ))),

      h2('Heat map'), br(), br(),
      wellPanel(
       fluidRow(
         column(
           width = 3,
           radioButtons(ns('sample_as_x'), "Show samples along:",
                        choices = c('x-axis' = TRUE, 'y-axis' = FALSE),
                        selected = TRUE)),

         column(
           width = 3,
           checkboxGroupInput(ns('show_dendro'), 'Show dendrogram',
                              choices = c('x-axis' = 'show_dendro_x',
                                         'y-axis' = 'show_dendro_y'),
                              selected = c('show_dendro_x', 'show_dendro_y'))),
         column(
           width = 3,
           selectInput(ns('hmap_tax_label'), 'Label taxa by:',
                       choices = c('featureID','Taxon','Species')))
         )),
      column(
        width = 12,
        column(
          width = 1, style = 'padding:0px;',
          mod_download_ui(ns('download_hmap'))
          # dropdown(
          #   size = 'xs', icon = icon('save'), inline = TRUE,
          #   style = 'material-circle', width = 160,
          #   animate = animateOptions(
          #     enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
          #     exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
          #
          #   myDownloadBttn(ns('dl_hmap_html'), icon_name = 'file-code',
          #                label = "Interactive lot",
          #                size = 'xs', style = 'minimal'), br(),
          #   myDownloadBttn(ns('dl_hmap_data'), icon_name = 'file-alt',
          #                label = "Plot data",
          #                size = 'xs', style = 'minimal'), br(),
          #   myDownloadBttn(ns('dl_hmap_rds'), icon_name = 'file-prescription',
          #                label = "RDS",
          #                size = 'xs', style = 'minimal'), br(),
          #   myDownloadBttn(ns('dl_hmap_all'), icon_name = "file-archive",
          #                label = "All",
          #                size = 'xs', style = 'minimal')
          # )
        ),
        column(
          width = 11, style = 'padding:0px;',
          shinyjqui::jqui_resizable(
            plotlyOutput(ns('hmap_plot'), width = '100%', height = 'auto')
          )
        )
      )
    ))
  )
}



# Module Server

#' @rdname mod_ov_hmap
#' @export
#' @keywords internal

mod_ov_hmap_server <- function(input, output, session, param){
  ns <- session$ns

  # unpack data from parent module----------------------------------------------
  # unpack alpha inputs
  hclust_method <- reactive(param$hmap_input$hclust_method)
  dist_method <- reactive(param$hmap_input$dist_method)
  hmap_calculate <- reactive(param$hmap_input$hmap_calculate)

  met_var <- reactive({
    out <- colnames(param$work_db$met)
    out <- out[out != 'sampleID']
  })

  # toggle div for input controls-----------------------------------------------
  observeEvent(hmap_calculate(), {
    show('hmap_body_div')
  })

  # render controls - heat map--------------------------------------------------
  output$hmap_samp_label_ui <- renderUI({
    selectInput(ns('hmap_samp_label'), "Label:",
                choices = colnames(param$work_db$met), selected = 'sampleID')
  })
  output$hmap_samp_colour_ui <- renderUI({
    radioButtons(ns('hmap_samp_colour'), "Show sample metadata:",
                 choices = c('none', met_var()),
                 selected = 'none')
  })

  output$hmap_asv_label_ui <- renderUI({
    selectInput(ns('hmap_asv_label'), "Label:",
                choices = colnames(param$work_db$tax), selected = 'featureID')
  })

  output$hmap_asv_colour_ui <- renderUI({
    choices <- c('none', colnames(param$work_db$tax))
    choices <- choices[!choices %in% c('sequence','featureID','Taxon')]
    radioButtons(ns('hmap_asv_colour'), "Show taxonomy level:",
                 choices = choices, selected = 'none')
  })

  # calculate heatmap-----------------------------------------------------------

  # calculate sample clustering
  samp_hclust <- reactive({
    req(hmap_calculate())
    hclust(vegan::vegdist(t(param$work_db$asv_transform),
                          method = dist_method()),
           method = hclust_method())
  })

  samp_ddata <- reactive({
    req(hmap_calculate())
    dendro_data_k(samp_hclust(), input$hmap_samp_k)
  })

  # sample dendrogram
  p_dend_samp <- reactive({
    req(input$hmap_samp_k, input$hmap_samp_colour)
    if(input$hmap_samp_colour == 'none') category <- NULL
    else category <- input$hmap_samp_colour
    p <- plot_ggdendro(
      samp_ddata(),
      direction = 'lr',
      branch.size = 0.5,
      metadata = param$work_db$met,
      category = category,
      nudge.label = 0.01,
      label.category = input$hmap_samp_label,
      id = 'sampleID')
    p
  })

  output$sample_dendro_plot <- renderPlotly({
    label_data <- ggplot_build(p_dend_samp())$data[[2]]

    ggplotly(p_dend_samp() + theme(legend.position = 'none')) %>%
      style(text = label_data$label, textposition = "middle right")
  })

  output$sample_dendro_leg <- renderPlot({
    p_legend <- cowplot::get_legend(p_dend_samp())
    grid::grid.draw(p_legend)
  })

  # download data
  for_download1 <- reactiveValues()
  observe({
    req(param$hmap_input$hclust_method, param$hmap_input$dist_method,
        param$hmap_input$hmap_calculate)
    for_download1$figure <- p_dend_samp()
    for_download1$fig_data <- samp_ddata()
  })

  callModule(mod_download_server, "download_samdendro", bridge = for_download1,
             'sample_dendrogram', dl_options = c('png','html','pdf','zip'))

  # calculate asv clustering

  asv_hclust <- reactive({
    req(hmap_calculate())
    hclust(vegan::vegdist(param$work_db$asv_transform, method = dist_method()),
           method = hclust_method())
  })

  asv_ddata <- reactive({
    req(hmap_calculate())
    dendro_data_k(asv_hclust(), input$hmap_asv_k)
  })

  # asv dendrogram
  p_dend_asv <- reactive({
    req(input$hmap_asv_k, input$hmap_asv_colour)
    if(input$hmap_asv_colour == 'none') category <- NULL
    else category <- input$hmap_asv_colour

    p <- plot_ggdendro(
      asv_ddata(),
      direction = 'lr',
      branch.size = 0.5,
      metadata = param$work_db$tax,
      label.category = input$hmap_asv_label,
      nudge.label = 0.01,
      category = category,
      id = 'featureID')
  })

  output$asv_dendro_plot <- renderPlotly({
    label_data <- ggplot_build(p_dend_asv())$data[[2]]
    ggplotly(p_dend_asv() + theme(legend.position = 'none')) %>%
      style(text = label_data$label, textposition = "middle right")
  })

  output$asv_dendro_leg <- renderPlot({
    p_legend <- cowplot::get_legend(p_dend_asv())
    grid::grid.draw(p_legend)
  })

  # download data
  for_download2 <- reactiveValues()
  observe({
    req(param$hmap_input$hclust_method, param$hmap_input$dist_method,
        param$hmap_input$hmap_calculate)
    for_download2$figure <- p_dend_asv()
    for_download2$fig_data <- asv_ddata()
  })

  callModule(mod_download_server, "download_asvdendro", bridge = for_download2,
             'feature_dendrogram', dl_options = c('png','html','pdf','zip'))

  # heatmap---------------------------------------------------------------------
  # set heatmap orientation
  hmap_data <- reactive({

    if(input$sample_as_x) {
      hmap_data <- param$work_db$asv_transform # taxon in rows, samples in columns
      rownames(hmap_data) <- param$work_db$tax[, input$hmap_tax_label]
    }
    else {
      hmap_data <- t(param$work_db$asv_transform)
      colnames(hmap_data) <- param$work_db$tax[, input$hmap_tax_label]
    }
    hmap_data
  })

  # output$check <- renderPrint({
  #   hmap_data()
  # })
  # parameterizing heat map object
  hmap <- reactive({
    heatmapr(
      x = hmap_data(),
      distfun = vegan::vegdist,
      dist_method = dist_method(),
      hclust_method = hclust_method(),
      dendrogram = 'both',
      show_dendrogram = c('show_dendro_y' %in% input$show_dendro,
                          'show_dendro_x' %in% input$show_dendro),
      digits = 3,
      show_grid = TRUE
    )
  })

  hmaply_plot <- reactive({
    req(hmap_calculate())

    if(param$work_db$transform_method == 'none') {
      key_title <- 'Read Count'
    }
    else if(param$work_db$transform_method == 'percent') {
      key_title <- 'Relative Abundance (%)'
    }
    else {
      key_title <- 'Normalized\nRelative Abundance'
    }
    heatmaply(hmap(), node_type = 'heatmap',
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red"),
              key.title = key_title)
  })
  # plot heat map
  output$hmap_plot <- renderPlotly({
    hmaply_plot()
  })

  # download data
  for_download3 <- reactiveValues()
  observe({
    req(param$hmap_input$hclust_method, param$hmap_input$dist_method,
        param$hmap_input$hmap_calculate)
    for_download3$figure <- hmaply_plot()
    for_download3$fig_data <- hmap_data() %>%
      as.data.frame() %>%
      mutate(featureID = rownames(hmap_data()))
  })

  callModule(mod_download_server, "download_hmap", bridge = for_download3,
             'heatmap', dl_options = c('html','csv','pdf','zip'))


  #
  #
  #
  # to_zip <- reactive({
  #   sprintf("heatmap_%s.%s", Sys.Date(), c("html","csv", "RDS", "zip"))
  # })
  #
  # # download data
  # output$dl_hmap_html <- downloadHandler(
  #   filename = function() {
  #     to_zip()[grepl('html', to_zip())]
  #   },
  #   content = function(file) {
  #     htmlwidgets::saveWidget(hmaply_plot(), file)
  #   },
  #   contentType = 'text/html'
  # )
  #
  # output$dl_hmap_data <- downloadHandler(
  #   filename = function() {
  #     to_zip()[grepl('csv', to_zip())]
  #   },
  #   content = function(file) {
  #     write.csv(hmap_data(), file, row.names = FALSE)
  #   },
  #   contentType = 'text/csv'
  # )
  #
  # output$dl_hmap_rds <- downloadHandler(
  #   fname <- function() {
  #     to_zip()[grepl('RDS', to_zip())]
  #   },
  #   content <- function(file) {
  #     saveRDS(hmap(), file)
  #   },
  #   contentType = 'application/rds'
  # )
  #
  # output$dl_hmap_all <- downloadHandler(
  #   filename = function() {
  #     to_zip()[grepl('zip', to_zip())]
  #   },
  #   content = function(file) {
  #     # save current directory
  #     mydir <- getwd()
  #     # create temporary directory
  #     tmpdir <- tempdir()
  #     setwd(tempdir())
  #
  #     htmlwidgets::saveWidget(hmaply_plot(), to_zip()[grepl('html', to_zip())])
  #     write.csv(hmap_data(), to_zip()[grepl('csv', to_zip())],
  #               row.row.names = FALSE)
  #     saveRDS(hmap(), to_zip()[grepl('RDS', to_zip())])
  #
  #     #create the zip file
  #     zip(file, to_zip()[grepl('zip', to_zip())])
  #     setwd(mydir)
  #   },
  #   contentType = 'application/zip'
  # )
}

## To be copied in the UI
# mod_ov_hmap_ui("ov_hmap_ui_1")

## To be copied in the server
# callModule(mod_ov_hmap_server, "ov_hmap_ui_1")

