# Module UI

#' @title   mod_da_prop_ui and mod_da_prop_server
#' @description  A shiny submodule of differential abundance analysis.
#' Calculates and visuzlaizes proportionality scores
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param param reactive values to communicate with outer module
#'
#' @rdname mod_da_prop
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import propr
mod_da_prop_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1('Proportionality'),
    column(
      width = 12,
      column(
        width = 12, 'Explanation....', br()),
      br(),
      hidden(div(
        id = ns('fdr_summary_div'),
        column(
          width = 8,
          verbatimTextOutput(ns('prop_fdr_summary'))
        ),
        column(
          width = 4,
          "Explanation...Choose the largest cutoff that keeps the FDR below 0.05"
        )
      )),
      br()
    ),
    column(
      width = 12,
      column(
        width = 8,
        h3("Summary of Rho Values"),
        verbatimTextOutput(ns('prop_summary'))
      ),
      column(
        width = 4,
        br(),br(),
        "Explanation..."
      )
    ),
    hidden(div(
      id = 'prop_hmap_div',
      h3('Proportionality Matrix'),
      column(
        width = 12,
        wellPanel(
          tags$b("Heirchical Cluster Parameters"),
          fluidRow(
            column(
              width = 3,
              selectInput(ns('hclust_method'), "Linkage method",
                          choices = c('complete','ward.D','ward.D2','single',
                                      'average','mcquitty','median','centroid'),
                          selected = 'complete'),
              selectInput(ns('dist_method'), "Distance method",
                          choices = c("manhattan", "euclidean", "canberra",
                                      "clark", "bray", "kulczynski", "jaccard",
                                      "gower", "altGower", "morisita", "horn",
                                      "mountford", "raup", "binomial", "chao",
                                      "cao", "mahalanobis"),
                          selected = 'euclidean')),
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
          )
        )
      )
    )),
    column(
      width = 12,
      column(
        width = 1, style = 'padding:0px;',
        mod_download_ui(ns("download_prop_hmap"))
      ),
      column(
        width = 11, style = 'padding:0px;',
        shinyjqui::jqui_resizable(
          plotlyOutput(ns('hmap_prop'), width = '100%', height = 'auto')))
    ),
    column(
      width = 12,
      h3('Visualizing Feature Pairs'),
      DT::dataTableOutput(ns('prop_table')),
    ),
    column(
      width = 12,
      tabsetPanel(
        type = 'tabs',
        tabPanel("Feature Pairs",
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
        ),
        tabPanel("By Metadata",

                 fluidRow(
                   uiOutput(ns("meta_x_ui"))
                 ),
                 fluidRow(
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
                )
      )
    )
  )
}

# Module Server

#' @rdname mod_da_prop
#' @export
#' @keywords internal

mod_da_prop_server <- function(input, output, session, param){
  ns <- session$ns

  # show/hide fdr summary-------------------------------------------------------
  observeEvent(param$prop_input$prop_calculate, {
    show('fdr_summary_div')
  })

  observeEvent(param$prop_input$apply_filter, {
    show('prop_hmap_div')
  })

  #generate UI------------------------------------------------------------------
  output$meta_x_ui <- renderUI({
    selectInput(ns('meta_x'), 'x-axis', choices = colnames(param$work_db$met),
                selected = 'sampleID')
  })

  # calculate rho---------------------------------------------------------------
  propr_obj <- eventReactive(param$prop_input$prop_calculate, {
    # propr package uses propr S4 class to store info -- see propr manual
    count_mat <- param$work_db$asv_transform

    # features in columns
    # default setting for ivar is clr transform
    out <- propr(t(count_mat), metric = 'rho')

    # calculate fdr at different cutoffs
    out <- updateCutoffs(out, cutoff = seq(0.05, 0.95, 0.15))
    out
  })

  # showing fdr calculations----------------------------------------------------
  output$prop_fdr_summary <- renderPrint({
    propr_obj()@fdr
  })

  # identify pairs that satisfy filter------------------------------------------
  pairs_keep <- eventReactive(param$prop_input$apply_filter, {
    if(param$prop_input$rho_filter == 'filter') {
      req(param$prop_input$rho_cutoff)
      # keep inside range
      if(param$prop_input$rho_operator == 'inside') {
        lesser <- propr_obj()["<=", max(param$prop_input$rho_cutoff)]@pairs
        greater <- propr_obj()[">=", min(param$prop_input$rho_cutoff)]@pairs
        out <- intersect(lesser, greater)
      }
      # keep outside range
      else {
        lesser <- propr_obj()["<=", min(param$prop_input$rho_cutoff)]@pairs
        greater <- propr_obj()[">=", max(param$prop_input$rho_cutoff)]@pairs
        out <- c(lesser, greater)
      }
    }
    # keep all (no filter)
    else if(param$prop_input$rho_filter == 'all') {
      out <- propr_obj()["<=", 1]@pairs
    }
    out
  })

  # apply filters
  work_obj <- eventReactive(param$prop_input$apply_filter,{
    out <- propr_obj()
    out@pairs <- pairs_keep()
    out <- propr::simplify(out)
  })

  # extract rho values
  rho_df <- reactive({
    req(param$prop_input$apply_filter)
    propr:::proprPairs(work_obj()@matrix)
  })

  output$prop_summary <- renderPrint({
    cat('Rho distribution in selection:\n')
    print(summary(rho_df()$prop))
    cat('Number of feature pairs in selection:\n')
    print(length(work_obj()@pairs))
  })

  # heatmap of subset-----------------------------------------------------------
  hmap_data <- reactive({
    out <- work_obj()@matrix
    convert <- data.frame(featureID = rownames(out))
    convert <- convert %>%
      left_join(param$work_db$tax %>% select(featureID, Taxon, Species),
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
    heatmapr(
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
    heatmaply(hmap(), node_type = 'heatmap',
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red",
                midpoint = 0,
                limits = c(-1, 1)),
              key.title = 'Rho')
  })
  # plot heat map
  output$hmap_prop <- renderPlotly({
    req(param$prop_input$apply_filter)
    hmaply_prop()
  })

  # download data
  for_download1 <- reactiveValues()
  observe({
    req(param$prop_input$prop_calculate, param$prop_input$apply_filter)
    for_download1$figure <- hmaply_prop()
    for_download1$fig_data <- hmap_data() %>%
      as.data.frame() %>%
      mutate(featureID = rownames(hmap_data()))
  })

  callModule(mod_download_server, "download_prop_hmap", bridge = for_download1,
             'heatmap', dl_options = c('html','csv','RDS','zip'))

  # select ASV pairs from table
  output$prop_table <- DT::renderDataTable({
    DT::datatable(rho_df() %>%
                    mutate(prop = round(prop, digits = 3)),
                  colnames = c('Feature 1','Feature 2','Rho'),
                  options = list(searchHighlight = TRUE), filter = 'top',
                  selection = list(mode = "multiple", selected = 1))
  })

  # visualize selected ASV pairs
  selected <- reactive({
    out <- rho_df()[input$prop_table_rows_selected,]
    out <- split(out, seq(nrow(out)))
    out
  })

  output$check <- renderPrint({
  })

  pair_pdata <- reactive({
    curr <- work_obj()@logratio
    out <- c()
    for(i in selected()) {
      curr_row <- c(i$feature1, i$feature2, round(i$prop, digits = 3))
      entry <- data.frame(pairID = paste(curr_row[1:2], collapse = "_"),
                          panel = sprintf("Feat1: %s, Feat2: %s, rho= %s",
                                          curr_row[1], curr_row[2], curr_row[3]),
                          feature1 = curr_row[1],
                          feature2 = curr_row[2],
                          prop = curr_row[3],
                          x = curr[,curr_row[1]],
                          y = curr[,curr_row[2]])
      out <- rbind(out, entry)
    }
    out
  })

  p_pair <- reactive({
    ggplot(pair_pdata(), aes(x = x, y = y)) +
      geom_point() +
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
    req(param$prop_input$prop_calculate, param$prop_input$apply_filter,
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
      inner_join(param$work_db$met, 'sampleID')

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

  p_meta <- reactive({
    req(input$meta_x)
    p <- ggplot(meta_pdata(), aes_string(x = input$meta_x, y = 'logratio')) +
      facet_wrap(~panel, scales = 'free', ncol = 3) +
      theme_bw(12) +
      theme(legend.position = 'none') +
      ylab('Normalized\nrelative abundance')

    if(any(add_line()$npoint > 2)) {
      p <- p +
        geom_point(aes(colour = featureID, group = featureID),
                   position=position_dodge(width = 0.75), alpha = 0.8) +
        geom_boxplot(aes(colour = featureID,
                         group = interaction(featureID, .data[[input$meta_x]])),
                     fill = NA)
    }
    else {
      p <- p +
        geom_point(aes(colour = featureID)) +
        geom_line(aes(colour = featureID, group = featureID))

    }
    p
  })

  output$plot_meta <- renderPlotly({
    if(any(add_line()$npoint > 2)) {
      ggplotly(p_meta()) %>%
        layout(boxmode='group')
    }
    else {
      ggplotly(p_meta())
    }

  })

  # download data
  for_download3 <- reactiveValues()
  observe({
    req(param$prop_input$prop_calculate, param$prop_input$apply_filter,
        input$prop_table_rows_selected)
    for_download3$figure <- p_meta()
    for_download3$fig_data <- meta_pdata()
  })

  callModule(mod_download_server, "download_pairs_meta", bridge = for_download3, 'prop')

}

## To be copied in the UI
# mod_da_prop_ui("da_prop_ui_1")

## To be copied in the server
# callModule(mod_da_prop_server, "da_prop_ui_1")

