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
    wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
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
        dropdown(
          size = 'xs', icon = icon('save'), inline = TRUE,
          style = 'material-circle', width = 160,
          animate = animateOptions(
            enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
            exit = shinyWidgets::animations$fading_exits$fadeOutLeft),

          downloadBttn(ns('dl_hmap_html'),
                       list(icon('file-code'), "Interactive plot"),
                       size = 'xs', style = 'minimal'), br(),
          downloadBttn(ns('dl_hmap_data'),
                       list(icon('file-alt'), "Plot data"),
                       size = 'xs', style = 'minimal'), br(),
          downloadBttn(ns('dl_hmap_rds'),
                       list(icon('file-prescription'), "RDS"),
                       size = 'xs', style = 'minimal'), br(),
          downloadBttn(ns('dl_hmap_all'),
                       list(icon('file-archive'), "All"),
                       size = 'xs', style = 'minimal')
        )),
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
                   dropdown(
                     size = 'xs', icon = icon('save'), inline = TRUE,
                     style = 'material-circle', width = 160,
                     animate = animateOptions(
                       enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                       exit = shinyWidgets::animations$fading_exits$fadeOutLeft),

                     downloadBttn(ns('dl_pair_html'),
                                  list(icon('file-code'), "Interactive plot"),
                                  size = 'xs', style = 'minimal'), br(),
                     downloadBttn(ns('dl_pair_data'),
                                  list(icon('file-alt'), "Plot data"),
                                  size = 'xs', style = 'minimal'), br(),
                     downloadBttn(ns('dl_pair_rds'),
                                  list(icon('file-prescription'), "RDS"),
                                  size = 'xs', style = 'minimal'), br(),
                     downloadBttn(ns('dl_pair_all'),
                                  list(icon('file-archive'), "All"),
                                  size = 'xs', style = 'minimal')
                   )),
                 column(
                   width = 11, style = 'padding:0px;',
                   shinyjqui::jqui_resizable(
                     plotlyOutput(ns("plot_pair"), width = '100%',
                                  height = 'auto'))
                 )
        ),
        tabPanel("By Metadata",
                 wellPanel(
                   uiOutput(ns("meta_x_ui"))
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
}

# Module Server

#' @rdname mod_da_prop
#' @export
#' @keywords internal

mod_da_prop_server <- function(input, output, session, param){
  ns <- session$ns

  # unpack data from param------------------------------------------------------
  met <- reactive(param$met)
  asv <- reactive(param$asv)
  tax <- reactive(param$tax)
  asv_transform <- reactive(param$asv_transform)

  prop_calculate <- reactive(param$prop_input$prop_calculate)
  apply_filter <- reactive(param$prop_input$apply_filter)
  rho_filter <- reactive(param$prop_input$rho_filter)
  rho_cutoff <- reactive(param$prop_input$rho_cutoff)
  rho_operator <- reactive(param$prop_input$rho_operator)

  # show/hide fdr summary-------------------------------------------------------
  observeEvent(prop_calculate(), {
    show('fdr_summary_div')
  })

  observeEvent(apply_filter(), {
    show('prop_hmap_div')
  })

  #generate UI------------------------------------------------------------------
  output$meta_x_ui <- renderUI({
    selectInput(ns('meta_x'), 'x-axis', choices = colnames(met()),
                selected = 'sampleID')
  })

  # calculate rho---------------------------------------------------------------
  propr_obj <- eventReactive(prop_calculate(), {
    # propr package uses propr S4 class to store info -- see propr manual
    count_mat <- asv() %>%
      select(-featureID) %>%
      as.matrix()
    rownames(count_mat) <- asv()$featureID

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
  pairs_keep <- eventReactive(apply_filter(), {
    req(rho_filter())
    if(rho_filter() == 'filter') {
      req(rho_cutoff())
      # keep inside range
      if(rho_operator() == 'inside') {
        lesser <- propr_obj()["<=", max(rho_cutoff())]@pairs
        greater <- propr_obj()[">=", min(rho_cutoff())]@pairs
        out <- intersect(lesser, greater)
      }
      # keep outside range
      else {
        lesser <- propr_obj()["<=", min(rho_cutoff())]@pairs
        greater <- propr_obj()[">=", max(rho_cutoff())]@pairs
        out <- c(lesser, greater)
      }
    }
    # keep all (no filter)
    else {
      out <- propr_obj()["<=", 1]@pairs
    }
    out
  })

  # apply filters
  work_obj <- eventReactive(apply_filter(),{
    out <- propr_obj()
    out@pairs <- pairs_keep()
    out <- simplify(out)
  })

  # extract rho values
  rho_df <- eventReactive(apply_filter(), {
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
      left_join(tax() %>% select(featureID, Taxon, Species),
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

  # plot heat map
  output$hmap_prop <- renderPlotly({
    req(apply_filter())
    heatmaply(hmap(), node_type = 'heatmap',
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red",
                midpoint = 0,
                limits = c(-1, 1)),
              key.title = 'Rho')
  })

  output$dl_hmap_html <- downloadHandler(
    fname <- function() {"da_hmap.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(
        heatmaply(
          hmap(),
          node_type = 'heatmap',
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "blue",
            high = "red",
            midpoint = 0,
            limits = c(-1, 1)),
          key.title = 'Rho'),
        file)
    }
  )

  output$dl_hmap_data <- downloadHandler(
    fname <- function() {"da_hmap.csv"},
    content <- function(file) {
      readr::write_csv(hmap_data(), file)
    }
  )

  output$dl_hmap_rds <- downloadHandler(
    fname <- function() {"da_hmap.rds"},
    content <- function(file) {
      saveRDS(hmap(), file)
    }
  )

  output$dl_hmap_all <- downloadHandler(
    fname <- function() {"da_hmap.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("da_hmap.html","da_hmap.csv", "da_hmap.rds")
      htmlwidgets::saveWidget(
        heatmaply(
          hmap(),
          node_type = 'heatmap',
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "blue",
            high = "red",
            midpoint = 0,
            limits = c(-1, 1)),
          key.title = 'Rho'),
        to_zip[2])
      write.csv(hmap_data(), to_zip[3])
      saveRDS(p_hmap(), to_zip[4])

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )

  # select ASV pairs from table
  output$prop_table <- DT::renderDataTable({
    DT::datatable(rho_df() %>%
                    mutate(prop = round(prop, digits = 3)),
                  colnames = c('Feature 1','Feature 2','Rho'),
                  options = list(searchHighlight = TRUE), filter = 'top')
  })

  # visualize selected ASV pairs
  selected <- reactive({
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
                          panel = sprintf("%s_%s, %s",
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
    req(input$prop_table_rows_selected)
    ggplotly(p_pair())
  })

  output$dl_pair_html <- downloadHandler(
    fname <- function() {"da_pair.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(ggplotly(p_pair()), file)
    }
  )

  output$dl_pair_data <- downloadHandler(
    fname <- function() {"da_pair.csv"},
    content <- function(file) {
      readr::write_csv(pair_pdata(), file)
    }
  )

  output$dl_pair_rds <- downloadHandler(
    fname <- function() {"da_pair.rds"},
    content <- function(file) {
      saveRDS(p_pair(), file)
    }
  )

  output$dl_pair_all <- downloadHandler(
    fname <- function() {"da_pair.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("da_pair.html","da_pair.csv", "da_pair.rds")
      htmlwidgets::saveWidget(ggplotly(p_pair()), to_zip[2])
      write.csv(pair_pdata(), to_zip[3])
      saveRDS(p_pair(), to_zip[4])

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )

  output$check <- renderPrint({
    print(head(pair_pdata()))
    print(head(meta_pdata()))
  })

  # put metadata with selected features
  meta_pdata <- reactive({

    logratio <- work_obj()@logratio
    logratio$sampleID <- rownames(logratio)
    logratio <- logratio %>%
      gather('featureID', 'logratio', -sampleID) %>%
      inner_join(met(), 'sampleID')

    pair_pdata() %>%
      distinct(pairID, panel, prop, feature1, feature2) %>%
      gather('variable','featureID', -pairID, -panel, -prop) %>%
      left_join(logratio, 'featureID')
  })

  p_meta <- reactive({
    ggplot(meta_pdata(), aes_string(x = input$meta_x, y = 'logratio')) +
      geom_point(aes(colour = featureID)) +
      geom_line(aes(colour = featureID, group = featureID)) +
      facet_wrap(~panel, scales = 'free', ncol = 3) +
      theme_bw(12) +
      theme(legend.position = 'none') +
      ylab('Normalized\nrelative abundance')
  })

  output$plot_meta <- renderPlotly({
    ggplotly(p_meta())
  })
  # # return dataset
  # cross_module <- reactiveValues()
  # # observe({cross_module$work_obj <- work_obj()})
  # observe({cross_module$prop_obj <- prop_obj()})
  # return(cross_module)
}

## To be copied in the UI
# mod_da_prop_ui("da_prop_ui_1")

## To be copied in the server
# callModule(mod_da_prop_server, "da_prop_ui_1")

