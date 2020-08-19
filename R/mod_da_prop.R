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
    column(
      width = 12,
      h3('Proportionality Matrix'),
      DT::dataTableOutput(ns('prop_table'))
    ),
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
    ),
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

  output$prop_table <- DT::renderDataTable(
    DT::datatable(work_obj()@matrix, extension = 'Buttons',
                   options=list(dom = 'Blfrtip', buttons = c('copy','csv'),
                                scrollX = TRUE)) %>%
      DT::formatRound(column = colnames(work_obj()@matrix), digits = 3)
  )

  output$check <- renderPrint({
    out <- work_obj()@matrix
    convert <- data.frame(featureID = rownames(out)) 
    convert <- convert %>%
      left_join(tax()%>% select(featureID, Taxon, Species), 'featureID')
    convert
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
              key.title = 'Rho')
  })

  output$dl_hmap_html <- downloadHandler(
    fname <- function() {"da_hmap.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(heatmaply(hmap(),
                                        node_type = 'heatmap', colors = 'RdYlBu',
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
      to_zip <- c("ov_hmap.html","ov_hmap.csv", "ov_hmap.rds")
      htmlwidgets::saveWidget(as_widget(ggplotly(p_hmap())), to_zip[2])
      write.csv(asv_ddata(), to_zip[3])
      saveRDS(p_hmap(), to_zip[4])

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
}

## To be copied in the UI
# mod_da_prop_ui("da_prop_ui_1")

## To be copied in the server
# callModule(mod_da_prop_server, "da_prop_ui_1")

