# Module UI

#' @title   mod_ov_bar_ui and mod_ov_bar_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ov_bar
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import htmlwidgets
#' @import shinyWidgets
#' @import readr
mod_ov_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    h1('Relative Distribution of Taxa'),
    column(width = 12,
           h3(textOutput(ns('bar_title'))),
           p("Observing relative abundance or sequence abundance based on metadata variables. Abundance values can be aggregated at different taxonomic levels. The mean relative abundance is shown when selected group variable contains multiple samples"),
           DT::dataTableOutput(ns('bar_table'))  %>%
             shinycssloaders::withSpinner()
          ),
    column(width = 12,
           column(
             width = 1, style = 'padding:0px;',
             mod_download_ui(ns("download_bar"))
           ),
           column(width = 11, style = 'padding:0px;',
                  shinyjqui::jqui_resizable(
                    plotlyOutput(ns('bar_plot'), width = '100%', height = 'auto')%>% 
                      shinycssloaders::withSpinner())
           )
    )
  )
}

# Module Server

#' @rdname mod_ov_bar
#' @export
#' @keywords internal

mod_ov_bar_server <- function(input, output, session, param){
  ns <- session$ns
  
  # unpack bar plot inputs
  bar_tax <- reactive(param$bar_input$bar_tax)
  bar_y <- reactive(param$bar_input$bar_y)
  bar_x <- reactive(param$bar_input$bar_x)
  
  # calculate output bar plot---------------------------------------------------
  
  bar_data <- reactive({
    param$work_db$work %>%
      # sample total read count
      group_by(sampleID) %>%
      mutate(sample_total = sum(read_count)) %>%
      # aggregate on taxon within each sample
      group_by(sampleID, !!sym(bar_tax())) %>%
      mutate(tax_cnt = sum(read_count), tax_rel = tax_cnt / sample_total) %>%
      ungroup() %>%
      distinct(!!sym(bar_tax()), !!sym(bar_x()), tax_cnt, tax_rel) %>%
      # mean of aggregated counts within selected group
      group_by(!!sym(bar_x()), !!sym(bar_tax())) %>%
      mutate(cnt_abund = mean(tax_cnt),
             rel_abund = mean(tax_rel)) %>%
      ungroup() 
  })
  
  output$bar_title  <- renderText({
    
    if(bar_y() == 'rel_abund') {
      sprintf('Mean Relative Abundance (%%), %s', bar_tax())
    }
    else {
      sprintf('Mean Cumulative Read Count, %s', bar_tax())
    }
  })
  
  # output$check <- renderPrint({
  # 
  # })
  
  pdata <- reactive({
    bar_data() %>%
      distinct(!!sym(bar_tax()), !!sym(bar_x()), !!sym(bar_y()))
  })
  
  
  output$bar_table <- DT::renderDataTable({
    out <- pdata() %>% spread(!!sym(bar_x()), !!sym(bar_y()))
    x_name <- colnames(out)
    x_name <- x_name[x_name != bar_tax()]
    
    # by default, only show first 50 samples + 8 tax columns
    if(ncol(out) <= 51) {
      # if less than 50 samples, show all
      col_ind <- 1:ncol(out) # index of columns to show
      vis_val <- TRUE
    }
    else {
      col_ind <- 52:ncol(out) # index of columns to hide
      vis_val <- FALSE
    }
    
    
    if(bar_y() == 'rel_abund') {
      DT::datatable(out,  extensions = 'Buttons',
                    options = list(
                      pageLength = 30,
                      scrollX = TRUE,
                      dom = 'Blfrtip',
                      buttons = list(c('copy','csv'), list(extend = 'colvis')),
                      columnDefs = list(
                        list(targets = col_ind, visible = vis_val)
                      ))) %>%
        DT::formatRound(column = x_name, digits = 3)
    }
    else {
      DT::datatable(out,extensions = 'Buttons',
                    options = list(
                      pageLength = 30,
                      scrollX = TRUE,
                      dom = 'Blfrtip',
                      buttons = list(c('copy','csv'), list(extend = 'colvis')),
                      columnDefs = list(
                        list(targets = col_ind, visible = vis_val)
                      )))
    }
    
  })
  
  p_bar <- reactive({
    
    p <- ggplot(pdata(), aes_string(x = bar_x(), y = bar_y(), fill = bar_tax())) +
      geom_bar(stat = 'identity') +
      xlab(bar_x()) +
      scale_fill_discrete(name = bar_tax()) +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    if(bar_y() == 'rel_abund') {
      p <- p +
        ylab(sprintf('Mean Relative Abundance (%%), %s', bar_tax()))
    }
    else {
      p <- p +
        ylab(sprintf('Mean Read Count, %s', bar_tax()))
    }
    p
  })
  
  output$bar_plot <- renderPlotly({
    ggplotly(p_bar())
  })
  
  # download data
  for_download <- reactiveValues()
  observe({
    req(param$bar_input$bar_tax, param$bar_input$bar_y, param$bar_input$bar_x)
    for_download$figure <- p_bar()
    for_download$fig_data <- pdata()
  })
  
  callModule(mod_download_server, "download_bar", bridge = for_download, 'bar')
  
}

## To be copied in the UI
# mod_ov_bar_ui("ov_bar_ui_1")

## To be copied in the server
# callModule(mod_ov_bar_server, "ov_bar_ui_1")