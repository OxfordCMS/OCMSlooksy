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
mod_ov_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    h1('Relative Distribution of Taxa'),
    column(width = 12,
           h3(textOutput(ns('bar_title'))),
           DT::dataTableOutput(ns('bar_table'))),
    column(width = 12,
           column(width = 1, style = 'padding:0px;', dropdown(
             size = 'xs', icon = icon('save'), inline = TRUE, 
             style = 'material-circle', width = 160,
             animate = animateOptions(
               enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
               exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
             
             downloadBttn(ns('dl_bar_original'), 
                          list(icon('file-image'), "Original plot"),
                          size = 'xs', style = 'minimal'), br(),
             downloadBttn(ns('dl_bar_html'), 
                          list(icon('file-code'), "Interactive plot"),
                          size = 'xs', style = 'minimal'), br(),
             downloadBttn(ns('dl_bar_data'), 
                          list(icon('file-alt'), "Plot data"),
                          size = 'xs', style = 'minimal'), br(),
             downloadBttn(ns('dl_bar_rds'), 
                          list(icon('file-prescription'), "RDS"),
                          size = 'xs', style = 'minimal'), br(),
             downloadBttn(ns('dl_bar_all'), 
                          list(icon('file-archive'), "All"),
                          size = 'xs', style = 'minimal')
           )),
           column(width = 11, style = 'padding:0px;',
                  shinyjqui::jqui_resizable(
                    plotlyOutput(ns('bar_plot'), width = '100%', height = 'auto')))
    )
  )
}
    
# Module Server
    
#' @rdname mod_ov_bar
#' @export
#' @keywords internal
    
mod_ov_bar_server <- function(input, output, session, param){
  ns <- session$ns
  
  # unpack data from param
  met <- reactive(param$met)
  asv <- reactive(param$asv)
  tax <- reactive(param$tax)
  asv_transform <- reactive(param$asv_transform)
  
  # unpack bar plot inputs
  bar_tax <- reactive(param$bar_input$bar_tax)
  bar_y <- reactive(param$bar_input$bar_y)
  bar_x <- reactive(param$bar_input$bar_x)
  
  output$check <- renderPrint({
    bar_data()
  })
  
  # putting data into one dataframe
  work <- reactive({
    clr_gather <- asv_transform()
    clr_gather$featureID <- rownames(clr_gather)
    clr_gather <- clr_gather %>%
      gather('sampleID', 'clr_count')
    
    asv_gather <- asv() %>%
      gather('sampleID','read_count', -featureID) %>%
      inner_join(clr_gather, 'sampleID')
    
    met() %>%
      inner_join(asv_gather, 'sampleID') %>%
      inner_join(tax(), 'featureID')
  })

  # calculate output bar plot---------------------------------------------------

  bar_data <- reactive({
    work() %>%
      # sample total read count
      group_by(sampleID) %>%
      mutate(sample_total = sum(read_count)) %>%
      # aggregate on taxon within each sample
      group_by(sampleID, get(bar_tax())) %>%
      mutate(tax_cnt = sum(read_count), tax_rel = tax_cnt / sample_total) %>%
      ungroup() %>%
      # rename columns for simplicity
      ## don't have to work with reactive variables in tidy verbs
      rename('tax_column' = !!bar_tax(), 'x_column' = !!bar_x()) %>%
      distinct(tax_column, x_column, tax_cnt, tax_rel) %>%
      # mean of aggregated counts within selected group
      group_by(x_column, tax_column) %>%
      mutate(cnt_abund = mean(tax_cnt),
             rel_abund = mean(tax_rel)) %>%
      ungroup() %>%
      # revert column names back to user selection
      rename(!!bar_tax() := tax_column, !!bar_x() := x_column)
  })

  output$bar_title  <- renderText({

    if(bar_y() == 'rel_abund') {
      sprintf('Mean Relative Abundance (%%), %s', bar_tax())
    }
    else {
      sprintf('Mean Cumulative Read Count, %s', bar_tax())
    }
  })

  pdata <- reactive({
    bar_data() %>%
      # rename columns for simplicity
      ## don't have to work with reactive variables in tidy verbs
      rename('tax_column' = !!bar_tax(), 'x_column' = !!bar_x(), 
             'y_column' = !!bar_y()) %>%
      distinct(tax_column, x_column, y_column)
  })
  
  output$bar_table <- DT::renderDataTable({
    out <- spread(pdata(), x_column, y_column)

    if(bar_y() == 'rel_abund') {
      DT::datatable(out,  extensions = 'Buttons',
                    options = list(scrollX = TRUE,
                                   dom = 'Blfrtip', 
                                   buttons = c('copy','csv'))) %>%
        DT::formatRound(column = colnames(met())[1:ncol(met()], digits = 3)
    }
    else {
      DT::datatable(out, extensions = 'Buttons',
                    options = list(scrollX = TRUE,
                                   dom = 'Blfrtip', buttons = c('copy','csv')))
    }

  })

  p_bar <- reactive({
    
    p <- ggplot(pdata(), aes(x = x_column, y = y_column, fill = tax_column)) +
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

  output$dl_bar_original <- downloadHandler(
    fname <- function() {"ov_bar.tiff"},
    content <- function(file) {ggsave(file, plot=p_bar())}
  )

  output$dl_bar_html <- downloadHandler(
    fname <- function() {"ov_bar.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_bar())), file)
    }
  )

  output$dl_bar_data <- downloadHandler(
    fname <- function() {"ov_bar.csv"},
    content <- function(file) {
      readr::write_csv(bar_data(), file)
    }
  )

  output$dl_bar_rds <- downloadHandler(
    fname <- function() {"ov_bar.rds"},
    content <- function(file) {
      saveRDS(p_bar(), file)
    }
  )

  output$dl_bar_all <- downloadHandler(
    fname <- function() {"ov_bar.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_bar.tiff", "ov_bar.html","ov_bar.csv", "ov_bar.rds")
      ggsave(to_zip[1], plot=p_bar())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_bar())), to_zip[2])
      write.csv(pdata_bar(), to_zip[3])
      saveRDS(p_bar(), to_zip[4])

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
}
    
## To be copied in the UI
# mod_ov_bar_ui("ov_bar_ui_1")
    
## To be copied in the server
# callModule(mod_ov_bar_server, "ov_bar_ui_1")
 
