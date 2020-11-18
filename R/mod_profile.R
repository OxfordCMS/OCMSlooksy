#' profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_profile_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(
          id = 'menu', br(),
          # Bar plot controls---------------------------------------------------
          fixedPanel(
            width = 225,
            tags$div(style = 'text-align: center', tags$b('Plot Parameters')),
            uiOutput(ns('bar_x_ui')),
            selectInput(ns('bar_tax'), 'Taxonomic level:',
                        choices = c('featureID','Kingdom','Phylum',
                                    'Class', 'Order', 'Family','Genus',
                                    'Species', 'Taxon'),
                        selected = 'Phylum'),
            radioButtons(ns('bar_y'), 'Response measure:',
                         c('Relative abundance' = 'rel_abund',
                           'Read count' = 'cnt_abund')))
        ) # end sidebar menu
      ), # end dashbaord sidebar
      # dashboard body----------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          
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
        ) # end box
      ) # end dashbaoad body
    ) # end dashboard Page
  ) # end taglist
}
    
#' profile Server Function
#'
#' @noRd 
mod_profile_server <- function(input, output, session, improxy){
  ns <- session$ns
 
  # render controls bar plot
  output$bar_x_ui <- renderUI({
    selectInput(ns('bar_x'), "x-axis", choices = colnames(improxy$work_db$met),
                selected = 'sampleID')
  })
  
 
  # calculate output bar plot---------------------------------------------------
  
  bar_data <- reactive({
    req(input$bar_tax, input$bar_x)
    improxy$work_db$work %>%
      # sample total read count
      group_by(sampleID) %>%
      mutate(sample_total = sum(read_count)) %>%
      # aggregate on taxon within each sample
      group_by(sampleID, !!sym(input$bar_tax)) %>%
      mutate(tax_cnt = sum(read_count), tax_rel = tax_cnt / sample_total) %>%
      ungroup() %>%
      distinct(!!sym(input$bar_tax), !!sym(input$bar_x), tax_cnt, tax_rel) %>%
      # mean of aggregated counts within selected group
      group_by(!!sym(input$bar_x), !!sym(input$bar_tax)) %>%
      mutate(cnt_abund = mean(tax_cnt),
             rel_abund = mean(tax_rel)) %>%
      ungroup() 
  })
  
  output$bar_title  <- renderText({
    req(input$bar_y)
    if(input$bar_y == 'rel_abund') {
      sprintf('Mean Relative Abundance (%%), %s', input$bar_tax)
    }
    else {
      sprintf('Mean Cumulative Read Count, %s', input$bar_tax)
    }
  })
  
  # output$check <- renderPrint({
  # 
  # })
  
  pdata <- reactive({
    req(input$bar_tax, input$bar_x, input$bar_y)
    bar_data() %>%
      distinct(!!sym(input$bar_tax), !!sym(input$bar_x), !!sym(input$bar_y))
  })
  
  
  output$bar_table <- DT::renderDataTable({
    req(input$bar_tax, input$bar_x, input$bar_y)
    out <- pdata() %>% spread(!!sym(input$bar_x), !!sym(input$bar_y))
    x_name <- colnames(out)
    x_name <- x_name[x_name != input$bar_tax]
    
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
    
    
    if(input$bar_y == 'rel_abund') {
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
    req(input$bar_tax, input$bar_x, input$bar_y)
    p <- ggplot(pdata(), aes_string(x = input$bar_x, y = input$bar_y, fill = input$bar_tax)) +
      geom_bar(stat = 'identity') +
      xlab(input$bar_x) +
      scale_fill_discrete(name = input$bar_tax) +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    if(input$bar_y == 'rel_abund') {
      p <- p +
        ylab(sprintf('Mean Relative Abundance (%%), %s', input$bar_tax))
    }
    else {
      p <- p +
        ylab(sprintf('Mean Read Count, %s', input$bar_tax))
    }
    p
  })
  
  output$bar_plot <- renderPlotly({
    ggplotly(p_bar())
  })
  
  # download data
  for_download <- reactiveValues()
  observe({
    req(input$bar_tax, input$bar_y, input$bar_x)
    for_download$figure <- p_bar()
    for_download$fig_data <- pdata()
  })
  
  callModule(mod_download_server, "download_bar", bridge = for_download, 'bar')
}
    
## To be copied in the UI
# mod_profile_ui("profile_ui_1")
    
## To be copied in the server
# callModule(mod_profile_server, "profile_ui_1")
 
