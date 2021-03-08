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
          menuItem('Task Info', tabName = 'info_tab_profile', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Microbiome Profile', tabName = "profile_tab"),
          menuItem('Report', tabName='profile_report_tab'),
          # Bar plot controls---------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'profile_tab'",
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = 'text-align: center', tags$b('Plot Parameters')),
              uiOutput(ns('bar_x_ui')),
              uiOutput(ns('bar_panel_ui')),
              selectInput(ns('bar_tax'), 'Taxonomic level:',
                          choices = c('featureID','Kingdom','Phylum',
                                      'Class', 'Order', 'Family','Genus',
                                      'Species', 'Taxon'),
                          selected = 'Phylum'),
              radioButtons(ns('bar_y'), 'Response measure:',
                           c('Relative abundance' = 'rel_abund',
                             'Read count' = 'cnt_abund')))
          )
        ) # end sidebar menu
      ), # end dashbaord sidebar
      # dashboard body----------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          tabItems(
            tabItem(
              tabName = "info_tab_profile",
              column(
                width=12,
                h1('Microbiome Profile'),
                p('Examine the relative abundance microbiome profile at various taxonomic levels.')
              )
            ), # end info tab
            tabItem(
              tabName = "profile_tab",
              column(
                width = 12,
                h1('Microbiome Profile'),
                h3(textOutput(ns('bar_title'))),
                p("Observing relative abundance or sequence abundance based on metadata variables. Abundance values can be aggregated at different taxonomic levels. The mean relative abundance is shown when selected group variable contains multiple samples"),
                DT::dataTableOutput(ns('bar_table'))  %>%
                  shinycssloaders::withSpinner()
              ),
              column(
                width = 12,
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
            ), # end profile tab
            tabItem(
              tabName = "profile_report_tab",
              mod_report_ui(ns("profile_report_ui"))
            ) # end report tab
          ) # end tabItems
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
    selectInput(ns('bar_x'), "x-axis",
                choices = colnames(improxy$work_db$met),
                selected = 'sampleID')
  })

  output$bar_panel_ui <- renderUI({
    selectInput(ns('bar_panel'), "panel by",
                choices = c('none', colnames(improxy$work_db$met)),
                selected = 'none')
  })


  # calculate output bar plot---------------------------------------------------
  bar_data <- reactive({
  req(input$bar_tax, input$bar_x, input$bar_panel)
    
    if(input$bar_panel == 'none') {
      out <- improxy$work_db$work %>%
        # sample total read count
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        # aggregate on taxon within each sample
        group_by(sampleID, !!sym(input$bar_tax)) %>%
        summarise(!!input$bar_x := .data[[input$bar_x]], 
                  tax_cnt = sum(read_count), 
                  tax_rel = tax_cnt / sample_total) %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_x), !!sym(input$bar_tax)) %>%
        summarise(cnt_abund = mean(tax_cnt),
                  rel_abund = mean(tax_rel))
    } else {
      out <- improxy$work_db$work %>%
        # sample total read count
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count)) %>%
        # aggregate on taxon within each sample
        group_by(sampleID, !!sym(input$bar_tax)) %>%
        summarise(!!input$bar_x := .data[[input$bar_x]], 
                  !!input$bar_panel := .data[[input$bar_panel]],
                  tax_cnt = sum(read_count), 
                  tax_rel = tax_cnt / sample_total) %>%
        # mean of aggregated counts within selected group
        group_by(!!sym(input$bar_x), !!sym(input$bar_tax)) %>%
        summarise(!!input$bar_panel := .data[[input$bar_panel]],
                  cnt_abund = mean(tax_cnt),
                  rel_abund = mean(tax_rel)) %>%
        distinct()
    }
    out
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

  bar_table <- reactive({
    req(input$bar_tax, input$bar_x, input$bar_y)
    if(input$bar_panel == 'none') {
      out <- bar_data() %>% 
        distinct(!!sym(input$bar_x), !!sym(input$bar_y), !!sym(input$bar_tax)) %>%
        spread(!!sym(input$bar_x), !!sym(input$bar_y))  
    } else {
      out <- bar_data() %>% 
        distinct(!!sym(input$bar_x), !!sym(input$bar_panel), 
                 !!sym(input$bar_y), !!sym(input$bar_tax)) %>%
        unite(x, !!sym(input$bar_x), !!sym(input$bar_panel)) %>%
        spread(x, !!sym(input$bar_y))  
    }
    out
  })
  output$bar_table <- DT::renderDataTable({
    
    out <- bar_table()
    x_name <- colnames(out)
    x_name <- x_name[x_name != input$bar_tax]

    out <- DT::datatable(out,  extensions = 'Buttons',
                         options = list(
                           scrollX = TRUE,
                           dom = 'Blfrtip',
                           buttons = c('copy','csv')))
    if(input$bar_y == 'rel_abund') {
       out %>%
        DT::formatRound(column = x_name, digits = 3)
    }
    else {
      out
    }
  })

  p_bar <- reactive({
    req(input$bar_tax, input$bar_x, input$bar_y, input$bar_panel)
    p <- ggplot(bar_data(), aes_string(x = input$bar_x, y = input$bar_y, 
                                       fill = input$bar_tax)) +
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

    if(input$bar_panel != 'none') {
      panel_formula = formula(paste("~", input$bar_panel))
      p <- p + facet_wrap(panel_formula, scales='free')
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
    for_download$fig_data <- bar_data()
  })

  callModule(mod_download_server, "download_bar", bridge = for_download, 'bar')
  
  # output$check <- renderPrint({
  #   print(for_report$params$sample_select_prompt)
  # })
  
  # initiate parameters to send to report
  for_report <- reactiveValues()
  observe({
    for_report$params <- list(
      met = improxy$work_db$met,
      sample_select_prompt = improxy$work_db$sample_select_prompt,
      sample_select = improxy$work_db$sample_select,
      bar_y = input$bar_y,
      bar_x = input$bar_x,
      bar_tax = input$bar_tax,
      bar_table = bar_table(),
      p_bar = p_bar()
    )
  })
  # build report
  callModule(mod_report_server, "profile_report_ui", bridge = for_report,
             template = "profile_report",
             file_name = "profile_report")
}

## To be copied in the UI
# mod_profile_ui("profile_ui_1")

## To be copied in the server
# callModule(mod_profile_server, "profile_ui_1")

