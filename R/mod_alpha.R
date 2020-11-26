#' alpha UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs
mod_alpha_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(
          id = 'menu', br(),
          menuItem('Task Info', tabName = 'info_tab_alpha', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Filter Features', tabName = 'filter_asv_alpha'),
          menuItem('\u03B1-Diversity Analysis', tabName = 'alpha_tab'),
          # filter menu controls--------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'filter_asv_alpha'",
            br(), hr(),
            tags$div(
              style = 'text-align: center',
              tags$b('Input controls')),
            
            fixedPanel(
              radioButtons(ns('asv_select_prompt'), 
                           "Features to exclude from analysis:",
                           choices = c('Use all features' = 'all', 
                                       'Filter features' = 'some'),
                           selected = 'all'),
              
              hidden(
                div(id = ns('asv_filter_options_ui'),
                    radioButtons(
                      ns('asv_filter_options'), 'Filter features by:',
                      choices = c('read count' = 'asv_by_count',
                                  'selection' = 'asv_by_select'),
                      selected = character(0))
                )),
              actionButton(ns('submit_asv'), "Filter features"))
              # withBusyIndicatorUI(
              #  )  
          )
        ) # end sidebar menu
      ), # end sidebar
      # dashboard body----------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          tabItems(
            # info tab body-----------------------------------------------------
            tabItem(
              tabName = 'info_tab_overview',
              h1("\u03B1-Diversity")
            ), # end tabItem
            # filter tab body---------------------------------------------------
            tabItem(
              tabName = "filter_asv_alpha",
              mod_filterfeat_ui(ns("filterfeat_ui_alpha"))
            ), # end tabItem
            # alpha tab body----------------------------------------------------
            tabItem(
              tabName = 'alpha_tab',

              h1("\u03B1-Diversity"),
              tags$div("Alpha diversity assesses the diversity of sets of communities (or sets of samples). Species richness is the number of unique species. Species evenness is a measure of the consistency of species abundances (uneven data sets have community members that dominate in abundance). Entropy measures such as Shannon entropy and Simpson index are measures of uncertainty in the species identity of a sample [Jost 2006]. Diversity measures, such as Shannon's Diveristy and Inverse Simpson's Index, takes into account of the abundance of species in the community. In fact, when all species in a community are equally common, entropy and diveristy measures are equivalent. Entropy indeces can be converted to diversity by mathematical transformation."),
              fluidRow(
                DT::dataTableOutput(ns('alpha_table'))  %>%
                  shinycssloaders::withSpinner()
              ), br(),
              fluidRow(
                width = 12,
                DT::dataTableOutput(ns('alpha_test'))  %>%
                  shinycssloaders::withSpinner()
              ), br(),
              fluidRow(
                column(
                  width = 3, br(), br(),
                  wellPanel(uiOutput(ns('alpha_grp_ui')))
                ),
                column(
                  width = 9,
                  column(
                    width = 1, style = 'padding:0px;',
                    mod_download_ui(ns('download_alpha')),
                  ),
                  column(
                    width = 11, style = 'padding:0px;',
                    shinyjqui::jqui_resizable(
                      plotlyOutput(ns('alpha_plot'), width = '100%', 
                                   height= 'auto') %>% 
                        shinycssloaders::withSpinner()
                    )
                  )  
                )
              )
            )
          ) # end tabItems
          
        ) # end box
      ) # end dashbaoad body
    ) # end dashboard Page
  ) # end taglist
}
    
#' alpha Server Function
#'
#' @noRd 
mod_alpha_server <- function(input, output, session, improxy){
  ns <- session$ns
 
  # store data in reactiveValues to pass onto submodule-------------------------
  bridge <- reactiveValues()
  observe({
    bridge$work_db <- improxy$work_db
  })
  
  # fitler features-------------------------------------------------------------
  # render sidebar controls - filter yes/no
  observeEvent(input$asv_select_prompt, {
    toggle("asv_filter_options_ui", condition = input$asv_select_prompt == 'some')
  })
  
  # pass in filter menu controls
  bridge$filter_input <- reactiveValues()
  observe({
    bridge$filter_input$asv_select_prompt <- input$asv_select_prompt
    bridge$filter_input$asv_filter_options <- input$asv_filter_options
    bridge$filter_input$submit_asv <- input$submit_asv
  })
  
  withBusyIndicatorServer('submit_asv', 'alpha_ui_1', {
    cross_mod <- callModule(mod_filterfeat_server, "filterfeat_ui_alpha", bridge=bridge)
  })
  
  output$check <- renderPrint({

  })
  
  # render controls - alpha diversity-------------------------------------------
  output$alpha_grp_ui <- renderUI({
    radioButtons(ns('alpha_grp'), "Compare Sample Groups",
                 choices = colnames(cross_mod$filtered$met), selected = 'sampleID')
  })
  
  # calculate alpha diversity---------------------------------------------------
  
  alpha_result <- reactive({
    req(input$alpha_grp)
    alpha_data <- cross_mod$filtered$asv %>% select(-featureID)
    alpha_data <- as.data.frame(alpha_data)
    rownames(alpha_data) <- cross_mod$filtered$asv$featureID
    
    shannon <- vegan::diversity(alpha_data,index = 'shannon',
                                base = 2, MARGIN = 2)
    shannon_d <- exp(shannon)
    richness <- vegan::specnumber(alpha_data, MARGIN = 2)
    evenness <- shannon / log(richness)
    invsimpson <- vegan::diversity(alpha_data,index = 'invsimpson',
                                   base = 2, MARGIN = 2)
    simpson <- vegan::diversity(alpha_data, index = 'simpson',
                                base = 2, MARGIN = 2)
    
    out <- data.frame(sampleID = names(shannon),
                      shannon = shannon,
                      simpson = simpson,
                      shannon_d = shannon_d,
                      richness = richness,
                      evenness = evenness,
                      invsimpson = invsimpson)
    out
  })
  
  # determine valid stat test
  grp_tally <- reactive(table(cross_mod$filtered$met[,input$alpha_grp]))
  stat_test <- reactive({
    if(length(grp_tally()) == 2) 'wilcox.test'
    else 'kruskal.test'
  })
  
  alpha_stat <- eventReactive(input$alpha_grp, {
    
    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )
    
    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      inner_join(cross_mod$filtered$met %>% 
                   gather('meta_variable','grouping', -sampleID),
                 'sampleID') %>%
      filter(meta_variable == input$alpha_grp)
    
    out <- ggpubr::compare_means(formula = alpha_value~grouping, data = out,
                                 group.by = c('alpha_metric'),
                                 method = stat_test(), p.adjust.method = 'BH')
    out
  })
  
  # show tables
  output$alpha_table <- DT::renderDataTable({
    out <- cross_mod$filtered$met %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID')
    DT::datatable(out, extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(alpha_result())[2:ncol(alpha_result())], digits = 3)
  })
  
  output$alpha_test <- DT::renderDataTable({
    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )
    
    DT::datatable(alpha_stat() %>%
                    select(-.y., -p.format, ), extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = 'p', digits = 3)
  })
  
  # plot alpha diversity
  pdata_alpha <- eventReactive(input$alpha_grp, {
    
    # set xorder based on shannon_d
    xorder <- cross_mod$filtered$met %>%
      mutate_all(as.character) %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID') %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(shannon_d)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], 
                                      desc(alpha_avg)))
    
    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      inner_join(cross_mod$filtered$met %>% mutate_all(as.character), 'sampleID') %>%
      group_by(alpha_metric, .data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value),
             x = factor(.data[[input$alpha_grp]], 
                        levels = levels(xorder$x))) %>%
      distinct(x, alpha_metric, alpha_value, alpha_avg) %>%
      ungroup()
    out
  })
  p_alpha <- reactive({
    req(input$alpha_grp)
    
    p <- ggplot(pdata_alpha(), aes(x = x, y = alpha_value, group = x)) +
      facet_wrap(~alpha_metric, scales = 'free')
    
    if(max(grp_tally()) > 1) {
      # geom_GeomSignif not compatible with plotly
      # compare_pairs <- combn(names(grp_tally()), 2, simplify = FALSE)
      
      ymax = pdata_alpha() %>%
        group_by(alpha_metric) %>%
        mutate(ymax = max(alpha_value), yupper = ymax * 1.04,
               ymin = min(alpha_value), ylower = ymin * 0.99)
      
      p <- p +
        ggpubr::stat_compare_means(method = stat_test(), label = 'p.format',
                                   size = 3) +
        geom_blank(data = ymax, aes(y = yupper)) +
        geom_blank(data = ymax, aes(y = ylower))
    }
    
    if(min(grp_tally()) > 5) {
      p <- p +
        geom_point(position = position_jitter(width = 0.25, seed = 1), 
                   alpha = 0.6)
    }
    else {
      p <- p +
        geom_point(alpha = 0.6)
    }
    
    p <- p +
      theme_bw() +
      xlab(input$alpha_grp) +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank())
    
    p
  })
  
  output$alpha_plot <- renderPlotly({
    ggplotly(p_alpha())
  })
  
  
  # download data
  for_download <- reactiveValues()
  observe({
    req(input$alpha_grp)
    for_download$figure <- p_alpha()
    for_download$fig_data <- pdata_alpha()
  })
  
  callModule(mod_download_server, "download_alpha", bridge = for_download, 'alpha')
  
}
    
## To be copied in the UI
# mod_alpha_ui("alpha_ui_1")
    
## To be copied in the server
# callModule(mod_alpha_server, "alpha_ui_1")
 
