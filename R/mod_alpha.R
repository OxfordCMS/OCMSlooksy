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
#' @import tibble
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
          menuItem('Report', tabName = 'alpha_report'),
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
                      selected = 'asv_by_count')
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
          wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
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
            ),
            # report------------------------------------------------------------
            tabItem(
              tabName = "alpha_report",
              mod_report_ui(ns("alpha_report_ui"))
            ) # end tabitem
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
  

  
  # render controls - alpha diversity-------------------------------------------
  output$alpha_grp_ui <- renderUI({
    radioButtons(ns('alpha_grp'), "Compare Sample Groups",
                 choices = colnames(cross_mod$filtered$met), selected = 'sampleID')
  })
  
  # calculate alpha diversity---------------------------------------------------
  
  alpha_result <- reactive({
    req(input$alpha_grp)
    alpha_data <- cross_mod$filtered$asv %>% 
      column_to_rownames('featureID')
    
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
  grp_tally <- reactive({
    req(input$alpha_grp)
    table(cross_mod$filtered$met[,input$alpha_grp])
  })
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
  
  validation_msg <- reactive({
    if(max(grp_tally()) == 1) {
      "Only one observation per group. Group-wise comparisons not performed"
    } else if(length(grp_tally()) == 1) {
      "All observations are in the same group. Group-wise comparisons not performed"
    } else {
      'valid'
    }
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
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_metric, alpha_value, alpha_avg) %>%
      ungroup()
    
    if(validation_msg() == 'valid') {
      # re-calculate comparison statistic
      compare_stat <- alpha_result() %>%
        gather('alpha_metric', 'alpha_value', -sampleID) %>%
        inner_join(cross_mod$filtered$met %>% 
                     gather('meta_variable','grouping', -sampleID),
                   'sampleID') %>%
        filter(meta_variable == input$alpha_grp)
      
      compare_stat <- ggpubr::compare_means(formula = alpha_value~grouping, 
                                            data = compare_stat,
                                            group.by = c('alpha_metric'),
                                            method = stat_test(), p.adjust.method = 'BH')
      
      out <- out %>%
        left_join(compare_stat %>% select(alpha_metric, p.adj), 'alpha_metric') %>%
        mutate(panel = sprintf("%s\np.adj=%0.3f", alpha_metric, p.adj))
    }
      
    out
  })
  p_alpha <- reactive({
    req(input$alpha_grp)
    
    p <- ggplot(pdata_alpha(), aes(x = .data[[input$alpha_grp]], 
                                   y = alpha_value, 
                                   group = .data[[input$alpha_grp]]))
    
    if(validation_msg() == 'valid') {
      p <- p + facet_wrap(~panel, scales = 'free')
    } else {
      p <- p + facet_wrap(~alpha_metric, scales = 'free')
    }
      
    if(min(grp_tally()) > 5) {
      p <- p +
        geom_boxplot(outlier.fill=NA) +
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
  
  output$check <- renderPrint({
    print(validation_msg())
    print(summary(for_report$params))
  })
  # initiate list to pass onto report submodule
  for_report <- reactiveValues()
  observe({
    for_report$params <- list(met1 = improxy$work_db$met,
                              tax1 = improxy$work_db$tax,
                              sample_select_prompt = improxy$work_db$sample_select_prompt,
                              sample_select = improxy$work_db$sample_select,
                              asv_select_prompt = input$asv_select_prompt,
                              asv_filter_options = input$asv_filter_options,
                              cutoff_method = cross_mod$params$cutoff_method,
                              asv_cutoff = cross_mod$params$asv_cutoff,
                              prevalence = cross_mod$params$prevalence,
                              asv_cutoff_msg = cross_mod$params$asv_cutoff_msg,
                              asv_remove = cross_mod$params$asv_remove,
                              prev_agg_plot = cross_mod$params$prev_agg_plot,
                              prev_read_plot = cross_mod$params$prev_read_plot,
                              empty_sample = cross_mod$params$empty_sample,
                              empty_asv = cross_mod$params$empty_asv,
                              met2 = cross_mod$filtered$met,
                              tax2 = cross_mod$filtered$tax,
                              alpha_result = alpha_result(),
                              validation_msg = validation_msg(),
                              p_alpha = p_alpha()
    )
  })
  
  observe({
    if(validation_msg() == 'valid') {
      for_report$params[['alpha_stat']] <- alpha_stat()
    }
  })
  
  # build report
  callModule(mod_report_server, "alpha_report_ui", bridge = for_report,
             template = "alpha_report",
             file_name = "alpha_report")
}
    
## To be copied in the UI
# mod_alpha_ui("alpha_ui_1")
    
## To be copied in the server
# callModule(mod_alpha_server, "alpha_ui_1")
 
