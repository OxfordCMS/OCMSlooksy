# Module UI

#' @title   mod_overview_ui and mod_overview_server
#' @description  Overview of dataset using exploratory analysis
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_explore
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import plotly
#' @import ALDEx2
#' @import ggfortify
#' @import heatmaply
#' @import ggdendro
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(id = 'menu', br(),br(), br(),
          menuItem('Main Page', tabName = 'main_tab_overview', selected = TRUE),
          menuItem('Relative Abundance', tabName = 'bar_tab'),
          menuItem('Multivariate Analysis'),
                   menuSubItem('PCA', tabName = 'pca_tab'),
                   menuSubItem('PCoA', tabName = 'pcoa_tab'),
                   menuSubItem('NMDS', tabName = 'nmds_tab'),
          menuItem('\u03B1-Diversity Analysis', tabName = 'alpha_tab'),
          menuItem('Cluster Analysis', tabName = 'hmap_tab'),
          
          # Bar plot controls---------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'bar_tab'",
            fixedPanel(
              width = 225,
                tags$div(style = 'text-align: center', tags$b('Plot Parameters')),
                uiOutput(ns('bar_x_ui')),
                selectInput(ns('bar_tax'), 'Taxonomic level:',
                            choices = c('ASV','Phylum','Class','Order',
                                        'Family','Genus','Species'),
                            selected = 'ASV'),
                radioButtons(ns('bar_y'), 'Response measure:',
                             c('Relative abundance' = 'rel_abund',
                               'Read count' = 'cnt_abund')))
          ),
          # Multivariate analysis-----------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pca_tab'",
            div(id = ns('pca_param_div'),
                br(), hr(),
                fixedPanel(
                  width = 225,
                  tags$div(style = "text-align: center", tags$b('PCA Parameters')),
                  radioButtons(ns('pca_scale'), "Scale",
                               choices = c("unit-variance scaling" = 'UV',
                                           "pareto scaling" = 'pareto',
                                           "vast scaling" = 'vast'),
                               selected = 'UV'),
                  actionButton(ns('pca_calculate'), "Calculate")
                  ))
            ),

          conditionalPanel(
            condition = "input.menu === 'pcoa_tab'",
            div(id = ns('pcoa_param_div'),
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b('PCoA Parameters')),
              actionButton(ns('pcoa_calculate'), "Calculate")
            ))
          ),

          conditionalPanel(
            condition = "input.menu === 'nmds_tab'",
            div(id = ns('nmds_param_div'),
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b('NMDS Parameters')),
              br(), br(), br(),
              actionButton(ns('nmds_calculate'), "Calculate")
            ))
          ),
        
          # # Alpha-diversity-------------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'alpha_tab'",
            div(id = ns('alpha_param_div'),
                br(), hr(),
                fixedPanel(
                  width = 225,
                  tags$div(style = "text-align: center", 
                           tags$b("\u03B1-Diversity Parameters")),
                  selectInput(ns('alpha_method'), "Diversity Metric",
                              choices = list(
                                          `Entropy Measures` =
                                            list("Shannon-Weaver Index (H)"="shannon",
                                                 "Simpson Index (D1)" = "simpson"),
                                          `Diversity Measures` =
                                            list("Shannon (H'), q = 1" = "shannon_d",
                                                 "Inverse Simpson (D2), q = 2" = "invsimpson",
                                                 "Species Richness (S), q = 0" = "richness",
                                                 "Species Evenness (J)" = "evenness"))),
                  br(),br(),br(),
                  actionButton(ns('alpha_calculate'), "Calculate")
                ))
            ),

          # Heat map--------------------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'hmap_tab'",
            div(id = ns('hmap_param_div'),
                br(), hr(),
                fixedPanel(
                  width = 225,
                  tags$div(style = "text-align: center",
                           tags$b("Heirchical Cluster Parameters")),
                  selectInput(ns('hclust_method'), "Linkage method",
                              choices = c('complete','ward.D','ward.D2','single',
                                          'average','mcquitty','median','centroid'),
                              selected = 'complete'),
                  selectInput(ns('dist_method'), "Distance method",
                              choices = c('euclidean','maximum','manhattan',
                                          'canberra','binary','minkowski'),
                              selected = 'euclidean'),
                  actionButton(ns('hmap_calculate'), 'Calculate')
                  ))
            )
        ## end of side bar------------------------------------------------------
        )),
      # body--------------------------------------------------------------------
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          
          # wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          
          tabItems(
            # main page---------------------------------------------------------
            tabItem(
              tabName = 'main_tab_overview',
              h1('Overview of data set'),
              column(width = 12, "A suite of tools to perform exploratory analysis in order to get an overall sense of the data set.")
            ),
            # bar plot body--------------------------------------------------------
            tabItem(
              tabName = 'bar_tab',
                h1('Relative Distribution of Taxa'),
                column(width = 12,
                  h3(textOutput(ns('bar_title'))),
                  DT::dataTableOutput(ns('bar_data'))),
                column(width = 12,
                  shinyjqui::jqui_resizable(
                    plotlyOutput(ns('bar_plot'), width = '100%', height = 'auto'))
                  )
              ),
            
            # multivariate analysis body---------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
              column(width = 12,
                h1('Principle Component Analysis')),
              column(width = 12,
                h2('Summary of PCA'),
                DT::dataTableOutput(ns('summary_pca'))),
              
              hidden(div(id = ns('mva_param_div'),
                h2('PCA Plot'),
                wellPanel(
                  tags$div(style = 'text_align: center', h3("Plot Parameters")),
                  fluidRow(
                    # Plot controls
                    column(width = 6,
                      div(style = "display: inline-block;vertical-align: top",
                          uiOutput(ns('xPC_ui'))),
                      div(style = "display: inline-block;vertical-align: top",
                          uiOutput(ns('yPC_ui'))),
                      div(style = "display: inline-block;vertical-align: top",
                          checkboxInput(ns('show_loading'), "Show loadings", TRUE)),
                      div(style = "display: inline-block;vertical-align: top",
                          checkboxInput(ns('load_arrow'), 'Show loading arrows', TRUE))
                      ),
                    column(width = 12, hr()),
                    column(width = 3,
                      # score point aesthetics
                      h3("Score points aesthetics"),
                      uiOutput(ns('score_pt_colour_ui')),
                      uiOutput(ns('score_pt_shape_ui')),
                      div(style = "display: inline-block;vertical-align: top",
                          numericInput(ns('score_pt_size'), 'Point size:',
                                       min = 0.1, max = 5, value = 3, step = 0.5)),
                      div(style = "display: inline-block;vertical-align: top",
                        numericInput(ns('score_pt_alpha'), 'Point transparency:',
                                     min = 0.1, max = 1, value = 1, step = 0.1))
                      ),
                    # score label aesthetics
                    column(width = 3,
                      h3("Score labels aesthetics"),
                      uiOutput(ns('score_label_ui')),
                      uiOutput(ns('score_lab_colour_ui')),
                      div(style = "display: inline-block;vertical-align: top",
                          numericInput(ns('score_lab_size'), 'Label size:',
                                       min = 0.1, max = 5, value = 3, step = 0.5)),
                      div(style = "display: inline-block;vertical-align: top",
                          numericInput(ns('score_lab_alpha'), 'Label transparency:',
                                       min = 0.1, max = 1, value = 1, step = 0.1))
                      ),
                    
                    hidden(div(id=ns('loading_div'),
                      column(width = 3,
                        # loading point aesthetics
                        h3('Loading points aesthetics'),
                        uiOutput(ns('load_pt_colour_ui')),
                        uiOutput(ns('load_pt_shape_ui')),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_pt_size'), 'Point size:',
                                         min = 0.1, max = 5, value = 3, step = 0.5)),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_pt_alpha'), 'Point transparency:',
                                         min = 0.1, max = 1, value = 1, step = 0.1))
                        ),
                      # loading label aesthetics
                      column(width = 3,
                        h3('Loading labels aethetics'),
                        uiOutput(ns('load_label_ui')),
                        uiOutput(ns('load_lab_colour_ui')),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_lab_size'), 'Label size:',
                                         min = 0.1, max = 5, value = 3, step = 0.5)),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_lab_alpha'), 'Label transparency:',
                                         min = 0.1, max = 1, value = 1, step = 0.1))
                        )
                      ))
                  ))
                )),
                column(width = 12,
                       shinyjqui::jqui_resizable(
                         plotlyOutput(ns('plot_pca'), width = '100%', height = 'auto')))
            ),
        
            # alpha diversity body----------------------------------------------
            tabItem(
              tabName = 'alpha_tab',
              h1("\u03B1-Diversity"),
              column(width = 12,
                     DT::dataTableOutput(ns('alpha_table'))),
              hidden(div(id = ns('alpha_body_div'),
                column(width = 3,
                  wellPanel(
                    uiOutput(ns('alpha_grp_ui')))),
                column(width = 9,
                       shinyjqui::jqui_resizable(
                         plotlyOutput(ns('alpha_plot'))
                       )),
                column(width = 12,
                       DT::dataTableOutput(ns('alpha_test')))
              ))
            ),
            # heatmap body------------------------------------------------------
            tabItem(
              tabName = 'hmap_tab',
              hidden(div(id = ns('hmap_body_div'),
                h1('Heirarchical Clustering'),
                column(width = 12,
                  column(width = 3,
                    wellPanel(
                      numericInput(ns('hmap_samp_k'), "Number of clusters, k",
                                   value = 1, min = 1, step = 1),
                      uiOutput(ns('hmap_samp_label_ui')),
                      uiOutput(ns('hmap_samp_colour_ui'))
                    )),
                  column(width = 2,
                        plotOutput(ns('sample_dendro_leg'))),
                  column(width = 7,
                         h3('Sample dendrogram'),
                         shinyjqui::jqui_resizable(
                           plotlyOutput(ns('sample_dendro_plot'))))),
                column(width = 12,
                  column(width = 3,
                    wellPanel(
                      numericInput(ns('hmap_asv_k'), "Number of clusters, k", 
                                   value = 1, min = 1, step = 1),
                      uiOutput(ns('hmap_asv_label_ui')),
                      uiOutput(ns('hmap_asv_colour_ui'))
                      )),
                  column(width = 2,
                         plotOutput(ns('asv_dendro_leg'))),
                  column(width = 7,
                    h3('Taxonomy dendrogram'),
                    shinyjqui::jqui_resizable(
                      plotlyOutput(ns('asv_dendro_plot'))))),
                
                h2('Heat map'),
                wellPanel(  
                  fluidRow(
                    column(width = 3,
                      radioButtons(ns('sample_as_x'), "Show samples along:",
                                   choices = c('x-axis' = TRUE, 'y-axis' = FALSE),
                                   selected = TRUE)),
                      
                    column(width = 3,
                      checkboxGroupInput(ns('show_dendro'), 'Show dendrogram',
                                       choices = c('x-axis' = 'show_dendro_x',
                                                   'y-axis' = 'show_dendro_y'),
                                       selected = c('show_dendro_x', 'show_dendro_y'))),
                    column(width = 3,
                      selectInput(ns('hmap_tax_label'), 'Label taxa by:',
                                  choices = c('ASV','Taxon','Species')))
                  )),
                column(width = 12,
                       shinyjqui::jqui_resizable(
                         plotlyOutput(ns('hmap_plot'), 
                                      width = '100%', height = 'auto')))
                ))
              )
            # end of dashboard body---------------------------------------------
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_explore
#' @export
#' @keywords internal

mod_overview_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  # import data into module
  working_set <- reactive(improxy$data_db)
  
  met <- reactive(working_set()$metadata)
  asv <- reactive(working_set()$asv)
  asv_transform <- reactive(working_set()$t_asv)
  tax <- reactive(working_set()$tax)
  
  met_var <- reactive({
    out <- colnames(met())
    out <- out[out != 'sampleID']
  })
  
  tax_bar <- reactive({
    out <- colnames(tax())
    out <- out[!out %in% c('ASV','Taxon','Species')]
  })
  # putting data into one dataframe---------------------------------------------
  work <- reactive({
    met() %>%
      inner_join(asv() %>% gather('sampleID','read_count', -Taxon), 'sampleID') %>%
      inner_join(tax(), 'Taxon')
  })
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(input$pca_calculate, {
    show('mva_param_div')
  })

  observeEvent(input$show_loading, {
    toggle('loading_div')
  })
  
  observeEvent(input$alpha_calculate, {
    show('alpha_body_div')
  })
  observeEvent(input$hmap_calculate, {
    show('hmap_body_div')
  })
  
  ## render controls bar plot---------------------------------------------------
  output$bar_x_ui <- renderUI({
    selectInput(ns('bar_x'), "x-axis", choices = colnames(met()),
                selected = 'sampleID')
  })
  
  ## render controls - multivariate analysis------------------------------------
  ### choose PCs to plot
  output$xPC_ui <- renderUI({
    numericInput(ns('xPC'), 'x-axis PC', value = 1, 
                 min = 1, max = nrow(d_pcx()$x), step = 1)
  })
  output$yPC_ui <- renderUI({
    numericInput(ns('yPC'), 'y-axis PC', value = 2,
                 min = 1, max = nrow(d_pcx()$x), step = 1)
  })
  ### score point aesthetics
  output$score_pt_colour_ui <- renderUI({
    selectInput(ns('score_pt_colour'), 'Point colour:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  output$score_pt_shape_ui <- renderUI({
    selectInput(ns('score_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  
  ### score label aethetics
  output$score_label_ui <- renderUI({
    selectInput(ns('score_label_by'), 'Label scores by:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  output$score_lab_colour_ui <- renderUI({
    selectInput(ns('score_lab_colour'), 'Label colour:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  
  ### loading points aesthetics
  output$load_pt_colour_ui <- renderUI({
    selectInput(ns('load_pt_colour'), 'Point colour:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_pt_shape_ui <- renderUI({
    selectInput(ns('load_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  ### loading labels aesthetics
  output$load_label_ui <- renderUI({
    selectInput(ns('load_label_by'), 'Label loadings by:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_lab_colour_ui <- renderUI({
    selectInput(ns('load_lab_colour'), 'Label colour:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })
  output$load_lab_shape_ui <- renderUI({
    selectInput(ns('load_lab_shape'), 'Label shape:', 
                choices = c('none', colnames(tax())), selected = 'none')
  })

  ## render controls - alpha diversity------------------------------------------
  output$alpha_grp_ui <- renderUI({
    radioButtons(ns('alpha_grp'), "Compare Sample Groups",
                       choices = colnames(met()), selected = 'sampleID')
  })
  ## render controls - heat map-------------------------------------------------
  output$hmap_samp_label_ui <- renderUI({
    selectInput(ns('hmap_samp_label'), "Label:",
                choices = colnames(met()), selected = 'sampleID')
  })
  output$hmap_samp_colour_ui <- renderUI({
    radioButtons(ns('hmap_samp_colour'), "Show sample metadata:",
                 choices = c('none', met_var()),
                 selected = 'none')
  })
  
  output$hmap_asv_label_ui <- renderUI({
    selectInput(ns('hmap_asv_label'), "Label:",
                choices = colnames(tax()), selected = 'Taxon')
  })
  
  output$hmap_asv_colour_ui <- renderUI({
    choices <- c('none', colnames(tax()))
    choices <- choices[!choices %in% c('sequence','ASV','Taxon')]
    radioButtons(ns('hmap_asv_colour'), "Show taxonomy level:",
                 choices = choices, selected = 'none')
  })
  # calculate output bar plot---------------------------------------------------
  pdata <- reactive({
    work() %>%
      # sample total read count
      group_by(sampleID) %>%
      mutate(sample_total = sum(read_count)) %>%
      # aggregate on taxon within each sample
      group_by(sampleID, .data[[input$bar_tax]]) %>%
      mutate(tax_cnt = sum(read_count), tax_rel = tax_cnt / sample_total) %>%
      ungroup() %>%
      distinct(.data[[input$bar_tax]], .data[[input$bar_x]], 
               .data[[input$bar_y]], tax_cnt, tax_rel) %>%
      # mean of aggregated counts within selected group
      group_by(.data[[input$bar_x]], .data[[input$bar_tax]]) %>%
      mutate(cnt_abund = mean(tax_cnt),
             rel_abund = mean(tax_rel)) %>%
      ungroup()

    })
    
    output$bar_title  <- renderText({
      
      if(input$bar_y == 'rel_abund') {
        req(input$bar_y)
        sprintf('Mean Relative Abundance (%%), %s', input$bar_tax)
      }
      else {
        req(input$bar_tax)
        sprintf('Mean Cumulative Read Count, %s', input$bar_tax)
      }
    })
    
    output$bar_data <- DT::renderDataTable({
      req(input$bar_y, input$bar_tax, input$bar_x)
      
      out <-  pdata() %>%
        distinct(.data[[input$bar_tax]], .data[[input$bar_x]], .data[[input$bar_y]]) %>%
        spread(.data[[input$bar_x]], .data[[input$bar_y]])
        
      if(input$bar_y == 'rel_abund') {
        DT::datatable(out, options = list(scrollX = TRUE)) %>%
          DT::formatRound(column = met()[,input$bar_x], digits = 3)
      }
      else {
        DT::datatable(out, options = list(scrollX = TRUE))
      }
      
    })
    
    output$bar_plot <- renderPlotly({
      req(input$bar_y, input$bar_tax, input$bar_x)
      p <- ggplot(pdata() %>%
                    distinct(.data[[input$bar_x]], .data[[input$bar_tax]], 
                             cnt_abund, rel_abund),
                  aes_string(x = input$bar_x, y = input$bar_y, 
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
      ggplotly(p)
    })  


  # calculate multivariate analysis---------------------------------------------
  # centre and scale
  asv_scale <- eventReactive(input$pca_calculate, {
    if(input$pca_scale == 'UV') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sd(x))
    }
    if(input$pca_scale == 'pareto') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sqrt(x))
    }
    if(input$pca_scale == 'vast') {
      apply(asv_transform(), 2, function(x) ((x - mean(x)) / sd(x)) * (mean(x) / sd(x)))
    }
    else {
      asv_transform()
    }
  })
  
  # performing pca
  d_pcx <- eventReactive(input$pca_calculate, {
    ## samples in rows
    ## centring and scaling done outside of prcomp
    prcomp(t(asv_scale()), center = FALSE, scale. = FALSE) 
  })
  
  xPC <- reactive(paste0('PC', input$xPC))
  yPC <- reactive(paste0('PC', input$yPC))
  
  score_data <- reactive({
    out <- as.data.frame(d_pcx()$x)
    out$sampleID <- rownames(out)
    out <- out %>% 
      select(sampleID, xPC(), yPC()) %>%
      left_join(met(), 'sampleID')
    out <- out[,c(xPC(), yPC(), colnames(met()))]
    out
  })

  load_data <- reactive({
    out <- as.data.frame(d_pcx()$rotation)
    out$Taxon <- rownames(out)
    out <- out %>% select('Taxon', xPC(), yPC()) %>%
      left_join(tax(), 'Taxon')
    out <- out[, c(xPC(), yPC(), colnames(tax()))]
    out
  })

  
  # summary of pca
  pcx_summary <- eventReactive(input$pca_calculate, {
    summary_pcx <- summary(d_pcx())
    summary_df <- unclass(summary_pcx)[['importance']]
    
    # check variation explained by each PC
    variance_pc <- summary_pcx$sdev**2
    variance_pc <- variance_pc/sum(variance_pc)*100
    
    summary_wip <- rbind(summary_df, variance_pc)
    rownames(summary_wip)[4] <- 'Variance Explained'
    summary_wip
  })
  
  output$summary_pca <- DT::renderDataTable({
    pcx_summary() %>%
      DT::datatable(options = list(scrollX = TRUE)) %>%
      DT::formatRound(column = colnames(pcx_summary()), digits = 3)
  })
  
  # pca plot parameters
  ## initiate parameters as objects
  load_pt_colour <- NULL
  load_pt_shape <- NULL
  load_pt_size <- NULL
  load_pt_alpha <- NULL
  load_arrow <- FALSE
  show_load_label <- FALSE
  load_lab_colour <- NULL
  load_lab_size <- NULL
  load_lab_alpha <- NULL
  
  ## score point parameters
  score_pt_colour <- eventReactive(input$score_pt_colour, {
    if(input$score_pt_colour == 'none') 'black'
    else input$score_pt_colour 
  })
    
  score_pt_shape <- eventReactive(input$score_pt_shape, {
    if(input$score_pt_shape == 'none') 1
    else input$score_pt_shape
  })
  
  score_pt_size <- reactive(input$score_pt_size)
  score_pt_alpha <- reactive(input$score_pt_alpha)

  ## score label parameters
  score_label_by <- eventReactive(input$score_label_by, {
    if(input$score_label_by == 'none') NULL
    else  input$score_label_by
  })
  
  score_label <- eventReactive(input$score_label_by, {
    if(input$score_label_by == 'none') FALSE
    else score_label <- TRUE
  })
  
  score_lab_colour <- eventReactive(input$score_lab_colour, {
    if(input$score_lab_colour == 'none') 'black'
    else input$score_lab_colour
  })
  
  
  score_lab_size <- reactive(input$score_lab_size)
  score_lab_alpha <- reactive(input$score_lab_alpha)

  ## loading point parameters
  load_pt_colour <- eventReactive(input$load_pt_colour, {
    req(input$show_loading)
    if(input$load_pt_colour == 'none') 'darkred'
    else input$load_pt_colour
  })
  
  load_pt_shape <- eventReactive(input$load_pt_shape, {
    req(input$show_loading)
    if(input$load_pt_shape == 'none') 2
    else input$load_pt_shape
  })  
      
  load_pt_size <- reactive({
    req(input$show_loading)
    input$load_pt_size
  })
  load_pt_alpha <- reactive({
    req(input$show_loading)
    input$load_pt_alpha
  })
  load_arrow <- reactive({
    req(input$show_loading)
    input$load_arrow
  })
      
  ## loading label parameters
  load_label_by <- eventReactive(input$load_label_by, {
    req(input$show_loading)
    if(input$load_label_by == 'none') NULL
    else input$load_label_by
  })
  
  show_load_label <- eventReactive(input$load_label_by, {
    req(input$show_loading)
    if(input$load_label_by == 'none') FALSE
    else show_load_label <- TRUE
  })
      
  load_lab_colour <- eventReactive(input$load_lab_colour, {
    req(input$show_loading)
    if(input$load_lab_colour == 'none') 'darkred'
    else input$load_lab_colour
  })
  
  load_lab_size <- reactive({
    req(input$show_loading)
    input$load_lab_size
  })
  
  load_lab_alpha <- reactive({
    req(input$show_loading)
    input$load_lab_alpha
  })
    
  output$plot_pca <- renderPlotly({
    req(input$pca_calculate)
    
    p_biplot <- OCMSExplorer:::cms_biplot(
      score_data(), load_data(),
      xPC = input$xPC, yPC = input$yPC,
      # score point
      colour = score_pt_colour(), shape = score_pt_shape(),
      size = score_pt_size(), alpha = score_pt_alpha(),
      # score label
      label = score_label(), label.label = score_label_by(),
      label.colour = score_lab_colour(), label.size = score_lab_size(),
      label.alpha = score_lab_alpha(), label.repel = FALSE,
      # loading point
      loadings = input$show_loading, loadings.colour = load_pt_colour(), 
      loadings.shape = load_pt_shape(), loadings.arrow = load_arrow(),
      loadings.alpha = load_pt_alpha(), loadings.size = load_pt_size(),
      # loading label
      loadings.label = show_load_label(), loadings.label.label = load_label_by(),
      loadings.label.colour = load_lab_colour(), loadings.label.repel = FALSE,
      loadings.label.size = load_lab_size(), loadings.label.alpha = load_lab_alpha()
    )
    
    p_biplot <- p_biplot +
      theme_bw(12) +
      xlab(sprintf("PC%s (%s%%)", 
                   input$xPC, 
                   round(pcx_summary()['Variance Explained', input$xPC]), 2)) +
      ylab(sprintf("PC%s (%s%%)", 
                   input$yPC, 
                   round(pcx_summary()['Variance Explained', input$yPC]), 2))
    ggplotly(p_biplot)
  })
  
  # calculate alpha diversity---------------------------------------------------
  output$check <- renderPrint({
    xorder <- pdata() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], desc(alpha_avg)))
    levels(xorder$x)
  })

  div_result <- eventReactive(input$alpha_calculate, {

    alpha_data <- asv() %>% select(-Taxon)
    alpha_data <- as.data.frame(alpha_data)
    rownames(alpha_data) <- asv()$Taxon
    
    if(input$alpha_method == 'shannon_d') {
      H <- vegan::diversity(alpha_data,index = 'shannon',
                            base = 2, MARGIN = 2)
      exp(H)
    }
    else if(input$alpha_method == 'richness') {
      vegan::specnumber(alpha_data, MARGIN = 2)
    }
    else if(input$alpha_method == 'evenness') {
      rich_result <- vegan::specnumber(alpha_data, MARGIN = 2)
      H <- vegan::diversity(alpha_data, index = 'shannon', base = 2, MARGIN = 2)
      
      H / log(rich_result)
    }
    else {
      vegan::diversity(alpha_data,index = input$alpha_method,
                       base = 2, MARGIN = 2)
    }
  })
  
  pdata <- eventReactive(input$alpha_calculate, {
    met() %>%
      arrange(sampleID) %>%
      mutate_all(as.character) %>%
      mutate(alpha_value = div_result()[sort(names(div_result()))])
  })
  
  output$alpha_table <- DT::renderDataTable({
    DT::datatable(pdata() %>% rename(!!input$alpha_method := alpha_value),
                  options = list(scrollX = TRUE))
  })
  
  output$alpha_plot <- renderPlotly({
    req(input$alpha_grp)
    xorder <- pdata() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], desc(alpha_avg)))
    
    pdata_ordered <- pdata() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value),
             x = factor(.data[[input$alpha_grp]], levels = levels(xorder$x))) %>%
      distinct(x, sampleID, alpha_value) %>%
      ungroup()
    
    p <- ggplot(pdata_ordered, aes(x = x, y = alpha_value))
    
    n_grp <- table(met()[,input$alpha_grp])
    
    if(min(n_grp) > 5) {
      p <- p +
        geom_point(aes(group = .data[[input$alpha_grp]]), alpha = 0.8) +
        stat_boxplot(aes(group = .data[[input$alpha_grp]]))
    }
    else {
      p <- p +
        geom_point(alpha = 0.8)
    }
    
    ytitle <- c("Shannon-Weaver Index (H)"="shannon",
              "Simpson Index (D1)" = "simpson",
              "Shannon (H'), q = 1" = "shannon_d",
              "Inverse Simpson (D2), q = 2" = "invsimpson",
              "Species Richness (S), q = 0" = "richness",
              "Species Evenness (J)" = "evenness")
    p <- p +
      theme_bw() +
      xlab(input$alpha_grp) +
      ylab(names(ytitle[ytitle == input$alpha_method])) +
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p)
  })
  
  # calculate heatmap-----------------------------------------------------------
  # calculate sample clustering
  samp_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(dist(t(asv_transform()), method = input$dist_method), 
           method = input$hclust_method)
  })
  
  samp_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(samp_hclust(), input$hmap_samp_k)
  })
  
  # sample dendrogram
  observe({
    req(input$hmap_samp_k, input$hmap_samp_colour)
    if(input$hmap_samp_colour == 'none') category <- NULL
    else category <- input$hmap_samp_colour
    p <- OCMSExplorer:::plot_ggdendro(
      samp_ddata(),
      direction = 'lr',
      branch.size = 0.5,
      metadata = met(),
      category = category,
      label.category = input$hmap_samp_label,
      id = 'sampleID')
    
    p_legend <- cowplot::get_legend(p)
    
    output$sample_dendro_plot <- renderPlotly({
      label_data <- ggplot_build(p)$data[[2]]
      
      ggplotly(p + theme(legend.position = 'none')) %>% 
        style(text = label_data$label, textposition = "middle right")
    })
    
    output$sample_dendro_leg <- renderPlot({
      grid::grid.draw(p_legend)
    })
  })
  
  
  # calculate asv clustering
  
  asv_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(dist(asv_transform(), method = input$dist_method),
           method = input$hclust_method)
  })
  
  asv_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(asv_hclust(), input$hmap_asv_k)
  })
  
  # asv dendrogram
  observe({
    req(input$hmap_asv_k, input$hmap_asv_colour)
    if(input$hmap_asv_colour == 'none') category <- NULL
    else category <- input$hmap_asv_colour
    
    p <- OCMSExplorer:::plot_ggdendro(
      asv_ddata(),
      direction = 'lr',
      branch.size = 0.5,
      metadata = tax(),
      label.category = input$hmap_asv_label,
      category = category,
      id = 'Taxon')
    
    p_legend <- cowplot::get_legend(p)
    
    output$asv_dendro_plot <- renderPlotly({
      label_data <- ggplot_build(p)$data[[2]]
      ggplotly(p + theme(legend.position = 'none')) %>% 
        style(text = label_data$label, textposition = "middle right")
    })
    
    output$asv_dendro_leg <- renderPlot({
      grid::grid.draw(p_legend)
    })
  })
  
  # set heatmap orientation
  hmap_data <- reactive({
    
    if(input$sample_as_x) {
      hmap_data <- asv_transform() # taxon in rows, samples in columns
      rownames(hmap_data) <- tax()[, input$hmap_tax_label]
    }
    else {
      hmap_data <- t(asv_transform())
      colnames(hmap_data) <- tax()[, input$hmap_tax_label]
    }
    hmap_data
  })
  
  # parameterizing heat map object
  hmap <- reactive({
    heatmapr(
      x = hmap_data(), 
      dist_method = input$dist_method,
      hclust_method = input$hclust_method,
      dendrogram = 'both',
      show_dendrogram = c('show_dendro_y' %in% input$show_dendro,
                          'show_dendro_x' %in% input$show_dendro),
      digits = 3,
      show_grid = TRUE
    )
  })
  
  # plot heat map
  output$hmap_plot <- renderPlotly({
    req(input$hmap_calculate)
    heatmaply(hmap(), node_type = 'heatmap', colors = 'RdYlBu',
              key.title = 'Normalized\nRelative Abundance') 
  })
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")