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
          menuItem('Relative Abundance', tabName = 'bar_tab', selected = FALSE),
          menuItem('Multivariate Analysis', selected = FALSE,
                   menuSubItem('PCA', tabName = 'pca_tab'),
                   menuSubItem('PCoA', tabName = 'pcoa_tab'),
                   menuSubItem('NMDS', tabName = 'nmds_tab')),
          menuItem('\u03B1-Diversity Analysis', tabName = 'alpha_tab', selected = FALSE),
          menuItem('Cluster Analysis', tabName = 'hmap_tab', selected = FALSE),
          
          # Bar plot controls---------------------------------------------------
          conditionalPanel(
           condition = "input.menu === 'bar_tab'",
           div(id = ns('bar_param_div'),
             tags$div(style = 'text_align: center', tags$b('Plot Parameters')),
             br(), hr(),
             fixedPanel(
               uiOutput(ns('bar_x_ui')),
               selectInput(ns('bar_tax'), 'Taxonomic level:',
                            choices = c('ASV','Phylum','Class','Order',
                                        'Family','Genus','Species'),
                            selected = 'ASV'),
               radioButtons(ns('bar_fill'), 'Colour by:',
                            c('Relative abundance' = 'rel_abund',
                              'Read count' = 'cnt_abund')),
               actionButton(ns('bar_submit'), "Calculate and Plot")
               )
             )
           ),
          # Multivariate analysis-----------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pca_tab'",
            div(id = ns('pca_param_div'),
                br(), hr(),
                fixedPanel(
                  width = 225,
                  tags$div(style = "text-align: center", tags$b('PCA Parameters')),
                  checkboxGroupInput(ns('pca_transform'), "Transform", 
                                     c("Centre-log ratio transform" = 'clr'),
                                     selected = 'clr'),
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
              br(), br(),
              actionButton(ns('nmds_calculate'), "Calculate")
            ))
          ),
              
          
        # Alpha-diversity-------------------------------------------------------
        
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
                )
              )
        )
        
        # set taxonomic level
        )
      ),
      dashboardBody(
        box(
          width = '100%', br(),br(), br(),
          h1('Overview of data set'),
          wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
          column(width = 12, "A suite of tools to perform exploratory analysis in order to get an overall sense of the data set.",
          br()),
          tabItems(
            # bar plot body--------------------------------------------------------
            tabItem(
              tabName = 'bar_tab',
              column(width = 12,
                h2('Relative Distribution of Taxa')),
              column(width = 12,
                h3(textOutput(ns('bar_title'))),
                DT::dataTableOutput(ns('bar_data'))),
              column(width = 12,
                     shinyjqui::jqui_resizabled(
                       plotlyOutput(ns('bar_plot'))))
              ),
            
            # multivariate analysis body---------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
              column(width = 12,
                h2('Principle Component Analysis')),
              column(width = 12,
                     h3('Summary of PCA'),
                     DT::dataTableOutput(ns('summary_pca'))),
              
              hidden(div(id = ns('mva_param_div'),
                h3('PCA Plot'),
                wellPanel(
                  tags$div(style = 'text_align: center', h4(tags$b("Plot Parameters"))),
                  fluidRow(
                    # Plot controls
                    column(width = 6,
                      div(style = "display: inline-block;vertical-align: top",
                          uiOutput(ns('xPC_ui'))),
                      div(style = "display: inline-block;vertical-align: top", br(),br(),),
                      div(style = "display: inline-block;vertical-align: top",
                          uiOutput(ns('yPC_ui'))),
                      div(style = "display: inline-block;vertical-align: top", 
                          br(), br(), br(), br()),
                      div(style = "display: inline-block;vertical-align: top",
                          checkboxInput(ns('mva_feature'), "Show loadings", TRUE)),
                      div(style = "display: inline-block;vertical-align: top", br(), br()),
                      div(style = "display: inline-block;vertical-align: top",
                          checkboxInput(ns('load_arrow'), 'Show loading arrows', TRUE)
                          ),
                      ),
                    column(width = 12, hr()),
                    column(width = 3,
                      # score point aesthetics
                      h4("Score points aesthetics"),
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
                      h4("Score labels aesthetics"),
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
                        h4('Loading points aesthetics'),
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
                        h4('Loading labels aethetics'),
                        uiOutput(ns('load_label_ui')),
                        uiOutput(ns('load_lab_colour_ui')),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_lab_size'), 'Label size:',
                                         min = 0.1, max = 5, value = 3, step = 0.5)),
                        div(style = "display: inline-block;vertical-align: top",
                            numericInput(ns('load_lab_alpha'), 'Label transparency:',
                                         min = 0.1, max = 1, value = 1, step = 0.1))
                        )
                      )),
                    column(width = 12, hr(),
                      actionButton(ns('pca_submit'), 'Plot PCA'))
                  ))
                )),
                column(width = 12,
                       shinyjqui::jqui_resizabled(
                         plotlyOutput(ns('plot_pca'))))
            ),
        
            tabItem(
              tabName = 'alpha_tab',
              plotlyOutput(ns('alpha_plot'), width = '100%', height = 'auto')
              ),
            
            # heatmap body------------------------------------------------------
            tabItem(
              tabName = 'hmap_tab',
              hidden(div(id = ns('hmap_body_div'),
                h3('Heirarchical Clustering'),
                column(
                  width = 12,
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
                         h4('Sample dendrogram'),
                         shinyjqui::jqui_resizabled(
                           plotlyOutput(ns('sample_dendro_plot'))))
                ),
                column(
                  width = 12,
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
                    h4('Taxonomy dendrogram'),
                    shinyjqui::jqui_resizabled(
                      plotlyOutput(ns('asv_dendro_plot'))))
                  
                ),
                
                h3('Heat map'),
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
                                  choices = c('ASV','Taxon','Species')),
                      actionButton(ns('hmap_submit'), 'Plot'))
                  )),
                column(width = 12,
                       shinyjqui::jqui_resizabled(
                         plotlyOutput(ns('hmap_plot'), 
                                      width = '100%', height = 'auto')))
                )),
              
              )
            
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

  observeEvent(input$mva_feature, {
    toggle('loading_div')
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
    selectInput(ns('score_pt_colour'), 'Point colour:', choices = c('none', colnames(met())))
  })
  output$score_pt_shape_ui <- renderUI({
    selectInput(ns('score_pt_shape'), 'Point shape:', choices = c('none', colnames(met())))
  })
  
  ### score label aethetics
  output$score_label_ui <- renderUI({
    selectInput(ns('score_label_by'), 'Label scores by:', 
                choices = c('none', colnames(met())))
  })
  output$score_lab_colour_ui <- renderUI({
    selectInput(ns('score_lab_colour'), 'Label colour:', choices = c('none', colnames(met())))
  })
  
  ### loading points aesthetics
  output$load_pt_colour_ui <- renderUI({
    selectInput(ns('load_pt_colour'), 'Point colour:', choices = c('none', colnames(tax())))
  })
  output$load_pt_shape_ui <- renderUI({
    selectInput(ns('load_pt_shape'), 'Point shape:', choices = c('none', colnames(tax())))
  })
  ### loading labels aesthetics
  output$load_label_ui <- renderUI({
    selectInput(ns('load_label_by'), 'Label loadings by:', 
                choices = c('none', colnames(tax())))
  })
  output$load_lab_colour_ui <- renderUI({
    selectInput(ns('load_lab_colour'), 'Label colour:', choices = c('none', colnames(tax())))
  })
  output$load_lab_shape_ui <- renderUI({
    selectInput(ns('load_lab_shape'), 'Label shape:', choices = c('none', colnames(tax())))
  })

  

  ## render controls - heat map-------------------------------------------------
  output$hmap_samp_label_ui <- renderUI({
    selectInput(ns('hmap_samp_label'), "Label:",
                choices = colnames(met()), selected = 'sampleID')
  })
  output$hmap_samp_colour_ui <- renderUI({
    choices <- c('none', colnames(met()))
    choices <- choices[choices != 'sampleID']
    radioButtons(ns('hmap_samp_colour'), "Show sample metadata:",
                 choices = choices,
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
  observeEvent(input$bar_submit, {
    pdata <- work() %>%
      group_by(.data[[input$bar_x]]) %>%
      mutate(grp_total = sum(read_count)) %>%
      group_by(.data[[input$bar_x]], .data[[input$bar_tax]]) %>%
      mutate(tax_total = sum(read_count),
             cnt_abund = tax_total,
             rel_abund = tax_total / grp_total * 100) 
    
    p <- ggplot(pdata %>%
                  distinct(.data[[input$bar_x]], .data[[input$bar_tax]], 
                           rel_abund, cnt_abund),
                aes_string(x = input$bar_x, y = input$bar_fill, 
                                    fill = input$bar_tax)) +
      geom_bar(stat = 'identity') +
      xlab(input$bar_x) +
      scale_fill_discrete(name = input$bar_tax) +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    if(input$bar_fill == 'rel_abund') {
      p <- p +
        ylab(sprintf('Relative Abundance (%%), %s', input$bar_tax))
    }
    else {
      p <- p +
        ylab(sprintf('Cumulative Read Count, %s', input$bar_tax))
    }
    
    output$bar_title  <- renderText({
      if(input$bar_fill == 'rel_abund') {
        sprintf('Relative Abundance (%%), %s', input$bar_tax)
      }
      else {
        sprintf('Cumulative Read Count, %s', input$bar_tax)
      }
    })
    output$bar_data <- DT::renderDataTable({
      out <-  pdata %>%
        select(sampleID, ASV, Kingdom:.data[[input$bar_fill]], .data[[input$bar_fill]]) %>%
        distinct() %>%
        spread(sampleID, .data[[input$bar_fill]])
      DT::datatable(out, options = list(scrollX = TRUE)) %>%
        DT::formatRound(column = met()$sampleID, digits = 3)
    })
    
    output$bar_plot <- renderPlotly({
      ggplotly(p)
    })  
  })

  # calculate multivariate analysis---------------------------------------------
  ## Transform counts to proportional data---------------------------------------
  asv_transform <- eventReactive(input$pca_calculate, {
    req(input$pca_transform)
    asv_df <- as.data.frame(asv())
    rownames(asv_df) <- asv()$Taxon
    asv_df <- asv_df[,colnames(asv_df) != 'Taxon']
    if(input$pca_transform == 'clr') {
      ## generate Monte Carlo samples from Dirichlet distribution
      ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
      ## use geometric mean abundance of features
      
      asv_clr <- ALDEx2::aldex.clr(asv_df, conds = met()$sampleID)
      clr_instance <- lapply(ALDEx2::getMonteCarloInstances(asv_clr),
                             function(m){t(apply(m,1,median))})
      ## samples in columns
      clr_df <- data.frame(matrix(unlist(clr_instance), ncol = length(clr_instance),
                                  byrow = FALSE,
                                  dimnames = list(colnames(clr_instance[[1]]),
                                                  names(clr_instance))),
                           stringsAsFactors=FALSE)
      out <- clr_df
    }
    else {
      out <- asv_df
    }
    out
  })
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
  observeEvent(input$pca_submit, {

    ## score point parameters
    if(input$score_pt_colour == 'none') {
      score_pt_colour <- 'black'
    }
    else{
      score_pt_colour <- input$score_pt_colour
    }
    if(input$score_pt_shape == 'none') {
      score_pt_shape <- 1
    }
    else{
      score_pt_shape <- input$score_pt_shape
    }
    score_pt_size <- input$score_pt_size
    score_pt_alpha <- input$score_pt_alpha

    ## score label parameters
    if(input$score_label_by == 'none') {
      score_label <- FALSE
      score_label_by <- NULL
    }
    else{
      score_label <- TRUE
      score_label_by <- input$score_label_by
    }
    if(input$score_lab_colour == 'none') {
      score_lab_colour <- 'black'
    }
    else {
   
      score_lab_colour <- input$score_lab_colour
    }
    score_lab_size <- input$score_lab_size
    score_lab_alpha <- input$score_lab_alpha

    ## loading point parameters
    show_loading <- input$mva_feature
    if(show_loading) {
      if(input$load_pt_colour == 'none') {
        load_pt_colour <- 'darkred'
      }
      else{
        load_pt_colour <- input$load_pt_colour
      }
      if(input$load_pt_shape == 'none') {
        load_pt_shape <- 2
      }
      else {
        load_pt_shape <- input$load_pt_shape
      }
      load_pt_size <- input$load_pt_size
      load_pt_alpha <- input$load_pt_alpha
      load_arrow <- input$load_arrow
      
      ## loading label parameters
      if(input$load_label_by == 'none') {
        load_label_by <- NULL
        show_load_label <- FALSE
      }
      else {
        load_label_by <- input$load_label_by
        show_load_label <- TRUE
      }
      if(input$load_lab_colour == 'none') {
        load_lab_colour <- 'darkred'
      }
      else {
        load_lab_colour <- input$load_lab_colour
      }
      load_lab_size <- input$load_lab_size
      load_lab_alpha <- input$load_lab_alpha
    }
    else {
      load_pt_colour <- NULL
      load_pt_shape <- NULL
      load_pt_size <- NULL
      load_pt_alpha <- NULL
      load_arrow <- FALSE
      show_load_label <- FALSE
      load_lab_colour <- NULL
      load_lab_size <- NULL
      load_lab_alpha <- NULL
    }
    

    output$plot_pca <- renderPlotly({
      p_biplot <- OCMSExplorer:::cms_biplot(
        score_data(), load_data(),
        xPC = input$xPC, yPC = input$yPC,
        # score point
        colour = score_pt_colour, shape = score_pt_shape,
        size = score_pt_size, alpha = score_pt_alpha,
        # score label
        label = score_label, label.label = score_label_by,
        label.colour = score_lab_colour, label.size = score_lab_size,
        label.alpha = score_lab_alpha, label.repel = FALSE,
        # loading point
        loadings = show_loading, loadings.colour = load_pt_colour, 
        loadings.shape = load_pt_shape, loadings.arrow = load_arrow,
        loadings.alpha = load_pt_alpha, loadings.size = load_pt_size,
        # loading label
        loadings.label = show_load_label, loadings.label.label = load_label_by,
        loadings.label.colour = load_lab_colour, loadings.label.repel = FALSE,
        loadings.label.size = load_lab_size, loadings.label.alpha = load_lab_alpha
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
  
  })
  
  # alpha diversity-------------------------------------------------------------
  output$alpha_plot <- renderPlotly({
    random_ggplotly('violin') %>%
      layout(
        title = 'Alpha diversity of sample groups',
        axis = list(title = 'sample group'),
        yaxis = list(title = 'Alpha diversity')
      )
  })
  
  # heatmap---------------------------------------------------------------------
  output$check <- renderPrint({

  })
  
  # Transform counts to proportional data---------------------------------------
  asv_transform <- eventReactive(input$hmap_calculate, {
    asv_df <- as.data.frame(asv())
    rownames(asv_df) <- asv_df$Taxon
    asv_df <- asv_df[,colnames(asv_df) != 'Taxon']
    
    ## generate Monte Carlo samples from Dirichlet distribution
    ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
    ## use geometric mean abundance of features
    
    asv_clr <- ALDEx2::aldex.clr(asv_df, conds = met()$sampleID)
    clr_instance <- lapply(ALDEx2::getMonteCarloInstances(asv_clr),
                           function(m){t(apply(m,1,median))})
    ## samples in columns
    clr_df <- data.frame(matrix(unlist(clr_instance), ncol = length(clr_instance),
                                byrow = FALSE,
                                dimnames = list(colnames(clr_instance[[1]]),
                                                names(clr_instance))),
                         stringsAsFactors=FALSE)
    clr_df
  })
  
  # calculate sample clustering-------------------------------------------------
  samp_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(dist(t(asv_transform()), method = input$dist_method), 
           method = input$hclust_method)
  })
  
  samp_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(samp_hclust(), input$hmap_samp_k)
  })
  
  # sample dendrogram-----------------------------------------------------------
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
  
  
  # calculate asv clustering----------------------------------------------------
  
  asv_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(dist(asv_transform(), method = input$dist_method),
           method = input$hclust_method)
  })
  
  asv_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(asv_hclust(), input$hmap_asv_k)
  })
  
  # asv dendrogram--------------------------------------------------------------
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
  hmap_data <- eventReactive(input$hmap_submit, {
    req(input$sample_as_x)
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
  hmap <- eventReactive(input$hmap_submit, {
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
    heatmaply(hmap(), node_type = 'heatmap', colors = 'RdYlBu',
              key.title = 'Normalized\nRelative Abundance') 
  })
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")