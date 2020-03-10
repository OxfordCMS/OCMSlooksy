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
        sidebarMenu(id = 'menu', br(),
          menuItem('Task Info', tabName = 'info_tab_overview', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Relative Abundance', tabName = 'bar_tab'),
          menuItem('Multivariate Analysis'),
                   menuSubItem('PCA', tabName = 'pca_tab'),
                   menuSubItem('PCoA', tabName = 'pcoa_tab'),
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
                            choices = c('featureID','Kingdom','Phylum',
                                        'Class', 'Order', 'Family','Genus',
                                        'Species', 'Taxon'),
                            selected = 'featureID'),
                radioButtons(ns('bar_y'), 'Response measure:',
                             c('Relative abundance' = 'rel_abund',
                               'Read count' = 'cnt_abund')))
          ),
          # PCA controls--------------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'pca_tab'",
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b('PCA Parameters')),
              radioButtons(ns('pca_scale'), "Scale",
                           choices = c("none" = "none",
                                       "unit-variance scaling" = 'UV',
                                       "pareto scaling" = 'pareto',
                                       "vast scaling" = 'vast'),
                           selected = 'UV'),
              actionButton(ns('pca_calculate'), "Calculate")
              )
            ),
          # PCoA controls------------------------------------------------------- 
          conditionalPanel(
            condition = "input.menu === 'pcoa_tab'",
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", tags$b("PCoA Parameters")),
              selectInput(ns('pcoa_dist'), "Distance Metric",
                          choices = c("manhattan", "euclidean", "canberra", 
                                      "clark", "bray", "kulczynski", "jaccard", 
                                      "gower", "altGower", "morisita", "horn",
                                      "mountford", "raup", "binomial", "chao", 
                                      "cao", "mahalanobis"),
                          selected = 'euclidean'),
              actionButton(ns('pcoa_calculate'), 'Calculate')
            )
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
                  # checkboxGroupInput(ns('alpha_test'), "Statistical Test",
                  #              c("ANOVA" = "anova",
                  #                "Kruskal-Wallis" = 'kw',
                  #                "Effect size" = "effect_size")),
                  # checkboxInput(ns('alpha_paired'), "Paired data", FALSE),
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
                              choices = c("manhattan", "euclidean", "canberra", 
                                          "clark", "bray", "kulczynski", "jaccard", 
                                          "gower", "altGower", "morisita", "horn",
                                          "mountford", "raup", "binomial", "chao", 
                                          "cao", "mahalanobis"),
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
              tabName = 'info_tab_overview',
              h1('Overview of data set'),
              column(width = 12, "A suite of tools to perform exploratory analysis in order to get an overall sense of the data set.")
            ),
            # bar plot body--------------------------------------------------------
            tabItem(
              tabName = 'bar_tab',
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
              ),
            
            # PCA body----------------------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
              column(width = 12,
                h1('Principle Component Analysis')),
              tags$div("PCA is a non-supervised multivariate analysis that provides a good 'first look' at microbiome data."),
              
              hidden(div(id = ns('pca_summary_div'), 
                h2('Summary of PCA'),
                DT::dataTableOutput(ns('summary_pca')))),
              
              hidden(div(id = ns('pca_body_div'),
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
                      sliderInput(ns('score_pt_size'), 'Point size:',
                                  min = 0.1, max = 5, value = 3, step = 0.5,
                                  ticks = FALSE),
                      sliderInput(ns('score_pt_alpha'), 'Point transparency:',
                                     min = 0.1, max = 1, value = 1, step = 0.1)
                      ),
                    # score label aesthetics
                    column(width = 3,
                      h3("Score labels aesthetics"),
                      uiOutput(ns('score_label_ui')),
                      uiOutput(ns('score_lab_colour_ui')),
                      sliderInput(ns('score_lab_size'), 'Label size:',
                                       min = 0.1, max = 5, value = 3, step = 0.5),
                      sliderInput(ns('score_lab_alpha'), 'Label transparency:',
                                       min = 0.1, max = 1, value = 1, step = 0.1)
                      ),
                    
                    hidden(div(id=ns('loading_div'),
                      column(width = 3,
                        # loading point aesthetics
                        h3('Loading points aesthetics'),
                        uiOutput(ns('load_pt_colour_ui')),
                        uiOutput(ns('load_pt_shape_ui')),
                        sliderInput(ns('load_pt_size'), 'Point size:',
                                         min = 0.1, max = 5, value = 3, step = 0.5),
                        sliderInput(ns('load_pt_alpha'), 'Point transparency:',
                                         min = 0.1, max = 1, value = 1, step = 0.1)
                        ),
                      # loading label aesthetics
                      column(width = 3,
                        h3('Loading labels aethetics'),
                        uiOutput(ns('load_label_ui')),
                        uiOutput(ns('load_lab_colour_ui')),
                        sliderInput(ns('load_lab_size'), 'Label size:',
                                         min = 0.1, max = 5, value = 3, step = 0.5),
                        sliderInput(ns('load_lab_alpha'), 'Label transparency:',
                                         min = 0.1, max = 1, value = 1, step = 0.1)
                        )
                      ))
                  ))
                )),
                column(width = 12,
                       column(width = 1, style = 'padding:0px;', dropdown(
                         size = 'xs', icon = icon('save'), inline = TRUE, 
                         style = 'material-circle', width = 160,
                         animate = animateOptions(
                           enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                           exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                         
                         downloadBttn(ns('dl_pca_original'), 
                                      list(icon('file-image'), "Original plot"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_pca_html'), 
                                      list(icon('file-code'), "Interactive plot"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_pca_data'), 
                                      list(icon('file-alt'), "Plot data"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_pca_rds'), 
                                      list(icon('file-prescription'), "RDS"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_pca_all'), 
                                      list(icon('file-archive'), "All"),
                                      size = 'xs', style = 'minimal')
                       )),
                       column(width = 11, style = 'padding:0px;',
                              shinyjqui::jqui_resizable(
                         plotlyOutput(ns('plot_pca'), width = '100%', height = 'auto'))))
            ),
            
            #PCoA body----------------------------------------------------------
            tabItem(
              tabName = "pcoa_tab",
              h1("Principal Coordinate Analysis"),
              tags$div("PCoA is a supervised multivariate analysis (a priori knowledge of clusters) that can be used for assessing statistical significance of cluster patterns under a multivariate model.", br()),
              hidden(div(id = ns('pcoa_body_div'),
                h2("Distance Matrix"),
                DT::dataTableOutput(ns('pcoa_dist_table')),
                h2("PCoA Summary"),
                DT::dataTableOutput(ns('pcoa_summary')),
                h2('PCoA plot'),
                wellPanel(
                  tags$div(style = 'text_align: center', h3("Plot Parameters")),
                  fluidRow(
                    # Plot controls
                    column(width = 3,
                      div(style = "display: inline-block;vertical-align: top",
                         uiOutput(ns('xPCo_ui'))),
                      div(style = "display: inline-block;vertical-align: top",
                         uiOutput(ns('yPCo_ui'))),
                    checkboxInput(ns('pcoa_ellipse'), "Show clusters", 
                                  value = TRUE)),
                    column(width = 3,
                      # score point aesthetics
                      h4("Score points aesthetics"),
                      uiOutput(ns('pcoa_pt_colour_ui')),
                      uiOutput(ns('pcoa_pt_shape_ui')),
                      sliderInput(ns('pcoa_pt_size'), 'Point size:',
                                 min = 0.1, max = 5, value = 3, step = 0.5,
                                 ticks = FALSE),
                      sliderInput(ns('pcoa_pt_alpha'), 'Point transparency:',
                                 min = 0.1, max = 1, value = 1, step = 0.1)),
                    # score label aesthetics
                    column(width = 3,
                      h4("Score labels aesthetics"),
                      uiOutput(ns('pcoa_label_ui')),
                      uiOutput(ns('pcoa_lab_colour_ui')),
                      sliderInput(ns('pcoa_lab_size'), 'Label size:',
                                 min = 0.1, max = 5, value = 3, step = 0.5),
                      sliderInput(ns('pcoa_lab_alpha'), 'Label transparency:',
                                 min = 0.1, max = 1, value = 1, step = 0.1)),
                    
                    # cluster aethetics
                    hidden(div(id = ns('pcoa_ell_div'),
                      column(width = 3,
                        h4("Cluster aesthetics"),
                        uiOutput(ns('pcoa_nclust_ui')),
                        checkboxInput(ns('pcoa_ell_colour'),"Colour cluster ellipses",
                                      value = TRUE),
                        selectInput(ns('pcoa_ell_type'), "Type of ellipse",
                                    choices = c('t-distribution' = 't',
                                                'normal distribution' = 'norm',
                                                'Euclidean distance' = 'euclid'),
                                    selected = 'norm'),
                        radioButtons(ns('pcoa_ell_line'), "Linetype",
                                     choices = c('solid','dashed','longdash',
                                                 'dotdash'),
                                     selected = 'solid'),
                        numericInput(ns('pcoa_ell_ci'), "Confidence Interval",
                                     min = 0.1, max = 0.99, value = 0.95, 
                                     step = 0.05))))
                    ))
                )),
              # column(width = 6, plotlyOutput(ns('CH_plot'))),
              # column(width = 6, verbatimTextOutput(ns('CH_index'))),
              column(width = 12, 
                     dropdown(
                       size = 'xs', icon = icon('save'), inline = TRUE, 
                       style = 'material-circle',
                       animate = animateOptions(
                         enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                         exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                       
                       downloadBttn(ns('dl_pcoa_original'), 
                                    list(icon('file-image'), "Original plot"),
                                    size = 'xs', style = 'minimal'), br(),
                       downloadBttn(ns('dl_pcoa_html'), 
                                    list(icon('file-code'), "Interactive plot"),
                                    size = 'xs', style = 'minimal'), br(),
                       downloadBttn(ns('dl_pcoa_data'), 
                                    list(icon('file-alt'), "Plot data"),
                                    size = 'xs', style = 'minimal'), br(),
                       downloadBttn(ns('dl_pcoa_rds'), 
                                    list(icon('file-prescription'), "RDS"),
                                    size = 'xs', style = 'minimal'), br(),
                       downloadBttn(ns('dl_pcoa_all'), 
                                    list(icon('file-archive'), "All"),
                                    size = 'xs', style = 'minimal')
                     ),
                     shinyjqui::jqui_resizable(
                       plotlyOutput(ns('pcoa_plot'), width = '100%')))
            ),
            # alpha diversity body----------------------------------------------
            tabItem(
              tabName = 'alpha_tab',
              h1("\u03B1-Diversity"),
              tags$div("Alpha diversity assesses the diversity of sets of communities (or sets of samples). Species richness is the number of unique species. Species evenness is a measure of the consistency of species abundances (uneven data sets have community members that dominate in abundance). Entropy measures such as Shannon entropy and Simpson index are measures of uncertainty in the species identity of a sample [Jost 2006]. Diversity measures, such as Shannon's Diveristy and Inverse Simpson's Index, takes into account of the abundance of species in the community. In fact, when all species in a community are equally common, entropy and diveristy measures are equivalent. Entropy indeces can be converted to diversity by mathematical transformation."),
              column(width = 12,
                     DT::dataTableOutput(ns('alpha_table'))),
              hidden(div(id = ns('alpha_body_div'),
                column(width = 3, br(), br(),
                  wellPanel(
                    uiOutput(ns('alpha_grp_ui')))),
                column(width = 9,
                       dropdown(
                         size = 'xs', icon = icon('save'), inline = TRUE, 
                         style = 'material-circle',
                         animate = animateOptions(
                           enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                           exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                         
                         downloadBttn(ns('dl_alpha_original'), 
                                      list(icon('file-image'), "Original plot"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_alpha_html'), 
                                      list(icon('file-code'), "Interactive plot"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_alpha_data'), 
                                      list(icon('file-alt'), "Plot data"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_alpha_rds'), 
                                      list(icon('file-prescription'), "RDS"),
                                      size = 'xs', style = 'minimal'), br(),
                         downloadBttn(ns('dl_alpha_all'), 
                                      list(icon('file-archive'), "All"),
                                      size = 'xs', style = 'minimal')
                       ),
                       shinyjqui::jqui_resizable(
                         plotlyOutput(ns('alpha_plot'), width = '100%')
                       )),
                column(width = 12,
                       DT::dataTableOutput(ns('alpha_test')))
              ))
            ),
            # heatmap body------------------------------------------------------
            tabItem(
              tabName = 'hmap_tab',
              h1('Heirarchical Clustering'),
              tags$div("Heirarchical clustering is influenced by the linkage method used to measure the distance between clusters of observations. The linkage methods differ in the criteria that is used to determine the distance of sets of observations. The criteria are based on the distance between individual observations within a set. Choice in distance method also affects the clustering outcome, which measures the distance between a pair of observations. Distance metrics fall into three categories: agglomerative, divisive, and dissimilarity."),
              hidden(div(id = ns('hmap_body_div'),
                column(width = 12,
                  column(width = 3, br(), br(),
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
                         dropdown(
                           size = 'xs', icon = icon('save'), inline = TRUE, 
                           style = 'material-circle',
                           animate = animateOptions(
                             enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                             exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                           
                           downloadBttn(ns('dl_dend_samp_original'), 
                                        list(icon('file-image'), "Original plot"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_dend_samp_html'), 
                                        list(icon('file-code'), "Interactive plot"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_dend_samp_data'), 
                                        list(icon('file-alt'), "Plot data"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_dend_samp_rds'), 
                                        list(icon('file-prescription'), "RDS"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_dend_samp_all'), 
                                        list(icon('file-archive'), "All"),
                                        size = 'xs', style = 'minimal')
                         ),
                         shinyjqui::jqui_resizable(
                           plotlyOutput(ns('sample_dendro_plot'), width = '100%')))),
                column(width = 12,
                  column(width = 3, br(), br(),
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
                    dropdown(
                      size = 'xs', icon = icon('save'), inline = TRUE, 
                      style = 'material-circle',
                      animate = animateOptions(
                        enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                        exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                      
                      downloadBttn(ns('dl_dend_asv_original'), 
                                   list(icon('file-image'), "Original plot"),
                                   size = 'xs', style = 'minimal'), br(),
                      downloadBttn(ns('dl_dend_asv_html'), 
                                   list(icon('file-code'), "Interactive plot"),
                                   size = 'xs', style = 'minimal'), br(),
                      downloadBttn(ns('dl_dend_asv_data'), 
                                   list(icon('file-alt'), "Plot data"),
                                   size = 'xs', style = 'minimal'), br(),
                      downloadBttn(ns('dl_dend_asv_rds'), 
                                   list(icon('file-prescription'), "RDS"),
                                   size = 'xs', style = 'minimal'), br(),
                      downloadBttn(ns('dl_dend_asv_all'), 
                                   list(icon('file-archive'), "All"),
                                   size = 'xs', style = 'minimal')
                    ),
                    shinyjqui::jqui_resizable(
                      plotlyOutput(ns('asv_dendro_plot'), width = '100%')))),
                
                h2('Heat map'), br(), br(),
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
                                  choices = c('featureID','Taxon','Species')))
                  )),
                column(width = 12,
                       column(width = 1, style = 'padding:0px;', dropdown(
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
                       column(width = 11, style = 'padding:0px;',
                         shinyjqui::jqui_resizable(
                           plotlyOutput(ns('hmap_plot'), 
                                        width = '100%', height = 'auto'))))
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
  
  # putting data into one dataframe---------------------------------------------
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
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(input$pca_calcualte, {
    show('pca_summary_div')
  })
  observeEvent(input$pca_calculate, {
    show('pca_body_div')
  })

  observeEvent(input$show_loading, {
    toggle('loading_div')
  })
  
  observeEvent(input$pcoa_calculate, {
    show('pcoa_body_div')
  })
  
  observeEvent(input$pcoa_ellipse, {
    toggle('pcoa_ell_div')
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
  
  ## render controls - PCA------------------------------------
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

  ## render controls - PCoA-----------------------------------------------------
  output$pcoa_nclust_ui <- renderUI({
    numericInput(ns('pcoa_nclust'), "Number of clusters, k", 
                 value = 2, min = 2, max = nrow(met())-1, step = 1)
  })
  output$xPCo_ui <- renderUI({
    numericInput(ns('xPCo'), "Principal Coordinate, x-axis", min = 1, max = length(met()$sampleID), step = 1,
                 value = 1)
  })
  output$yPCo_ui <- renderUI({
    numericInput(ns('yPCo'), "Principal Coordinate, y-axis", min = 1, max = length(met()$sampleID), step = 1,
                 value = 2)
  })
  
  ### pcoa point aesthetics
  output$pcoa_pt_colour_ui <- renderUI({
    selectInput(ns('pcoa_pt_colour'), 'Point colour:', 
                choices = c('none', 'cluster', colnames(met())), selected = 'none')
  })
  output$pcoa_pt_shape_ui <- renderUI({
    selectInput(ns('pcoa_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  
  ### pcoa label aethetics
  output$pcoa_label_ui <- renderUI({
    selectInput(ns('pcoa_label'), 'Label by:', 
                choices = c('none', colnames(met())), selected = 'none')
  })
  output$pcoa_lab_colour_ui <- renderUI({
    selectInput(ns('pcoa_lab_colour'), 'Label colour:', 
                choices = c('none', 'cluster', colnames(met())), selected = 'none')
  })
  
  # ### pca loaing points aesthetics
  # output$pcoa_pt_colour_ui <- renderUI({
  #   selectInput(ns('pcoa_pt_colour'), 'Point colour:', 
  #               choices = c('none', colnames(tax())), selected = 'none')
  # })
  # output$pcoa_pt_shape_ui <- renderUI({
  #   selectInput(ns('pcoa_pt_shape'), 'Point shape:', 
  #               choices = c('none', colnames(tax())), selected = 'none')
  # })
  
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
                choices = colnames(tax()), selected = 'featureID')
  })
  
  output$hmap_asv_colour_ui <- renderUI({
    choices <- c('none', colnames(tax()))
    choices <- choices[!choices %in% c('sequence','featureID','Taxon')]
    radioButtons(ns('hmap_asv_colour'), "Show taxonomy level:",
                 choices = choices, selected = 'none')
  })
  # calculate output bar plot---------------------------------------------------
  
  bar_data <- reactive({
    req(input$bar_tax, input$bar_x, input$bar_y)
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
    
    output$bar_table <- DT::renderDataTable({
      req(input$bar_y, input$bar_tax, input$bar_x)
      
      out <-  bar_data() %>%
        distinct(.data[[input$bar_tax]], .data[[input$bar_x]], .data[[input$bar_y]]) %>%
        spread(.data[[input$bar_x]], .data[[input$bar_y]])
        
      if(input$bar_y == 'rel_abund') {
        DT::datatable(out,  extensions = 'Buttons', 
                      options = list(scrollX = TRUE, 
                                     dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
          DT::formatRound(column = met()[,input$bar_x], digits = 3)
      }
      else {
        DT::datatable(out, extensions = 'Buttons', 
                      options = list(scrollX = TRUE, 
                                     dom = 'Blfrtip', buttons = c('copy','csv')))
      }
      
    })
    
    p_bar <- reactive({
      p <- ggplot(bar_data() %>%
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

  # calculate pca---------------------------------------------------------------

  # centre and scale
  asv_scale <- eventReactive(input$pca_calculate, {
    if(input$pca_scale == 'UV') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sd(x))
    }
    else if(input$pca_scale == 'pareto') {
      apply(asv_transform(), 2, function(x) (x - mean(x)) / sqrt(x))
    }
    else if(input$pca_scale == 'vast') {
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
    out$featureID <- rownames(out)
    out <- out %>% select('featureID', xPC(), yPC()) %>%
      left_join(tax(), 'featureID')
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
      DT::datatable(extensions = 'Buttons', 
                    options = list(scrollX = TRUE, 
                                  dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
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
    
  p_biplot <- reactive({
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
    
    p_biplot
  })
  
  output$plot_pca <- renderPlotly({
    ggplotly(p_biplot())
  })
  
  output$dl_pca_original <- downloadHandler(
    fname <- function() {"ov_pca.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_biplot())}
  )
  
  output$dl_pca_html <- downloadHandler(
    fname <- function() {"ov_pca.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_biplot())), file)
    }
  )
  
  output$dl_pca_data <- downloadHandler(
    fname <- function() {"ov_pcadata.zip"}, 
    content <- function(file) {
      # put together pca data to write to file
      to_save <- d_pcx()
      to_save[['pca_score']] <- score_data()
      to_save[['pca_load']] <- load_data()
      
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- sprintf("ov_pca-%s.csv", names(to_save))
      for(i in 1:length(to_zip)) {
        write.csv(to_save, to_zip[i])  
      }
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
    
  )
  
  output$dl_pca_rds <- downloadHandler(
    fname <- function() {"ov_pca.rds"},
    content <- function(file) {
      saveRDS(p_biplot(), file)
    }
  )
  
  output$dl_pca_all <- downloadHandler(
    fname <- function() {"ov_pca.zip"},
    content <- function(file) {
      
      # put together pca data to write to file
      to_save <- d_pcx()
      to_save[['pca_score']] <- score_data()
      to_save[['pca_load']] <- load_data()
      
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_pca.tiff", "ov_pca.html","ov_pca.rds", 
                  sprintf("ov_pca-%s.csv", names(to_save)))
      ggsave(to_zip[1], plot=p_biplot())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_biplot())), to_zip[2])
      saveRDS(p_biplot(), to_zip[3])
      for(i in 1:length(to_save)) {
        write.csv(to_save, to_zip[i+3])  
      }

      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  # calculate pcoa--------------------------------------------------------------
  # sample clustering

  ## samples as rows
  pcoa_dist <- reactive({
    vegan::vegdist(t(asv_transform()), method = input$pcoa_dist)
  })
  
  output$pcoa_dist_table <- DT::renderDataTable({
    DT::datatable(as.data.frame(as.matrix(pcoa_dist())), 
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  # identify clusters based on distances
  cluster_result <- reactive({
    data.frame(sampleID = rownames(as.matrix(pcoa_dist())),
               pam_cluster = as.vector(cluster::pam(pcoa_dist(), 
                                                input$pcoa_nclust)$cluster))
    
  })
  
  ## determine the optimal number of clusters for the dataset using the mediod
  ## as a midpoint
  pcoa_optk <- reactive({
    out <- 0
    
    # cluster of 1 returns NaN
    for (k in 2:(nrow(met())-1)) {
      # find mediod clusters and return a vector of clusters
      
      # calculate Calisnki-Harabasz index to determine the fit to the cluster
      out[k] <- clusterSim::index.G1(t(asv_transform()), cluster_result()$pam_cluster, 
                                     d = pcoa_dist(), centrotypes = "medoids")
    }
    
    out
  })
  
  # # plot CH index
  # output$CH_plot <- renderPlotly({
  #   
  #   pdata <- data.frame(x=1:length(pcoa_optk()), y=0, yend=pcoa_optk())
  #   pdata$xend <- pdata$x
  #   
  #   k <- nrow(met())-1
  #   # plot number of clusters and respective CH index
  #   p <- ggplot(pdata) +
  #     geom_segment(ggplot2::aes(x=x, y=y, xend=xend, yend=yend)) +
  #     scale_x_continuous(breaks=2:k, limits=c(2,k)) +
  #     xlab('k clusters') +
  #     ylab('Calinski-Harabasz Index') +
  #     theme_bw() +
  #     theme(panel.grid.minor = element_blank(),
  #           panel.grid.major.x=element_blank())
  #   
  #   ggplotly(p)
  # })
  # 
  # calculate principal coordinates
  pcoa_data <- eventReactive(input$pcoa_calculate, {
    ape::pcoa(pcoa_dist(), correction = 'cailliez')
  })
  
  # summary of pcoa
  pcoa_summary <- reactive({
    out <- as.matrix(pcoa_data()$values)
    out <- out[,c('Eigenvalues', 'Relative_eig','Cumul_eig')]
    rownames(out) <- paste0('PC', 1:nrow(out))
    colnames(out) <- c('Eigenvalues','Variance Explained', 'Cumulative Variance Explained')
    t(out)
  })
  
  output$pcoa_summary <- DT::renderDataTable({
    DT::datatable(pcoa_summary(), 
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(pcoa_summary()), digits = 3)
      
  })
  
  # setting pcoa plot parameters
  pcoa_pt_colour <- reactive({
    if(input$pcoa_pt_colour == 'none')  'black'
    else if(input$pcoa_pt_colour == 'cluster') 'pam_cluster'
    else input$pcoa_pt_colour
  })

  pcoa_pt_shape <- reactive({
    if(input$pcoa_pt_shape == 'none') 1
    else pcoa_pt_shape <- input$pcoa_pt_shape
  })
  
  pcoa_pt_size <- reactive(input$pcoa_pt_size)
  pcoa_pt_alpha <- reactive(input$pcoa_pt_alpha)
  
  pcoa_label <- reactive({
    if(input$pcoa_label == 'none') FALSE
    else TRUE
  })
  
  pcoa_label_by <- reactive({
    if(input$pcoa_label == 'none') NULL
    else input$pcoa_label
  })
  
  pcoa_lab_colour <- reactive({
    if(input$pcoa_lab_colour == 'none') NULL
    else if(input$pcoa_lab_colour == 'cluster') 'pam_cluster'
    else input$pcoa_lab_colour
  })
  
  pcoa_lab_size <- reactive(input$pcoa_lab_size)
  pcoa_lab_alpha <- reactive(input$pcoa_lab_alpha)
  
  pcoa_ell_colour <- reactive({
    if(input$pcoa_ell_colour) 'pam_cluster'
    else 'black'
  })
  
  # plot pcoa plot
  pdata_pcoa <- reactive({
    pdata <- data.frame(pcoa_data()$vectors)
    pdata$sampleID <- rownames(pcoa_data()$vectors)
    pdata <- pdata %>%
      inner_join(cluster_result() %>%
                   mutate(pam_cluster = as.character(pam_cluster)), 
                 'sampleID') %>%
      inner_join(met(), 'sampleID')
    pdata
  })
  
  p_pcoa <- reactive({
    xPCo <- paste('Axis', input$xPCo, sep = ".")
    yPCo <- paste('Axis', input$yPCo, sep = ".")
    
    p <- ggplot(pdata_pcoa(), aes_string(x = xPCo, y = yPCo))
    
    p <- p +
      ggfortify:::geom_factory(ggplot2::geom_point, pdata_pcoa(), 
                               colour = pcoa_pt_colour(), size = pcoa_pt_size(), 
                               alpha = pcoa_pt_alpha(), shape = pcoa_pt_shape())
    
    if(input$pcoa_ellipse) {
      p <- p +
        ggfortify:::geom_factory(ggplot2::stat_ellipse, pdata_pcoa(),
                                 group = 'pam_cluster',
                                 colour = pcoa_ell_colour(),
                                 linetype = input$pcoa_ell_line,
                                 type = input$pcoa_ell_type,
                                 level = input$pcoa_ell_ci)
    }
    
    p <- ggfortify:::plot_label(p = p, data = pdata_pcoa(), label = pcoa_label(), 
                                label.label = pcoa_label_by(), 
                                label.colour = pcoa_lab_colour(), 
                                label.alpha = pcoa_lab_alpha(), 
                                label.size = pcoa_lab_size())
    
    xvar <- round(pcoa_data()$values$Broken_stick[input$xPCo]*100, 2)
    yvar <- round(pcoa_data()$values$Broken_stick[input$yPCo]*100, 2)
    p <- p + 
      theme_bw(12) +
      xlab(sprintf('PCo %s (%s%%)', input$xPCo, xvar)) +
      ylab(sprintf("PCo %s (%s%%)", input$yPCo, yvar))
    
    p
  })
  
  output$pcoa_plot <- renderPlotly({
    ggplotly(p_pcoa())
  })

  output$dl_pcoa_original <- downloadHandler(
    fname <- function() {"ov_pcoa.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_pcoa())}
  )
  
  output$dl_pcoa_html <- downloadHandler(
    fname <- function() {"ov_pcoa.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_pcoa())), file)
    }
  )
  
  output$dl_pcoa_data <- downloadHandler(
    fname <- function() {"ov_pcoadata.zip"}, 
    content <- function(file) {
      # put together pcoa data to write to file
      to_save <- pcoa_data()
      to_save[['pcoa_plotdata']] <- pdata_pcoa()

      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      
      to_zip <- sprintf('ov_pcoa%s.csv',names(to_save))
      for(i in 1:length(to_zip)) {
        write.csv(to_save, to_zip[i])  
      }
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
  output$dl_pcoa_rds <- downloadHandler(
    fname <- function() {"ov_pcoa.rds"},
    content <- function(file) {
      saveRDS(p_pcoa(), file)
    }
  )
  
  output$dl_pcoa_all <- downloadHandler(
    fname <- function() {"ov_pcoa.zip"},
    content <- function(file) {
      # put together pcoa data to write to file
      to_save <- pcoa_data()
      to_save[['pcoa_plotdata']] <- pdata_pcoa()
      
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      
      to_zip <- c("ov_pcoa.tiff", "ov_pcoa.html","ov_pcoa.rds",
                  sprintf('ov_pcoa%s.csv',names(to_save)))
      
      # writing temp files
      ggsave(to_zip[1], plot=p_pcoa())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_pcoa())), to_zip[2])
      saveRDS(p_pcoa(), to_zip[3])
      for(i in 1:length(to_save)) {
        write.csv(to_save, to_zip[i+3])  
      }
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  # calculate alpha diversity---------------------------------------------------

  div_result <- eventReactive(input$alpha_calculate, {

    alpha_data <- asv() %>% select(-featureID)
    alpha_data <- as.data.frame(alpha_data)
    rownames(alpha_data) <- asv()$featureID
    
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
  
  # perform statistical tests on alpha values ********PICK UP HERE**********
  alpha_stat_validate <- function(input) {
    if(any(input == 'anova')) {
      
    }
  }
  alpha_stat <- eventReactive(input$alpha_calculate, {
    
    # validate selected tests

  })
  
  
  # plot alpha diversity
  pdata_alpha <- eventReactive(input$alpha_calculate, {
    met() %>%
      arrange(sampleID) %>%
      mutate_all(as.character) %>%
      mutate(alpha_value = div_result()[sort(names(div_result()))])
  })
  
  output$alpha_table <- DT::renderDataTable({
    DT::datatable(pdata_alpha() %>% rename(!!input$alpha_method := alpha_value),
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  p_alpha <- reactive({
    req(input$alpha_grp)
    xorder <- pdata_alpha() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], desc(alpha_avg)))
    
    pdata_ordered <- pdata_alpha() %>%
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
        geom_violin(aes(group = .data[[input$alpha_grp]]), fill = NA)
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
    
    p
  })
  output$alpha_plot <- renderPlotly({
    ggplotly(p_alpha())
  })
  
  output$dl_alpha_original <- downloadHandler(
    fname <- function() {"ov_alpha.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_alpha())}
  )
  
  output$dl_alpha_html <- downloadHandler(
    fname <- function() {"ov_alpha.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_alpha())), file)
    }
  )
  
  output$dl_alpha_data <- downloadHandler(
    fname <- function() {"ov_alpha.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_alpha(), file)
    }
  )
  
  output$dl_alpha_rds <- downloadHandler(
    fname <- function() {"ov_alpha.rds"},
    content <- function(file) {
      saveRDS(p_alpha(), file)
    }
  )
  
  output$dl_alpha_all <- downloadHandler(
    fname <- function() {"ov_alpha.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_alpha.tiff", "ov_alpha.html","ov_alpha.csv", "ov_alpha.rds")
      ggsave(to_zip[1], plot=p_alpha())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_alpha())), to_zip[2])
      write.csv(pdata_alpha(), to_zip[3])
      saveRDS(p_alpha(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  # calculate heatmap-----------------------------------------------------------
  # calculate sample clustering
  samp_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(vegan::vegdist(t(asv_transform()), method = input$dist_method), 
           method = input$hclust_method)
  })
  
  samp_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(samp_hclust(), input$hmap_samp_k)
  })
  
  # sample dendrogram
  p_dend_samp <- reactive({
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
    p
  })

  output$sample_dendro_plot <- renderPlotly({
    label_data <- ggplot_build(p_dend_samp())$data[[2]]
    
    ggplotly(p_dend_samp() + theme(legend.position = 'none')) %>% 
      style(text = label_data$label, textposition = "middle right")
  })
  
  output$sample_dendro_leg <- renderPlot({
    p_legend <- cowplot::get_legend(p_dend_samp())
    grid::grid.draw(p_legend)
  })

  output$dl_dend_samp_original <- downloadHandler(
    fname <- function() {"ov_dend_samp.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_dend_samp())}
  )
  
  output$dl_dend_samp_html <- downloadHandler(
    fname <- function() {"ov_dend_samp.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_dend_samp())), file)
    }
  )
  
  output$dl_dend_samp_data <- downloadHandler(
    fname <- function() {"ov_dend_samp.csv"}, 
    content <- function(file) {
      readr::write_csv(samp_ddata(), file)
    }
  )
  
  output$dl_dend_samp_rds <- downloadHandler(
    fname <- function() {"ov_dend_samp.rds"},
    content <- function(file) {
      saveRDS(p_dend_samp(), file)
    }
  )
  
  output$dl_dend_samp_all <- downloadHandler(
    fname <- function() {"ov_dend_samp.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_dend_samp.tiff", "ov_dend_samp.html",
                  "ov_dend_samp.csv", "ov_dend_samp.rds")
      ggsave(to_zip[1], plot=p_dend_samp())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_dend_samp())), to_zip[2])
      write.csv(samp_ddata(), to_zip[3])
      saveRDS(p_dend_samp(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
  # calculate asv clustering
  
  asv_hclust <- reactive({
    req(input$hmap_calculate)
    hclust(vegan::vegdist(asv_transform(), method = input$dist_method),
           method = input$hclust_method)
  })
  
  asv_ddata <- reactive({
    req(input$hmap_calculate)
    OCMSExplorer:::dendro_data_k(asv_hclust(), input$hmap_asv_k)
  })
  
  # asv dendrogram
  p_dend_asv <- reactive({
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
      id = 'featureID')
  })

  output$asv_dendro_plot <- renderPlotly({
    label_data <- ggplot_build(p_dend_asv())$data[[2]]
    ggplotly(p_dend_asv() + theme(legend.position = 'none')) %>% 
      style(text = label_data$label, textposition = "middle right")
  })
  
  output$asv_dendro_leg <- renderPlot({
    p_legend <- cowplot::get_legend(p_dend_asv())
    grid::grid.draw(p_legend)
  })
  
  
  output$dl_dend_asv_original <- downloadHandler(
    fname <- function() {"ov_dend_asv.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_dend_asv())}
  )
  
  output$dl_dend_asv_html <- downloadHandler(
    fname <- function() {"ov_dend_asv.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_dend_asv())), file)
    }
  )
  
  output$dl_dend_asv_data <- downloadHandler(
    fname <- function() {"ov_dend_asv.csv"}, 
    content <- function(file) {
      readr::write_csv(asv_ddata(), file)
    }
  )
  
  output$dl_dend_asv_rds <- downloadHandler(
    fname <- function() {"ov_dend_asv.rds"},
    content <- function(file) {
      saveRDS(p_dend_asv(), file)
    }
  )
  
  output$dl_dend_asv_all <- downloadHandler(
    fname <- function() {"ov_dend_asv.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_dend_asv.tiff", "ov_dend_asv.html",
                  "ov_dend_asv.csv", "ov_dend_asv.rds")
      ggsave(to_zip[1], plot=p_dend_asv())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_dend_asv())), to_zip[2])
      write.csv(asv_ddata(), to_zip[3])
      saveRDS(p_dend_asv(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
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
  
  output$check <- renderPrint({

  })
  # parameterizing heat map object
  hmap <- reactive({
    heatmapr(
      x = hmap_data(), 
      distfun = vegan::vegdist,
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
  
  output$dl_hmap_html <- downloadHandler(
    fname <- function() {"ov_hmap.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(heatmaply(hmap(), 
                                        node_type = 'heatmap', colors = 'RdYlBu',
                                        key.title = 'Normalized\nRelative Abundance'), 
                              file)
    }
  )
  
  output$dl_hmap_data <- downloadHandler(
    fname <- function() {"ov_hmap.csv"}, 
    content <- function(file) {
      readr::write_csv(hmap_data(), file)
    }
  )
  
  output$dl_hmap_rds <- downloadHandler(
    fname <- function() {"ov_hmap.rds"},
    content <- function(file) {
      saveRDS(hmap(), file)
    }
  )
  
  output$dl_hmap_all <- downloadHandler(
    fname <- function() {"ov_hmap.zip"},
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
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")