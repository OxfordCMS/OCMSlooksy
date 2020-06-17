# Module UI

#' @title   mod_overview_ui and mod_overview_server
#' @description  Overview of dataset using exploratory analysis
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_overview
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
                   uiOutput(ns('pca_menu_ui')),
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
              uiOutput(ns('pca_scale_ui')),
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
              uiOutput(ns('pcoa_dist_ui')),
              actionButton(ns('pcoa_calculate'), 'Calculate')
            )
          ),
        
          # # Alpha-diversity-------------------------------------------------------
          # conditionalPanel(
          #   condition = "input.menu === 'alpha_tab'",
          #   div(id = ns('alpha_param_div'),
          #       br(), hr(),
          #       fixedPanel(
          #         width = 225,
          #         tags$div(style = "text-align: center",
          #                  tags$b("\u03B1-Diversity Parameters")),
          #         actionButton(ns('alpha_calculate'), "Calculate")
          #       ))
          #   ),

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
                              choices = c('complete','ward.D2',
                                          'average','median','centroid'),
                              selected = 'ward.D2'),
                  uiOutput(ns('dist_method_ui')),
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
              mod_ov_bar_ui(ns("ov_bar_ui_1"))
            ),
            
            # PCA body----------------------------------------------------------    
            tabItem(
              tabName = 'pca_tab',
                mod_ov_pca_ui(ns("ov_pca_ui_1"))
            ),
            
            #PCoA body----------------------------------------------------------
            tabItem(
              tabName = "pcoa_tab",
              mod_ov_pcoa_ui(ns("ov_pcoa_ui_1"))
            ),
            # alpha diversity body----------------------------------------------
            tabItem(
              tabName = 'alpha_tab',
              mod_ov_alpha_ui(ns("ov_alpha_ui_1"))
            ),
            # heatmap body------------------------------------------------------
            tabItem(
              tabName = 'hmap_tab',
              mod_ov_hmap_ui(ns("ov_hmap_ui_1"))
              )
          ) # end of tab items
        )
      ) # end of dashboard body---------------------------------------------
    )
  )
}

# Module Server

#' @rdname mod_overview
#' @export
#' @keywords internal

mod_overview_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  # import data into module-----------------------------------------------------
  working_set <- reactive(improxy$data_db)
  
  met <- reactive(working_set()$metadata)
  asv <- reactive(working_set()$asv)
  asv_transform <- reactive(working_set()$t_asv)
  t_selected <- reactive(working_set()$t_selected)
  tax <- reactive(working_set()$tax)
  
  # output$check <- renderPrint({
  # 
  # })
  output$pca_menu_ui <- renderUI({
    if(t_selected() != 'percent') {
      sidebarMenu(menuSubItem('PCA', tabName = 'pca_tab'))
    }
  })

  # store data in reactiveValues to pass onto submodule-------------------------
  bridge <- reactiveValues()
  observe({
    bridge$met <- improxy$data_db$metadata
    bridge$asv <- improxy$data_db$asv
    bridge$asv_transform <- improxy$data_db$t_asv
    bridge$tax <- improxy$data_db$tax
  })
  
  # bar plot server-------------------------------------------------------------
  # render controls bar plot
  output$bar_x_ui <- renderUI({
    selectInput(ns('bar_x'), "x-axis", choices = colnames(met()),
                selected = 'sampleID')
  })
  
  # pass bar reactive inputs to submodule
  bridge$bar_input <- reactiveValues()
  observe({
    bridge$bar_input$bar_tax <- input$bar_tax
    bridge$bar_input$bar_y <- input$bar_y
    bridge$bar_input$bar_x <- input$bar_x
  })

  # call overview submodule for bar plot
  callModule(mod_ov_bar_server, "ov_bar_ui_1", param = bridge)
  
  # PCA server------------------------------------------------------------------
  output$pca_scale_ui <- renderUI({
    if(any(asv_transform() < 0)) {
      choices <- c("none" = "none",
                   "unit-variance scaling" = 'UV',
                   "vast scaling" = 'vast')
    }
    else {
      choices <- c("none" = "none",
                   "unit-variance scaling" = 'UV',
                   "pareto scaling" = 'pareto',
                   "vast scaling" = 'vast')
    }
    
    radioButtons(ns('pca_scale'), "Scale",
                 choices = choices,
                 selected = 'UV')
  })
  bridge$pca_input <- reactiveValues()
  observeEvent(input$pca_calculate, {
    if(t_selected() != 'percent') {
      # pass pca reactive inputs to submodule
      bridge$pca_input$pca_calculate <- input$pca_calculate
      bridge$pca_input$pca_scale <- input$pca_scale
      callModule(mod_ov_pca_server, "ov_pca_ui_1", param = bridge)
    }
  })
  
  # PCoA server-----------------------------------------------------------------
  output$pcoa_dist_ui <- renderUI({
    if(t_selected() == 'percent') choices <- 'bray'
    else choices <- c("manhattan", "euclidean", "canberra")
    
    selectInput(ns('pcoa_dist'), "Distance method",
                choices = choices,
                selected = choices[1])
  })
  
  bridge$pcoa_input <- reactiveValues()
  observe({
    bridge$pcoa_input$pcoa_dist <- input$pcoa_dist
    bridge$pcoa_input$pcoa_calculate <- input$pcoa_calculate
  })
  callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1", param = bridge)
  
  # Alpha diversity server------------------------------------------------------

  callModule(mod_ov_alpha_server, "ov_alpha_ui_1", param = bridge)
  
  # Heatmap server--------------------------------------------------------------
  output$dist_method_ui <- renderUI({
    if(t_selected() == 'percent') choices <- 'bray'
    else choices <- c("manhattan", "euclidean", "canberra", "bray")
    
    selectInput(ns('dist_method'), "Distance method",
                choices = choices,
                selected = choices[1])
  })
  
  bridge$hmap_input <- reactiveValues()
  observe({
    bridge$hmap_input$hclust_method <- input$hclust_method
    bridge$hmap_input$dist_method <- input$dist_method
    bridge$hmap_input$hmap_calculate <- input$hmap_calculate
  })
  callModule(mod_ov_hmap_server, "ov_hmap_ui_1", param = bridge)
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")