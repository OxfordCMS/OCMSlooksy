# Module UI

#' @title   mod_explore_ui and mod_explore_server
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
#' @import plotly
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        # set taxonomic level
        selectInput(ns('tax_level'), 'Taxonomic level:',
                    choices = c('ASV','Phylum','Class','Order',
                                'Family','Genus','Species'),
                    selected = 'ASV')
        
        # Barplot
        
        # PCA
        
        # Alpha-diversity
        
        # Heat map
      ),
      dashboardBody(
        fluidRow(
          box(width = 12,
              h3('Check Box'), verbatimTextOutput(ns('check')))),
        fluidRow(
          box(id = 'box_bar', width = 12,
              plotlyOutput(ns('plot_bar'), width = '100%', height = 'auto'))),
        fluidRow(
          box(id = 'box_pca',
              plotlyOutput(ns('plot_pca'), width = '100%', height = 'auto')),
          box(id = 'box_alpha',
              plotlyOutput(ns('plot_alpha'), width = '100%', height = 'auto'))),
        fluidRow(
          box(id = 'box_hmap', width = 12,
              plotlyOutput(ns('plot_hmap'), width = '100%', height = 'auto')))
      )
    )
  )
}

# Module Server

#' @rdname mod_explore
#' @export
#' @keywords internal

mod_explore_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  # import data into module
  data_set <- reactive(improxy$data_db)
  
  # prepare dataset at specified taxonomy level
  
    
  output$check <- renderPrint({
    
    })
  # bar plot
  output$plot_bar <- renderPlotly({
    random_ggplotly('bar') %>%
      layout(
        title ='Stacked bar plot of relative abundance',
        xaxis = list(title = 'sample'),
        yaxis = list(title = 'relative abundance (%)'))
  })
  
  # pca plot
  output$plot_pca <- renderPlotly({
    random_ggplotly('point') %>%
      layout(
        title = 'PCA plot with scores as sample,\nloading as ASV at chosen taxon level',
        xaxis = list(title = 'PC1 (%)'),
        yaxis = list(title = 'PC2 (%)')
      )
  })
  
  # alpha diversity
  output$plot_alpha <- renderPlotly({
    random_ggplotly('violin') %>%
      layout(
        title = 'Alpha diversity of sample groups',
        axis = list(title = 'sample group'),
        yaxis = list(title = 'Alpha diversity')
      )
  })
  
  # heatmap
  output$plot_hmap <- renderPlotly({
    random_ggplotly('tile') %>%
      layout(
        title = 'Clustered heat map of samples and ASV',
        xaxis = list(title = 'Samples'),
        yaxis = list(title = 'ASV at chosen taxon level')
      )
  })
}

## To be copied in the UI
# mod_explore_ui("explore_ui_1")

## To be copied in the server
# callModule(mod_explore_server, "explore_ui_1")