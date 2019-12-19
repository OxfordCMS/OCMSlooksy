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
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        # set taxonomic level
        selectInput('tax_level', 'Taxonomic level:',
                    choices = c('ASv','Phylum','Class','Order',
                                'Family','Genus','Species'),
                    selected = 'ASV')
        
        # Barplot
        
        # PCA
        
        # Alpha-diversity
        
        # Heat map
      ),
      dashboardBody(
        box(id = 'box_bar',
            plotlyOutput('plot_bar')),
        
        box(id = 'box_pca',
            plotlyOutput('plot_pca')),
        
        box(id = 'box_alpha',
            plotlyOutput('plot_alpha')),
        
        box(id = 'box_hmap',
            plotlyOutput('plot_hmap'))
      )
    )
  )
}

# Module Server

#' @rdname mod_explore
#' @export
#' @keywords internal

mod_explore_server <- function(input, output, session){
  ns <- session$ns
}

## To be copied in the UI
# mod_explore_ui("explore_ui_1")

## To be copied in the server
# callModule(mod_explore_server, "explore_ui_1")