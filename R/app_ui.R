#' @import shiny
#' @import shinipsum
#' @import DT
#' @import datasets
#' @import htmltools
#' @import shinydashboard
#' @import shinyjs

app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    navbarPage(title = "Prototype App", position = 'static-top',
               
               # Introduction---------------------------------------------------
               tabPanel(title = "Introduction", icon = icon("book"),
                        mod_intro_ui("intro_ui_1")),
               
               # Import data----------------------------------------------------
               tabPanel(title = "Import Database", icon = icon("database"),
                        mod_import_ui("import_ui_1")),
               
               # Explore samples------------------------------------------------
               tabPanel(title = "Overview", icon = icon("binoculars"),
                        mod_explore_ui("explore_ui_1"))
               
               # Explore Beta-Diversity-----------------------------------------
               
    ))
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'OCMSExplorer')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
