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
    shinyjs::useShinyjs(),
    # List the first level UI elements here 

    navbarPage(title = "OCMS Explorer", id = 'tabs', position = 'fixed-top',

               # Introduction---------------------------------------------------
               tabPanel(title = "Introduction", value = 'intro', icon = icon("book"),
                        mod_intro_ui("intro_ui_1")),

               # Import data----------------------------------------------------
               tabPanel(title = "Import Database", value = 'import', icon = icon("database"),
                        mod_import_ui("import_ui_1")),

               # # QC report------------------------------------------------------
               tabPanel(title = "QC Report", value = 'qc', icon = icon("broom"),
                        mod_qc_ui("qc_ui_1")),

               # Prepare dataset for analysis-----------------------------------
               tabPanel(title = "Prepare Data Set", value = 'prepare',
                        icon = icon("filter"), mod_setup_ui("setup_ui_1")),
               
               # Overview samples------------------------------------------------
               tabPanel(title = "Overview", value = 'overview', icon = icon("binoculars"),
                        mod_overview_ui("overview_ui_1"))

               # Explore Beta-Diversity-----------------------------------------

    ))
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
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
