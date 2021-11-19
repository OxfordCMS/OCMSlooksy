#' @import shiny
#' @import htmltools

app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here

    navbarPage(
      theme = shinythemes::shinytheme('flatly'),
      title = "OCMSlooksy", id = 'tabs', position = 'fixed-top',
      # Introduction---------------------------------------------------
      tabPanel(title = "Introduction", value = 'intro', icon = icon("book"),
              mod_intro_ui("intro_ui_1")),

      # Import data----------------------------------------------------
      tabPanel(title = "Import Data", value = 'import', icon = icon("database"),
              mod_import_ui("import_ui_1")),

      # # QC report----------------------------------------------------
      tabPanel(title = "QC Report", value = 'qc', icon = icon("broom"),
              mod_qc_ui("qc_ui_1")),

      # Quality filter samples-----------------------------------------
      tabPanel(title = "Filter Samples", value = 'qualityfilter',
              icon = icon("filter"),
#
#               fluidRow(
#                 wellPanel(h3('Check'),verbatimTextOutput('check'))
#               ),
              mod_qualityfilter_ui("qualityfilter_ui_1")),

      navbarMenu(
        title = "Analysis Modules",
        menuName = 'analysis_module',
        # relative abundance profiles------------------------------------
        tabPanel(title = "Microbiome Profile", value = 'profile',
                icon = icon("chart-bar"), mod_profile_ui("profile_ui_1")),

        # Alpha diversity------------------------------------------------
        tabPanel(title = "\u03B1-Diversity", value = 'alpha',
                icon = icon("seedling"), mod_alpha_ui("alpha_ui_1")),

        # Beta diversity-------------------------------------------------
        tabPanel(title = "\u03B2-Diversity", value = 'beta',
                icon = icon("project-diagram"), mod_beta_ui("beta_ui_1")),

        # Diffential abundance-------------------------------------------
        tabPanel(title = "Differential Abundance", value = 'diff',
                 icon = icon("balance-scale-left"),
                 mod_diff_abund_ui("diff_abund_ui_1")),

        # Feature Proportionality----------------------------------------
        tabPanel(title = "Feature Comparsion", value = 'prop',
                icon = icon("chart-line"), mod_prop_ui("prop_ui_1"))

      ) # end navbarMenu
    ) # end navbarpage
  )
}



#' @import shiny
#' @importFrom golem add_resource_path activate_js
#' @importFrom golem favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  addResourcePath(
    'www', system.file('app/www', package = 'OCMSlooksy')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    shinyjs::useShinyjs()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
