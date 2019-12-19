#' @import shiny
#' @import shinipsum
#' @import DT
#' @import datasets
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' 
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  
  callModule(mod_intro_server, "intro_ui_1")
  callModule(mod_import_server, "import_ui_1")
  callModule(mod_explore_server, "explore_ui_1")
}