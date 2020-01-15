#' @import shiny
#' @import shinipsum
#' @import DT
#' @import datasets
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' 
app_server <- function(input, output, session) {
  # List the first level callModules here


  callModule(mod_intro_server, "intro_ui_1", parent_session = session)
  cross_module <- callModule(mod_import_server, "import_ui_1", parent_session = session)
  callModule(mod_qc_server, "qc_ui_1", cross_module)
  # callModule(mod_setup_server, "setup_ui_1", cross_module)

  #callModule(mod_explore_server, "explore_ui_1", cross_module)
}