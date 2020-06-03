#' @import shiny
#' @import DT
#' @import datasets
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' 
app_server <- function(input, output, session) {
  # List the first level callModules here

  options(shiny.maxRequestSize=100*1024^2)
  callModule(mod_intro_server, "intro_ui_1", parent_session = session)
  
  cross_module1 <- callModule(mod_import_server, "import_ui_1", parent_session = session)
  callModule(mod_qc_server, "qc_ui_1", cross_module1)
  
  cross_module2 <- callModule(mod_setup_server, "setup_ui_1", cross_module1)
  callModule(mod_overview_server, "overview_ui_1", cross_module2)
}