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

  cross_module1 <- callModule(mod_import_server, "import_ui_1",
                              parent_session = session)
  # output$check <- renderPrint({
  #   print(cross_module1$import_status)
  # })
  #
  # hide qc tab if parameter table not detected
  observe({
    req(cross_module1$import_status)
    if(cross_module1$import_status == "Data validation successful" &&
      'parameter_table' %in% names(cross_module1$data_db)) {
      showTab(inputId = 'tabs', target = 'qc')
      callModule(mod_qc_server, "qc_ui_1", cross_module1)
    }
  })

  observe({
    hideTab(inputId = 'tabs', target = "qc")
    hideTab(inputId = 'tabs', target = 'qualityfilter')
    hideTab(inputId = 'tabs', target = "Analysis Tasks")
  })

  observe({
    req(cross_module1$import_status)
    if(cross_module1$import_status == "Data validation successful") {
      showTab(inputId = 'tabs', target = 'qualityfilter')
      showTab(inputId = 'tabs', target = "Analysis Tasks")
      # cross_module2 <- callModule(mod_setup_server, "setup_ui_1", cross_module1)
      cross_module2 <- callModule(mod_qualityfilter_server,
                                  "qualityfilter_ui_1", cross_module1)

      # relative abundance
      callModule(mod_profile_server, "profile_ui_1", cross_module2)

      # alpha diversity
      callModule(mod_alpha_server, "alpha_ui_1", cross_module2)

      # beta diversity
      callModule(mod_beta_server, "beta_ui_1", cross_module2)

      # # feature proportionality
      callModule(mod_prop_server, "prop_ui_1", cross_module2)
      #
      # # differential abundance
      callModule(mod_diff_abund_server, "diff_abund_ui_1", cross_module2)
    }
  })



}