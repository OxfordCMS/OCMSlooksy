#' da_permanova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_da_permanova_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      h3('PERMANOVA Results'),
      DT::dataTableOutput(ns('permanova_summary'))
    )

  )
}

#' da_permanova Server Function
#'
#' @noRd
mod_da_permanova_server <- function(input, output, session, param){
  ns <- session$ns

  # perform permanova
  fit <- eventReactive(param$permanova_input$permanova_calculate, {
    # need sample in rows
    adonis_data <- as.data.frame(param$work_db$asv_transform)
    adonis_data <- t(adonis_data)
    f_terms <- paste(rev(param$permanova_input$permanova_terms), collapse = '+')

    vegan::adonis(formula = as.formula(sprintf("adonis_data~%s", f_terms)),
                  data = param$work_db$met,
                  method = param$permanova_input$permanova_dist,
                  perm = 999)

  })

  # permanova result summary
  output$permanova_summary <- DT::renderDataTable({
    out <- as.data.frame(fit()$aov.tab)
    DT::datatable(out)
  })

}

## To be copied in the UI
# mod_da_permanova_ui("da_permanova_ui_1")

## To be copied in the server
# callModule(mod_da_permanova_server, "da_permanova_ui_1")

