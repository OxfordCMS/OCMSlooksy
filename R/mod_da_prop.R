# Module UI

#' @title   mod_da_prop_ui and mod_da_prop_server
#' @description  A shiny submodule of differential abundance analysis.
#' Calculates and visuzlaizes proportionality scores
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param param reactive values to communicate with outer module
#'
#' @rdname mod_da_prop
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import propr
mod_da_prop_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    h1('Proportionality'),
    column(
      width = 12,
      column(
        width = 12, 'Explanation....', br()),
      br(),
      hidden(div(
        id = ns('fdr_summary_div'),
        column(
          width = 8,
          verbatimTextOutput(ns('prop_fdr_summary'))
        ),
        column(
          width = 4,
          "Explanation...Choose the largest cutoff that keeps the FDR below 0.05"
        )
      )),
      br()
    ),
    column(
      width = 12,
      column(
        width = 8,
        h3("Summary of Rho Values"),
        verbatimTextOutput(ns('prop_summary'))
      ),
      column(
        width = 4,
        br(),br(),
        "Explanation..."
      )
    ),
    column(
      width = 12,
      h3('Proportionality Matrix'),
      DT::dataTableOutput(ns('prop_table'))
    )

  )
}

# Module Server

#' @rdname mod_da_prop
#' @export
#' @keywords internal

mod_da_prop_server <- function(input, output, session, param){
  ns <- session$ns

  # unpack data from param------------------------------------------------------
  met <- reactive(param$met)
  asv <- reactive(param$asv)
  tax <- reactive(param$tax)
  asv_transform <- reactive(param$asv_transform)

  prop_calculate <- reactive(param$prop_input$prop_calculate)
  apply_filter <- reactive(param$prop_input$apply_filter)
  rho_filter <- reactive(param$prop_input$rho_filter)
  rho_cutoff <- reactive(param$prop_input$rho_cutoff)
  rho_operator <- reactive(param$prop_input$rho_operator)

  # show/hide fdr summary-------------------------------------------------------
  observeEvent(prop_calculate(), {
    show('fdr_summary_div')
  })

  # calculate rho---------------------------------------------------------------
  propr_obj <- eventReactive(prop_calculate(), {
    # propr package uses propr S4 class to store info -- see propr manual
    count_mat <- asv() %>%
      select(-featureID) %>%
      as.matrix()
    rownames(count_mat) <- asv()$featureID

    # features in columns
    # default setting for ivar is clr transform
    out <- propr(t(count_mat), metric = 'rho')

    # calculate fdr at different cutoffs
    out <- updateCutoffs(out, cutoff = seq(0.05, 0.95, 0.15))
    out
  })

  # showing fdr calculations----------------------------------------------------
  output$prop_fdr_summary <- renderPrint({
    propr_obj()@fdr
  })

  # identify pairs that satisfy filter------------------------------------------
  pairs_keep <- eventReactive(apply_filter(), {
    req(rho_filter())
    if(rho_filter() == 'filter') {
      req(rho_cutoff())
      # keep inside range
      if(rho_operator() == 'inside') {
        lesser <- propr_obj()["<=", max(rho_cutoff())]@pairs
        greater <- propr_obj()[">=", min(rho_cutoff())]@pairs
        out <- intersect(lesser, greater)
      }
      # keep outside range
      else {
        lesser <- propr_obj()["<=", min(rho_cutoff())]@pairs
        greater <- propr_obj()[">=", max(rho_cutoff())]@pairs
        out <- c(lesser, greater)
      }
    }
    # keep all (no filter)
    else {
      out <- propr_obj()["<=", 1]@pairs
    }
    out
  })

  # apply filters
  work_obj <- eventReactive(apply_filter(),{
    out <- propr_obj()
    out@pairs <- pairs_keep()
    out <- simplify(out)
  })

  # extract rho values
  rho_df <- eventReactive(apply_filter(), {
    propr:::proprPairs(work_obj()@matrix)
  })

  # # convert df to matrix
  # rho_mat <- reactive({
  #   work_df() %>%
  #     spread(Partner, rho)
  # })

  output$prop_summary <- renderPrint({
    cat('Rho distribution:\n')
    print(summary(rho_df()$prop))
    cat('Number of Feature Pairs:\n')
    print(length(work_obj()@pairs))
  })

  output$prop_table <- DT::renderDataTable(
    DT::datatable(work_obj()@matrix, extension = 'Buttons',
                   options=list(dom = 'Blfrtip', buttons = c('copy','csv'),
                                scrollX = TRUE)) %>%
      DT::formatRound(column = colnames(work_obj()@matrix), digits = 3)
  )

  output$check <- renderPrint({
    print(head(rho_df()))
  })
}

## To be copied in the UI
# mod_da_prop_ui("da_prop_ui_1")

## To be copied in the server
# callModule(mod_da_prop_server, "da_prop_ui_1")

