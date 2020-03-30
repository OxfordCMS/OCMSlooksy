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
        width = 12, 'Explanation....'),
      br(),
      hidden(div(
        id = ns('fdr_summary_div'),
        column(
          width = 8,
          verbatimTextOutput(ns('prop_fdr_summary'))
        ),
        column(
          width = 4,
          "Explanation..."
        )
      ))
    ),
    column(
      width = 12,
      column(
        width = 4,
        wellPanel(
            tags$div(style = "text-align: center",
                     tags$b("Proportionality Parameters")),
            tags$style(HTML(".irs-bar {background: none; border: none}")),
            tags$style(HTML("irs-grid-pol.small {height: 0px;}")),
            tags$style(HTML(".irs-grid-text { font-size: 11pt; }")),
            sliderInput(ns('rho_cutoff'), "Rho cutoff", min = -1, max = 1,
                        value = c(-0.6, 0.6), step = 0.1),
            radioButtons(ns("rho_operator"), "Keep Rho values",
                         choices = c('inside range'='inside',
                                     'outside range' = 'outside'))
        )
      ),
      column(
        width = 8,
        h2("Rho metric"),
        DT::dataTableOutput(ns('prop_table')))
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

  # show/hide ui componnent-----------------------------------------------------
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

  # showing fdr calculations
  output$prop_fdr_summary <- renderPrint({
    propr_obj()@fdr
  })

  # # subsetting dataset based on cutoff
  # sub_obj <- reactive({
  #
  # })
  rho_df <- reactive({
    req(prop_calculate())
    out <- propr_obj()$results

    # add asv ids to results -- map asv to partner/pair
    map <- data.frame(mapID = 1:length(asv()$featureID),
                      featureID = asv()$featureID)
    out <- out %>%
      inner_join(map, c("Partner" = "mapID")) %>%
      select(-Partner) %>%
      rename('Partner' = featureID) %>%
      inner_join(map, c('Pair' = 'mapID')) %>%
      select(-Pair) %>%
      rename('Pair' = featureID)

    out
  })

  # output$prop_table <- DT::renderDataTable(
  #
  # )

  output$check <- renderPrint({
    head()
  })
}

## To be copied in the UI
# mod_da_prop_ui("da_prop_ui_1")

## To be copied in the server
# callModule(mod_da_prop_server, "da_prop_ui_1")

