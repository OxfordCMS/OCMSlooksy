# Module UI

#' @title   mod_diffAbund_ui and mod_diffAbund_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param improxy import reactive values across modules
#'
#' @rdname mod_diffAbund
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @import plotly
#' @import ALDEx2
#' @import sortable
mod_diffAbund_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(
          id = 'menu', br(),
          menuItem('Task Info', tabName = 'info_tab_diffAbund',
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Proportionality', tabName = 'prop_tab'),
          menuItem('PERMANOVA', tabName = "permanova_tab"),
          menuItem('Differential Abundance', tabName = "diffAbund_tab"),

          # proportionality-----------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'prop_tab'",
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center",
                       tags$b("Proportionality Parameters")),
              withBusyIndicatorUI(
                actionButton(ns('prop_calculate'), 'Calculate')
              ),
              hidden(div(
                id = ns('rho_filter_div'),
                radioButtons(ns('rho_filter'), NULL,
                             choices = c('Show all pairs' = 'all',
                                         'Filter by rho' = 'filter'),
                             selected = 'all'),
                hidden(div(
                  id = ns('rho_slider_div'),
                  tags$style(HTML(".irs-bar {background: none; border: none}")),
                  tags$style(HTML("irs-grid-pol.small {height: 0px;}")),
                  tags$style(HTML(".irs-grid-text { font-size: 11pt; }")),
                  sliderInput(ns('rho_cutoff'), "Rho cutoff", min = -1, max = 1,
                              value = c(-0.6, 0.6), step = 0.01),
                  radioButtons(ns("rho_operator"), "Keep Rho values",
                               choices = c('inside range'='inside',
                                           'outside range' = 'outside'))
                )),
                actionButton(ns('apply_filter'), "Apply filter")
              ))
            )
          )
        )
      ),
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),

          tabItems(
            # main page---------------------------------------------------------
            tabItem(
              tabName = 'info_tab_diffAbund',
              h1('Pair-wise Feature Analysis'),
              column(width = 12, "A suite of tools to perform differntial abundance analysis of pairs of features across sample groups in order to get identify features of interest.")
            ),
            # proportionality---------------------------------------------------
            tabItem(
              tabName = 'prop_tab',
              mod_da_prop_ui(ns("da_prop_ui_1"))
            ),

            # permanova-----------------------------------------------------------
            tabItem(
              tabName = 'permanova_tab',
              h1('PERMANOVA'),
              fluidRow(
                column(
                  width = 12,

                  uiOutput(ns('formula_ui')),
                  h4('Formula preview'),
                  verbatimTextOutput(ns('formula_preview')),
                  uiOutput(ns('permanova_dist_ui')),
                  br(), br(),
                  withBusyIndicatorUI(
                    actionButton(ns('permanova_calculate'), 'Calculate PERMANOVA')
                  ),
                )
              ),
              mod_da_permanova_ui(ns("da_permanova_ui_1"))
            )
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_diffAbund
#' @export
#' @keywords internal

mod_diffAbund_server <- function(input, output, session, improxy){
  ns <- session$ns

  # store data in reactiveValues to pass onto submodule-------------------------
  bridge <- reactiveValues()
  observe({
    bridge$work_db <- improxy$work_db
  })

  # proportionality-------------------------------------------------------------
  # show/hide ui component
  observeEvent(input$prop_calculate, {
    show('rho_filter_div')
  })

  observeEvent(input$rho_filter, {
    if(input$rho_filter == 'filter') {
      show('rho_slider_div')
    }
    else {
      hide('rho_slider_div')
    }
  })

  # add prop inputs
  bridge$prop_input <- reactiveValues()
  observe({
    bridge$prop_input$prop_calculate <- input$prop_calculate
    bridge$prop_input$apply_filter <- input$apply_filter
    bridge$prop_input$rho_filter <- input$rho_filter
    bridge$prop_input$rho_cutoff <- input$rho_cutoff
    bridge$prop_input$rho_operator <- input$rho_operator
  })
  callModule(mod_da_prop_server, "da_prop_ui_1", bridge)

  # permanova-------------------------------------------------------------------


  output$formula_ui <- renderUI({
    # selectInput(ns('variable'), "Select x variable",
    #             choices = colnames(improxy$work_db$met))
    bucket_list(
      header = "Groups to compare in PERMANOVA",
      add_rank_list(
        text = "Drag variables from here",
        labels = colnames(improxy$work_db$met)
      ),
      add_rank_list(
        input_id = ns('formula_terms'),
        text = "Drop and order variables here",
        labels = NULL
      )
    )
  })

  output$formula_preview <- renderPrint({
    req(input$formula_terms)
    sprintf("~ %s", paste(input$formula_terms, collapse = " + "))
  })

  output$permanova_dist_ui <- renderUI({
    if(improxy$work_db$transform_method == 'percent') choices <- 'bray'
    else choices <- c("manhattan", "euclidean", "canberra")

    selectInput(ns('permanova_dist'), "Distance method",
                choices = choices,
                selected = choices[1])
  })

  bridge$permanova_input <- reactiveValues()
  observe({
    bridge$permanova_input$permanova_calculate <- input$permanova_calculate
    bridge$permanova_input$permanova_terms <- input$formula_terms
    bridge$permanova_input$permanova_dist <- input$permanova_dist
  })

  withBusyIndicatorServer('permanova_calculate', 'diffAbund_ui_1', {
    callModule(mod_da_permanova_server, "da_permanova_ui_1", param = bridge)
  })

  # differential abundance------------------------------------------------------
}

## To be copied in the UI
# mod_diffAbund_ui("diffAbund_ui_1")

## To be copied in the server
# callModule(mod_diffAbund_server, "diffAbund_ui_1")

