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
          menuItem('Differential Abundance', tabName = "diffAbund_tab"),
          
          # proportionality-----------------------------------------------------
          conditionalPanel(
            condition = "input.menu === 'prop_tab'",
            br(), hr(),
            fixedPanel(
              width = 225,
              tags$div(style = "text-align: center", 
                       tags$b("Proportionality Parameters")),
              sliderInput(ns('rho_cutoff'), "Rho cutoff, absolute value",
                          min = 0, max = 1, value = 0.6),
              selectInput(ns("rho_operator"))
              actionButton(ns('prop_calculate'), 'Calculate')
            )
          )
        )
      ),
      dashboardBody(
        box(
          width = '100%', br(), br(), br(),
          
          wellPanel(width = 12, h3('check'), br(),
                    verbatimTextOutput(ns('check'))),
          
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
  
  # import data into module-----------------------------------------------------
  working_set <- reactive(improxy$data_db)
  
  met <- reactive(working_set()$metadata)
  asv <- reactive(working_set()$asv)
  asv_transform <- reactive(working_set()$t_asv)
  tax <- reactive(working_set()$tax)
  
  # store data in reactiveValues to pass onto submodule-------------------------
  bridge <- reactiveValues()
  observe({
    bridge$met <- improxy$data_db$metadata
    bridge$asv <- improxy$data_db$asv
    bridge$asv_transform <- improxy$data_db$t_asv
    bridge$tax <- improxy$data_db$tax
  })
  
  # proportionality-------------------------------------------------------------
  # add prop inputs
  bridge$prop_input <- reactiveValues()
  observe({
    bridge$prop_input$prop_calculate <- input$prop_calculate
  })
  callModule(mod_da_prop_server, "da_prop_ui_1", bridge)
}
    
## To be copied in the UI
# mod_diffAbund_ui("diffAbund_ui_1")
    
## To be copied in the server
# callModule(mod_diffAbund_server, "diffAbund_ui_1")
 
