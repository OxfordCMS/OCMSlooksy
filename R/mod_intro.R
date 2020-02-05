# Module UI

#' @title   mod_intro_ui and mod_intro_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_intro
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
        fluidRow(
          br(), br(),br(),br(), br(),
          column(width = 8,
              h2('About the OCMS Explorer App'),
              # Module 1 - Introduction
              tableOutput(ns('paragraph'))),
          column(width = 4,
              br(),
              h4('About OCMS'),
              tableOutput(ns('about'))))
                 
    )
  )
}

# Module Server

#' @rdname mod_intro
#' @export
#' @keywords internal

mod_intro_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  output$paragraph <- renderText(random_text(nwords = 250))
  output$about <- renderText({ random_text(nwords = 100)})
  
  observeEvent(input$next_tab, {
    updateTabsetPanel(session = parent_session, "tabs", selected = "import")
  })
}

## To be copied in the UI
# mod_intro_ui("intro_ui_1")
    
## To be copied in the server
# callModule(mod_intro_server, "intro_ui_1")
 
