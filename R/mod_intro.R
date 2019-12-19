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
    fluidPage(h1('OCMS Explorer'),
              column(8,
                     # Module 1 - Introduction
                     tableOutput(ns('paragraph'))),
              column(4,
                     h4('About OCMS'),
                     tableOutput(ns('about')))
    )
  )
}

# Module Server

#' @rdname mod_intro
#' @export
#' @keywords internal

mod_intro_server <- function(input, output, session){
  ns <- session$ns
  
  output$paragraph <- renderText(random_text(nwords = 250))
  
  output$about <- renderText({ random_text(nwords = 100)})
}

## To be copied in the UI
# mod_intro_ui("intro_ui_1")
    
## To be copied in the server
# callModule(mod_intro_server, "intro_ui_1")
 
