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
          br(), br(),br(),
          column(width = 8,
              h2('About the OCMS Explorer App'),
              # Module 1 - Introduction
              tags$div("This app is a tool for analyzing marker gene sequencing data after it has been processed through the OCMS bioinformatics pipeline. This tool allows for interactive exploration of microbiome data, and browse through the analyses performed by the OCMS bioinformatics team. All data generated from the analyses and associated plots are downloadable. Finally, a report summarizes all parameters and analyses applied throughout data exploration.")),
          column(width = 4,
              br(),
              h4('About OCMS'),
              tags$div("The Oxford Centre for Microbiome Studies (OCMS) is a research core that provides a comprehensive platform for conducting microbiome science. The OCMS, based at the Kennedy Institute, is comprised of 16 dedicated scientists with expertise spanning across 8 Univeristy of Oxford departments (NDORMS, NDM, RDM, Psychiatry, Zoology, School of Geography and the Environment, Sir William Dunn School of Pathology, Oncology). The inter-disciplinary nature of microbiome science is reflected in the support offered by the OCMS, including microbiology, immunology, gnotobiotic mouse facility, genomics and metagenomics, and bioinformatics. Please vist the ", a("OCMS website", href = "https://www.kennedy.ox.ac.uk/technologies/centre-for-microbiome-studies"), "for details on the type of services offered.")))
    )
  )
}

# Module Server

#' @rdname mod_intro
#' @export
#' @keywords internal

mod_intro_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  observeEvent(input$next_tab, {
    updateTabsetPanel(session = parent_session, "tabs", selected = "import")
  })
}

## To be copied in the UI
# mod_intro_ui("intro_ui_1")
    
## To be copied in the server
# callModule(mod_intro_server, "intro_ui_1")
 
