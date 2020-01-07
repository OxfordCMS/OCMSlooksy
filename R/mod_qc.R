# Module UI
  
#' @title   mod_qc_ui and mod_qc_server
#' @description  Provides interactive version of QC report returned from dada2 
#' analysis via CGAT-core pipeline
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_qc
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_qc_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
      ),
      dashboardBody(
        # Summary of read counts--------------------------------------------------
        fluidRow(
          box(width = 12,
              title = h3('Summary of read counts'),
              h5('Read count preview:'),
              DT::dataTableOutput(ns('table_preview')),
              
              h5('Read count summary:'),
              verbatimTextOutput('read_summary'),
              
              tags$b('Read count distribution:'),
              plotOutput(ns('read_distribution'))),
          box(width = 4,
              tags$b('ASV Prevelence'),
              plotOutput(ns('asv_prev'))),
          box(width = 4,
              plotOutput(ns('asv_spur')))),
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_qc
#' @export
#' @keywords internal
    
mod_qc_server <- function(input, output, session, improxy){
  ns <- session$ns
  
  data_set <- reactive({improxy$data_db})
  
  
  asv <- data_set()$species_abundance
  met <- data_set()$metadata
  tax <- data_set()$merged_taxonomy
  
  # format taxonomy table
  tax_format <- tax %>% 
    mutate(Taxon = paste(Phylum, Class, Order, Family, 
                         Genus, Species, sep=";"))
  tax_format$Taxon <- stringr::str_replace_all(tax_format$Taxon, "_", "__")
  
  # combine tables into working dataframe
  work <- asv %>%
    gather('sampleID','read_count', -Taxon) %>%
    inner_join(tax_format, by = 'Taxon') %>%
    mutate(read_count = as.numeric(read_count)) %>%
    # adding arbitrary ASV number to use as sequence ID
    mutate(ASV = paste('ASV', stringr::str_pad(1:n(), 3, pad = '0'), 
                       sep = ''))
  
  # preview of count table----------------------------------------------------
  output$table_preview <- DT::renderDataTable({
    spread(wip(), sampleID, agg_count)
  })
  output$read_summary <- renderPrint({
    summary(spread(wip(), sampleID, agg_count))
  })
  
  # read count distribution
  output$read_distribution <- renderPlot({
    
    pdata <- work %>%
      mutate(sampleID = forcats::fct_reorder(sampleID, -agg_count))
    
    ggplot(pdata, aes(x = sampleID, y = agg_count)) +
      geom_bar(stat='identity') +
      theme_classic(12) +
      ggtitle('Histgram of read count')
  })
  
  # prevelance of ASVs across samples
  output$asv_prev <- renderPlot({
    
  })
  
}
    
## To be copied in the UI
# mod_qc_ui("qc_ui_1")
    
## To be copied in the server
# callModule(mod_qc_server, "qc_ui_1")
 
