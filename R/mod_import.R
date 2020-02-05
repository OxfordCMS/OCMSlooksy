# Module UI

#' @title   mod_import_ui and mod_import_server
#' @description  Import dataset and show basic overview
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_import
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import shinydashboard
#' @import stringr
#' @import dplyr
#' @import forcats
#' @import DBI
mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(id = 'menu',
                    br(),br(), br(),
        menuItem('Upload Dataset', tabName = 'upload', selected = FALSE, startExpanded = TRUE,
                 # Use sample dataset?
                 shinyWidgets::materialSwitch(
                   ns("example"), "Example dataset",
                   inline = TRUE, value = TRUE,
                   status = 'success'),
                 br(),
                 conditionalPanel(
                   condition = paste0("input['", ns('example'), "'] == false"),
                   # Upload sqlite database file
                   fileInput(ns("db_file"), "Database file", accept = '.db')),
                 br(),
                 
                 # Launch data
                 actionButton(ns('launch'), 'Launch Dataset'),
                 br(), hr()),
        
        menuItemOutput(ns('toc1')),
        menuItemOutput(ns('toc2')),
        conditionalPanel(
          condition = "input.menu === 'tax_preview'",
          br(), hr(),
          div(style="text-align: center",
              tags$b('Input controls')),
          fixedPanel(
            radioButtons(ns('tax_level'), 'Taxonomic level',
                         c('ASV'='Taxon','Phylum'='Phylum','Class'='Class',
                           'Order'='Order','Family'='Family','Genus'='Genus',
                           'Species'='Species'),
                         selected = 'Taxon')))
        )),
      
      dashboardBody(
        box(width = "100%",
            br(),br(), br(),
        # Dataset at a glance---------------------------------------------------
        h1('Dataset at a glance'),
        fluidRow(
          box(width = 12, h3('Check'),
              verbatimTextOutput(ns('check')))),
        tabItems(
          
          # Preview of metadata-------------------------------------------------
          tabItem(
            tabName = 'metadata',
            fluidRow(
              column(width = 12,
                     h3('Preview of metadata'),
                     DT::DTOutput(ns('metadata_preview'))))),
          
          # Preview of read count-----------------------------------------------
          tabItem(
            tabName = 'tax_preview',
            fluidRow(
              column(width = 12,
                     h3('Preview of read counts'),
                     DT::DTOutput(ns('read_preview')))),
            )
          )
        )
      )
    )
  )
}

# Module Server-----------------------------------------------------------------

#' @rdname mod_import
#' @export
#' @keywords internal

mod_import_server <- function(input, output, session, parent_session) {
  ns <- session$ns
  
  # Check
  output$check <- renderPrint({
  
  })
  
  data_set <- eventReactive(input$example, {
    # read in database file-----------------------------------------------------
    if(input$example == FALSE) {
      req(input$db_file)
      
      con <- RSQLite::dbConnect(RSQLite::SQLite(), input$db_file$datapath)
      
      # extract data tables
      table_ls <- RSQLite::dbListTables(con)
      
      data_ls <- list()
      for(i in 1:length(table_ls)) {
        query <- sprintf("SELECT * FROM %s", table_ls[i])
        entry <- RSQLite::dbGetQuery(con, query)
        
        data_ls[[table_ls[i]]] <- entry
      }
      RSQLite::dbDisconnect(con)
      data_ls
    }
    
    # Use example dataset---------------------------------------------------------
    else {
      switch(input$example, OCMSExplorer::example_data)  }
  })
  
  
  # Launch dataset-------------------------------------------------------------
  observeEvent(input$launch, {
    
    # show menu items
    output$toc1 <- renderMenu({
      menuItem('Metadata', tabName = 'metadata', selected = TRUE)
    })
    
    output$toc2 <- renderMenu({
      menuItem('Taxonomic Distribution', tabName = 'tax_preview')
    })
    
    asv <- data_set()$species_abundance
    met <- data_set()$metadata
    tax <- data_set()$merged_taxonomy
    
    # format taxonomy table
    tax_format <- tax %>% 
      dplyr::mutate(Taxon = paste(Phylum, Class, Order, Family, 
                           Genus, Species, sep=";"))
    tax_format$Taxon <- stringr::str_replace_all(tax_format$Taxon, "_", "__")
    
    # combine tables into working dataframe
    work <- asv %>%
      gather('sampleID','read_count', -Taxon) %>%
      inner_join(tax_format, by = 'Taxon') %>%
      mutate(read_count = as.numeric(read_count))
     
    
    # customize count data based on selected taxonomic level--------------------
    wip <- reactive({
      work %>%
        group_by(.data[[input$tax_level]], sampleID) %>%
        select(.data[[input$tax_level]], sampleID, read_count) %>%
        summarise(agg_count = sum(read_count)) %>%
        ungroup() %>%
        group_by(sampleID) %>%
        # adding arbitrary ASV number to use as sequence ID
        mutate(ASV = paste('ASV', stringr::str_pad(1:n(), 3, pad = '0'), 
                           sep = '')) %>% 
        ungroup() 
    })
    
    # Summary of metadata-------------------------------------------------------
    output$metadata_summary <- renderPrint({
      out <- apply(met, 2, as.factor)
      summary(out, maxsum = nrow(met))
      })
    output$metadata_preview <- DT::renderDT(met)

    # preview of count table----------------------------------------------------
    output$read_preview <- DT::renderDT({
      out <- wip() %>%
        spread(sampleID, agg_count)
      DT::datatable(out, options = list(scrollX = TRUE))
    })
 
  })
  # jump to next tab------------------------------------------------------------
  observeEvent(input$next_tab, {
    updateTabsetPanel(session, "tabs", selected = "prepare")
  })
  
  # return dataset
  cross_module = reactiveValues()
  observe({
    cross_module$data_db <- data_set()
  })
  return(cross_module)

}
## To be copied in the UI
# mod_import_ui("import_ui_1")

## To be copied in the server
# callModule(mod_import_server, "import_ui_1")