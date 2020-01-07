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
mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        # Use sample dataset
        shinyWidgets::materialSwitch(ns("example"), "Example dataset",
                                     inline = TRUE, value = FALSE,
                                     status = 'success'),
        
        br(),
        
        conditionalPanel(condition = paste0("input['", ns('example'), "'] == false"),
                         
                         # Upload sqlite database file
                         fileInput(ns("db_file"), "Database file", accept = '.db')),
        
        br(),
        
        # Launch data
        actionButton(ns('launch'), 'Launch Dataset'),
        
        tags$hr(),

        uiOutput(ns('tax_ui')),
        
        tags$hr(),
        br(),
        br(),
        
        # next tab
        actionButton("next_tab", "Next Step")
        
      ),
      dashboardBody(
        
        # Dataset at a glance---------------------------------------------------
        fluidRow(
          box(width = 8,
              title = h3('Dataset at a glance'),
              
              tags$b('Number of samples:'),
              textOutput(ns('n_sample'), inline = TRUE),
              br(),
              tags$b('Number of ASVs:'), 
              textOutput(ns('n_asv'), inline = TRUE),
              br(),
              tags$b('Reference database:'), 
              textOutput(ns('ref_tax'), inline = TRUE),
              br())),
        
        # Summary of metadata---------------------------------------------------
        fluidRow(
          box(width = 8,
              title = h3('Summary of metadata'),
              DTOutput(ns('metadata_preview'))),
          box(width = 4,
              tags$b('Metadata summary'),
              verbatimTextOutput(ns('metadata_summary')))),
        

        # Summary of taxonomic assigment----------------------------------------
        fluidRow(
          box(width = 8,
              title = h3('Taxonomic assignment'),
              plotOutput(ns('plot_preview'))),
          box(width = 4,
              tags$b('Taxonomy summary:'),
              verbatimTextOutput(ns('tax_summary'))))
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
  
  # read in database file-----------------------------------------------------
  data_set <- reactive({
    
    # validate file is db file
    ext <- tools::file_ext(input$db_file$filename)
    validate(need(ext == 'db', 'Please upload a db file'))
    
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
  })
  
  # Use example dataset---------------------------------------------------------
  data_set <- eventReactive(input$example, {
    
    switch(input$example, OCMSExplorer::example_data)
  })
  
  # Launch dataset-------------------------------------------------------------
  observeEvent(input$launch, {
    
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
      mutate(read_count = as.numeric(read_count))
     
    
    output$tax_ui <- renderUI({
        radioButtons(ns('tax_level'), 'Taxonomic level',
                     c('ASV'='Taxon','Phylum'='Phylum','Class'='Class',
                       'Order'='Order','Family'='Family','Genus'='Genus',
                       'Species'='Species'),
                     selected = 'Taxon')
    })
    
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
    
    # Dataset at a glance-------------------------------------------------------
    output$n_sample <- renderText(length(unique(met$sampleID)))
    output$ref_tax <- renderText(random_text(nwords = 1))
    output$n_asv <- renderText({
      length(unique(wip()$ASV))
    })
    
    # Summary of metadata-------------------------------------------------------
    output$metadata_summary <- renderPrint(summary(met))
    output$metadata_preview <- renderDT(met)

    
    # Distribution of taxonomic assignment--------------------------------------
    pdata <- reactive({
      wip() %>%
      select(.data[[input$tax_level]], agg_count) %>%
      group_by(.data[[input$tax_level]]) %>%
      summarize(agg_count = sum(agg_count))
      })
    
    output$plot_preview <- renderPlot({
      
      ggplot(pdata(), aes(x = "", y = agg_count)) +
        geom_bar(aes_string( fill = input$tax_level), 
                 stat = 'identity', width = 1, colour = 'grey45') +
        coord_polar('y', start = 0) +
        ggtitle('Distribution of Taxonomic assigment') +
        scale_fill_discrete(name = input$tax_level) +
        theme_void(14)
    })
  
    output$tax_summary <- renderPrint({
      summary(pdata())
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