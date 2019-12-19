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
mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        shinyjs::shinyjs(),
        # Use sample dataset
        shinyWidgets::materialSwitch(ns("example"),
                                     "Example dataset",
                                     status = 'success'),
        
        br(),
        
        # Upload sqlite database file
        fileInput(ns("db_file"), "Database file", accept = '.db'),
        
        br(),
        
        # Launch data
        actionButton(ns('launch'), 'Launch Dataset')
      ),
      dashboardBody(
        box(width = 12,
            h3('Check Box'), verbatimTextOutput(ns('check'))),
        box(width = 12,
            title = h3('Dataset at a glance'),
            
            tags$b('Read count preview:'),
            DTOutput(ns('table_preview')),
            
            tags$b('Read count summary:'),
            verbatimTextOutput('read_summary'),
            
            tags$b('Read count distribution:'),
            plotOutput(ns('read_distribution')),
            
            tags$b('Number of samples:'),
            textOutput(ns('n_sample'), inline = TRUE)),
        box(width = 12,
            title = h3('Taxonomic assignment'),
            box(width = 4,
                tags$b('Reference Database:'), 
                textOutput(ns('ref_tax'), inline = TRUE),
                
                selectInput(ns('tax_level'), 'Taxonomy level:',
                            choices = c('ASV','Phylum','Class','Order',
                                        'Family','Genus','Species'),
                            selected = 'ASV'),
                tags$b('Number of ASVs:'), 
                textOutput(ns('n_asv'), inline = TRUE)),
            box(width = 8,
                plotOutput(ns('plot_preview')))
        ),
        box(width = 12,
            title = h3('Summary of metadata'),
            DTOutput(ns('metadata_preview')),
            tags$b('Metadata summary'),
            verbatimTextOutput(ns('metadata_summary')),
        )
      )
    )
  )
}

# Module Server-----------------------------------------------------------------

#' @rdname mod_import
#' @export
#' @keywords internal

mod_import_server <- function(input, output, session){
  ns <- session$ns
  
  # read in database file-----------------------------------------------------
  
  data_set <- reactive({
    req(input$db_file)
    
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
    
    ## Extract database tables
    # phylum <- data_set()[[grep('phylum', names(data_set()))]]
    # metadata <- data_set()[[grep('metadata', names(data_set()))]]
    # taxonomy <- data_set()[[grep('taxonomy', names(data_set()))]]
  })
  
  # Use example dataset---------------------------------------------------------
  data_set <- eventReactive(input$example, {
    removeUI("div:has(> #db_file)")
    
    switch(input$example, OCMSexplorer::example_data)
  })
  
  # Launch dataset-------------------------------------------------------------
  observeEvent(input$launch, {
    
    asv <- data_set()$example_asv
    met <- data_set()$example_metadata
    tax <- data_set()$example_taxonomy
    
    output$check <- renderPrint(unique(as.character(tax_view()[[input$tax_level]])))
    
    # Dataset at a glance
    output$table_preview <- renderDT(asv)
    output$read_summary <- renderPrint(summary(asv))
    output$read_distribution <- renderPlot(random_ggplot('histogram'))
    output$n_sample <- renderText(sample(10:50, 1))
    
    output$ref_tax <- renderText(random_text(nwords = 1))
    output$n_asv <- renderText({
      length(unique(as.character(tax_view()[[input$tax_level]])))
    })
    
    # subset and summarize count based on selected taxon
    tax_view <- reactive({
      req(input$tax_level)
      asv %>%
        gather('sampleID','read_count', -ASV) %>%
        inner_join(tax, by = 'ASV') %>%
        group_by(.data[[input$tax_level]]) %>%
        summarize(tot_tax = sum(read_count)) %>%
        data.frame()
    })
    
    output$plot_preview <- renderPlot({
      
      ggplot(tax_view(), aes(x = "", y = tot_tax)) +
        geom_bar(width = 1, stat = 'identity', 
                 aes(fill = .data[[input$tax_level]])) +
        coord_polar('y', start = 0) +
        ggtitle(input$tax_level) +
        theme_minimal(12)
    })
    
    # Summary of metadata
    output$metadata_summary <- renderPrint(summary(met))
    output$metadata_preview <- renderDT(met)
    data_set()
  })
  
  
  
  # asv <- reactive(parse_listDB(data_set(), 'asv'))
  # output$table_summary <- renderPrint(summary(asv()))
  # output$table_preview <- renderTable(asv())
  
  # data_set <- reactive({
  #   req(input$example)
  #   random_DT(50,8)
  #   })
  
}

## To be copied in the UI
# mod_import_ui("import_ui_1")

## To be copied in the server
# callModule(mod_import_server, "import_ui_1")