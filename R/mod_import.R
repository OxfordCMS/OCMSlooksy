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
#' @import RSQLite
#' @import vroom
mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        sidebarMenu(id = 'menu',
                    br(),
          menuItem('Task Info', tabName = 'info_tab_import', 
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('Upload Dataset', tabName = 'upload', selected = FALSE, 
                   startExpanded = TRUE,
                   # Use sample dataset?
                   shinyWidgets::materialSwitch(
                     ns("example"), "Example dataset",
                     inline = TRUE, value = FALSE, status = 'success'),
                   br(),
                   conditionalPanel(
                     condition = paste0("input['", ns('example'), "'] == false"),
                     # Upload sqlite database file
                     fileInput(ns("db_file"), "Database file"),
                     fileInput(ns("metadata_file"), "Metadata file", 
                               accept = c('.csv','.tsv'))),
                   br(),
                   
                   # Launch data
                   actionButton(ns('launch'), 'Launch Dataset'),
                   br(), hr()),
          
          menuItemOutput(ns('metadata_menu')),
          menuItemOutput(ns('asv_menu')),
          menuItemOutput(ns('tax_menu'))
      ),
      
      dashboardBody(
        box(width = "100%",
            br(),br(), br(),
        #   
        # fluidRow(
        #   box(width = 12, h3('Check'),
        #       verbatimTextOutput(ns('check')))),
        tabItems(
          
          # info tab------------------------------------------------------------
          tabItem(
            tabName = 'info_tab_import',
            column(width = 12,
              h1('Import Data'),
              tags$div("Importing the 16S rRNA gene sequences and associated data tables is the first step in the analysis. This is done be uploading the database file produced by the OCMS 16S analysis pipeline. If your data has not been processed through this pipeline, a helper tool is available to help you format your data accordingly (see below for details).", br(),
              h2('Additional Resources'),
              "The database file produced from the OCMS pipeline is a sqlite relational database framework. You can access the data tables in the database by using GUI sqlite tools such as", 
              a('SQLite Browser', href = 'https://sqlitebrowser.org'), ".", 
              br(),
              "If your data has not been processed through the OCMS pipeline, you can format data tables into a sqlite database file using the [create database tool]"),
              br(),
              div(style="font-weight: bold",
                  textOutput(ns('import_status')))
              ),
           
          ),
          # Preview of metadata-------------------------------------------------
          tabItem(
            tabName = 'metadata_menu_tab',
            fluidRow(
              column(width = 12,
                     h1('Preview of metadata'),
                     DT::DTOutput(ns('metadata_preview'))))),
          
          # Preview of read count-----------------------------------------------
          tabItem(
            tabName = 'asv_menu_tab',
            fluidRow(
              column(width = 12,
                     h1('Preview of sequence counts'),
                     DT::DTOutput(ns('asv_preview')))),
            ),
          tabItem(
            tabName = 'tax_menu_tab',
            column(width = 12,
                   h1('Preview of taxonomy'),
                   DT::DTOutput(ns('tax_preview'))))
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
  
  data_set <- eventReactive(input$launch, {
    # read in database file-----------------------------------------------------
    if(input$example == FALSE) {
      req(input$db_file, input$metadata_file)
      
      
      # initialize list of dataframes
      data_ls <- list()
      
      # read in metadata
      metadata <- reactive({
        req(input$metadata_file)

        ext <- tools::file_ext(input$metadata_file$name)
        switch(ext,
               csv = vroom::vroom(input$metadata_file$datapath, delim = ","),
               tsv = vroom::vroom(input$metadata_file$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
      })
      
      data_ls[['metadata']] <- metadata()
      
      # read in database
      con <- RSQLite::dbConnect(RSQLite::SQLite(), input$db_file$datapath)
      
      # extract data tables
      table_ls <- RSQLite::dbListTables(con)
      
      for(i in 1:length(table_ls)) {
        query <- sprintf("SELECT * FROM %s", table_ls[i])
        entry <- RSQLite::dbGetQuery(con, query)
        
        data_ls[[table_ls[i]]] <- entry
      }
      # close connection
      RSQLite::dbDisconnect(con)

      table_ls <- c('merged_abundance_id', 'merged_taxonomy', 'metadata',
                    'merged_filter_summary','merged_qc_summary') # need ymltable
      
      shiny::validate(
        # data_set contains necessary tables
        need(any(table_ls %in% names(data_ls)),
             "database file missing necessary table(s)."),
        # metadata must have sampleID as a identifier
        need("sampleID" %in% colnames(data_ls$metadata), 
             "Metadata must include 'sampleID'."),
        # sampleID must be unique
        need(!any(duplicated(data_ls$metadata$sampleID)),
             "Sample identifiers (sampleID) must be unique."),
        # sampleID matches merge_abundance_id samples exactly
        need(identical(sort(data_ls$metadata$sampleID),
                       sort(colnames(data_ls[['merged_abundance_id']])[2:ncol(data_ls[['merged_abundance_id']])])),
             "sampleID in metadata do not match samples in uploaded database."),
        errorClass = 'importError')
      data_ls
    }
    
    # Use example dataset-------------------------------------------------------
    else {
      switch(input$example, {data_ls <- OCMSExplorer::example_data})  }

  })
  
  # validate dataset------------------------------------------------------------
  observe({
    table_ls <- c('merged_abundance_id', 'merged_taxonomy', 'metadata',
                  'merged_filter_summary','merged_qc_summary') # need ymltable
    output$import_status <- renderText({
      shiny::validate(
        # data_set contains necessary tables
        need(any(table_ls %in% names(data_set())),
             "database file missing necessary table(s)."),
        # metadata must have sampleID as a identifier
        need("sampleID" %in% colnames(data_set()$metadata), 
             "Metadata must include 'sampleID'."),
        # sampleID must be unique
        need(!any(duplicated(data_set()$metadata$sampleID)),
             "Sample identifiers (sampleID) must be unique."),
        # sampleID matches merge_abundance_id samples exactly
        need(identical(sort(data_set()$metadata$sampleID),
                       sort(colnames(data_set()[['merged_abundance_id']])[2:ncol(data_set()[['merged_abundance_id']])])),
             "sampleID in metadata do not match samples in uploaded database."),
        errorClass = 'importError'
      )
      if(class(data_set()) == 'list') {
        "Data validation successful"
      }
    })
  })
  
  # # Check
  # output$check <- renderPrint({
  # 
  # })
  # Launch dataset-------------------------------------------------------------
  observeEvent(input$launch, {
    
    # show menu items
    output$metadata_menu <- renderMenu({
      menuItem('Metadata Preview', tabName = 'metadata_menu_tab', selected = TRUE)
    })
    
    output$asv_menu <- renderMenu({
      menuItem('Sequence Count Preview', tabName = 'asv_menu_tab')
    })
    
    output$tax_menu <- renderMenu({
      menuItem('Taxonomy Preview', tabName = 'tax_menu_tab')
    })
  })  

  asv <- reactive({
    req(input$launch)
    data_set()$merged_abundance_id
  })
  met <- reactive({
    req(input$launch)
    data_set()$metadata
  })
  tax <- reactive({
    req(input$launch)
    data_set()$merged_taxonomy
  })

  # combine tables into working dataframe
  work <- reactive({
    asv() %>%
      gather('sampleID','read_count', -featureID) %>%
      inner_join(tax(), by = 'featureID') %>%
      select(-sequence) %>%
      mutate(read_count = as.numeric(read_count)) %>%
      ungroup()
  })

  # Summary of metadata-------------------------------------------------------
  output$metadata_preview <- DT::renderDT({
    DT::datatable(met(), extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })

  # preview of count table----------------------------------------------------
  output$asv_preview <- DT::renderDT({
    out <- work() %>%
      spread(sampleID, read_count)
    DT::datatable(out, extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })

  output$tax_preview <- DT::renderDT({
    DT::datatable(tax(), extensions = "Buttons",
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
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