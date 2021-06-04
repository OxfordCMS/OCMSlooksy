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
                   withBusyIndicatorUI(
                     actionButton(ns('launch'), 'Launch Dataset', class = "btn-primary")
                   ),
                   br(), hr()),
          
          menuItemOutput(ns('metadata_menu')),
          menuItemOutput(ns('asv_menu')),
          menuItemOutput(ns('tax_menu'))
      )),
      
      dashboardBody(
        box(width = "100%",
            br(),br(), br(),

        # fluidRow(
        #   box(width = 12, h3('Check'),
        #       verbatimTextOutput(ns('check')))),
        tabItems(
          
          # info tab------------------------------------------------------------
          tabItem(
            tabName = 'info_tab_import',
            column(
              width = 12,
              fluidRow(
                h1('Import Data'),
                tags$div("Importing the 16S rRNA gene sequences and associated data tables is the first step in the analysis. This is done be uploading the database file produced by the OCMS 16S analysis pipeline. If your data has not been processed through this pipeline, a helper tool is available to help you format your data accordingly (see below for details).", br(),
                         h2('Additional Resources'),
                         "The database file produced from the OCMS pipeline is a sqlite relational database framework. You can access the data tables in the database by using GUI sqlite tools such as", 
                         a('SQLite Browser', href = 'https://sqlitebrowser.org'), ".", 
                         br(),
                         "If your data has not been processed through the OCMS pipeline, you can format data tables into a sqlite database file using the create_db() function. See ", code("?OCMSlooksy::create_db()"), "for details."),
                br(),
                "You can find a tutorial on how to use this app on the", 
                a("OCMS blog", href = "https://oxfordcms.github.io/OCMS-blog/")
              ),
              fluidRow(
                div(style="font-weight: bold",
                    textOutput(ns('import_status'))  %>%
                      shinycssloaders::withSpinner()
                    )
              )
              
            )
          ),
          # Preview of metadata-------------------------------------------------
          tabItem(
            tabName = 'metadata_menu_tab',
            fluidRow(
              column(width = 12,
                     h1('Preview of metadata'),
                     DT::dataTableOutput(ns('metadata_preview'))  %>%
                       shinycssloaders::withSpinner()
                    ))),
          
          # Preview of read count-----------------------------------------------
          tabItem(
            tabName = 'asv_menu_tab',
            fluidRow(
              column(width = 12,
                     h1('Preview of sequence counts'),
                     DT::dataTableOutput(ns('asv_preview'))  %>%
                       shinycssloaders::withSpinner()
                    )),
            ),
          tabItem(
            tabName = 'tax_menu_tab',
            column(width = 12,
                   h1('Preview of taxonomy'),
                   DT::dataTableOutput(ns('tax_preview'))  %>%
                     shinycssloaders::withSpinner()
                  ))
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
  
  # activate launch button once uploaded or if using example data
  observe({
    toggleState('launch', condition = (input$example == TRUE || 
       (!is.null(input$metadata_file) | !is.null(input$db_file$datapath))))
  })
  
  data_set <- eventReactive(input$launch, {
    withBusyIndicatorServer("launch", 'import_ui_1',{
      Sys.sleep(1)
      # read in database file-----------------------------------------------------
      if(input$example == FALSE) {
        req(input$db_file, input$metadata_file)
        
        # initialize list of dataframes
        data_ls <- list()
        
        # read in metadata
        metadata <- reactive({
          req(input$metadata_file)
  
          ext <- tools::file_ext(input$metadata_file$name)
          out <- switch(ext,
                   csv = vroom::vroom(input$metadata_file$datapath, delim = ","),
                   tsv = vroom::vroom(input$metadata_file$datapath, delim = "\t"),
                   validate("Invalid file; Please upload a .csv or .tsv file"))
          
          # check for spaces or special characters
          check_char <- any(grepl("[^[:alnum:]_]", colnames(out)))
          
          if(check_char) {
            # remove all spaces and special characters
            new_colname <- gsub("[^[:alnum:]_]", "_", colnames(out))
            colnames(out) <- new_colname
          }
          out
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
  
      }
      
      # Use example dataset-------------------------------------------------------
      else {
        switch(input$example, {data_ls <- OCMSlooksy::example_data})  
      }
  
      # roll down taxonomy for unclassified taxa--------------------------------
      tax_df <- data_ls$merged_taxonomy %>%
        mutate_all(as.character)
      
      tax_level <- c('Kingdom','Phylum','Class','Order','Family','Genus',
                     'Species')
      
      # work with one column at a time -- not checking Kingdom level
      for(i in 2:length(tax_level)) {
        
        # find row with na in current tax_level
        na_ind <- which(is.na(tax_df[tax_level[i]]))
        
        if(length(na_ind) != 0) {
          
          # look at column before
          curr <- tax_df[, c(tax_level[i-1], tax_level[i])]
          
          # make updated tax_df labels
          curr <- curr %>%
            mutate(updated = ifelse(
              is.na(.data[[tax_level[i]]]), # if current taxon is NA
              # prefix with prev level
              paste(.data[[tax_level[i-1]]], 'unclassified',sep = '_'),
              .data[[tax_level[i]]])) # else keep as current taxaon
          
          # update entire row tax_df table
          tax_df[na_ind, tax_level[i:length(tax_level)]] <- curr$updated[na_ind]
        }
      }
      data_ls$merged_taxonomy <- tax_df
      
      data_ls
    })
  })
  
  # validate dataset------------------------------------------------------------
  table_ls <- c('merged_abundance_id', 'merged_taxonomy', 'metadata',
                'merged_filter_summary','merged_qc_summary', 'parameter_table') 
  
  metaID <- reactive(sort(as.character(data_set()$metadata$sampleID)))
  dbID <- reactive({
    sort(as.character(colnames(data_set()[['merged_abundance_id']])[colnames(data_set()[['merged_abundance_id']]) != 'featureID']))
  })
  msg <- reactive({
    # check sample IDs match
    ref <- unique(c(metaID(), dbID()))
    checkID <- data.frame(refID = ref, metadataID = ref %in% metaID(), 
                          databaseID = ref %in% dbID()) 
    only_in_db <- as.character(checkID$refID[checkID$metadataID == FALSE])
    only_in_met <- as.character(checkID$refID[checkID$databaseID == FALSE])

    msg <- ''
    if(length(only_in_met) > 0) {
      entry <- sprintf("'%s' only found in metadata file.", 
                       paste(only_in_met, collapse = "', '"))
      
      msg <- paste(msg, entry, collapse='')
    }
    if(length(only_in_db) > 0) {
      entry <- sprintf("'%s' only found in database file.",
                       paste(only_in_db, collapse = "', '"))
      msg <- paste(msg, entry, collapse='')
    }
    
    msg
  })
  
  import_status <- reactiveVal("No data imported")
  observeEvent(input$launch,{
    if(!any(table_ls %in% names(data_set()))) {
      import_status("database file missing necessary table(s).")
    } else if(!"sampleID" %in% colnames(data_set()$metadata)) {
      # metadata must have sampleID as a identifier
      import_status("Metadata must include 'sampleID'.")
    } else if(any(duplicated(data_set()$metadata$sampleID))) {
      # sampleID must be unique
      import_status("Sample identifiers (sampleID) must be unique.")
    } else if(!identical(metaID(), dbID())) {
      # sampleID matches merge_abundance_id samples exactly
      import_status(sprintf("Uh oh! sampleID in metadata do not match samples in uploaded database.\n%s", msg()))
    }
    if(class(data_set()) == 'list') {
      import_status("Data validation successful")
    }
  })
  
  output$import_status <- renderText({
    import_status()
  })
  
  # # Check
  # output$check <- renderPrint({
  #
  # })
  # Launch dataset-------------------------------------------------------------
  observeEvent(input$launch, {

    # show menu items
    output$metadata_menu <- renderMenu({
      menuItem('Metadata Preview', tabName = 'metadata_menu_tab', 
               selected = TRUE)
    })
    
    output$asv_menu <- renderMenu({
      menuItem('Sequence Count Preview', tabName = 'asv_menu_tab')
    })
    
    output$tax_menu <- renderMenu({
      menuItem('Taxonomy Preview', tabName = 'tax_menu_tab')
    })

  })  

  asv <- eventReactive(input$launch, {
    data_set()$merged_abundance_id
  })
  met <- eventReactive(input$launch, {
    data_set()$metadata
  })
  tax <- eventReactive(input$launch, {
    data_set()$merged_taxonomy
  })

  # combine tables into working dataframes--------------------------------------
  # reduce loading times downstream
  asv_gather <- eventReactive(input$launch, {
    asv() %>%
      gather('sampleID','read_count', -featureID)
  })
  
  asv_tax <- eventReactive(input$launch, {
    asv_gather() %>%
      inner_join(tax(), by = 'featureID') %>%
      select(-sequence) %>%
      mutate(read_count = as.numeric(read_count)) %>%
      ungroup()
  })
  
  asv_met <- eventReactive(input$launch, {
    asv_gather() %>%
      inner_join(met(), by = 'sampleID')
  })
  
  work <- eventReactive(input$launch, {
    asv_tax() %>% inner_join(met(), by = 'sampleID')
  })
  
  
  # Summary of metadata-------------------------------------------------------
  output$metadata_preview <- DT::renderDT({
    DT::datatable(met(), extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })

  # preview of count table----------------------------------------------------
  output$asv_preview <- DT::renderDT({
    out <- asv_tax() %>%
      spread(sampleID, read_count)
    
    DT::datatable(out, extensions = list(c('Buttons', 'FixedColumns')), 
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE, 
                    dom = 'Blfrtip', 
                    buttons = c('copy','csv'),
                    fixedColumns=list(leftColumns = 2)
                    ))
  })

  output$tax_preview <- DT::renderDT({
    
    DT::datatable(tax() %>% relocate(sequence, .after=last_col()), 
                  extensions = 'Buttons', 
                  options = list(
                    pageLength = 30,
                    scrollX = TRUE, 
                    dom = 'Blfrtip', 
                    buttons = c('copy','csv')))
  })

  # output$check <- renderPrint({
  #   cross_module$import_status
  # })
  # jump to next tab------------------------------------------------------------
  # observeEvent(input$next_tab, {
  #   updateTabsetPanel(session, "tabs", selected = "prepare")
  # })

  # return dataset
  cross_module = reactiveValues()
  observe({
    cross_module$data_db <- data_set()
    # adding long data formats to data list to be passed along in modules
    cross_module$asv_gather <- asv_gather()
    cross_module$asv_tax <- asv_tax()
    cross_module$asv_met <- asv_met()
    cross_module$work <- work()
    cross_module$import_status <- import_status()
  })
  return(cross_module)

}
## To be copied in the UI
# mod_import_ui("import_ui_1")

## To be copied in the server
# callModule(mod_import_server, "import_ui_1")