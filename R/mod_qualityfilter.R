#' qualityfilter UI Function
#'
#' @description Filter out poor quality samples
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qualityfilter_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(br(),br()),
      sidebarLayout(
        sidebarPanel(
          id = 'menu',
          width=3,
          br(),
          radioButtons(ns('sample_select_prompt'),
                       "Samples to include in analysis:",
                       choices = c('Use all samples' = 'all',
                                   'Include select samples' = 'include',
                                   'Exclude select samples' = 'exclude'),
                       selected = 'all'),
          div(
            style="display:inline-block",
            withBusyIndicatorUI(
              actionButton(ns('submit_sample'), "Filter samples")
            )
          ),
          div(
            style="display:inline-block;",
            withBusyIndicatorUI(
              actionButton(ns('clear_sample'),
                           tags$div(title = "Clear selected samples", icon("undo-alt"))
              )
            )
          )
        ), # end sidebarPanel
        mainPanel(
          width=9,
          # fluidRow(
          # box(width = 12, h3('Check'),
          # verbatimTextOutput(ns('check')))),
            fluidRow(
              h1('Filter Samples'), br(),
              tags$div("It may be desirable to perform analysis on a subset of samples if they are of poor quality or are no longer relevant to the current research question. If you want to continue the analysis with all samples, select 'Use all samples' and click 'Filter samples'. You can reset filtering by using the 'clear selected samples' button (", icon('undo-alt'), ")."),
              br(),
              hidden(div(
                id = ns('sample_filter_div'),
                p("Select samples to include/exclude from the table and click 'Filter samples' to apply changes"), br(),
                DT::dataTableOutput(ns('sample_options_ui'))
              )),
              h3(textOutput(ns('preview_sample_title'))),
              DT::dataTableOutput(ns('preview_sample')) %>%
                shinycssloaders::withSpinner()
            ) # end fluidRow
        ) # end mainPanel
      ) # end sidebarLayout
    ) # end fluidPage
  ) # end taglist
}

#' qualityfilter Server Function
#'
#' @noRd
mod_qualityfilter_server <- function(input, output, session, improxy){
  ns <- session$ns

  # # check
  # output$check <- renderPrint({
  #
  # })
  # import data into module
  met <- reactive(improxy$data_db$metadata)
  asv <- reactive(improxy$data_db$merged_abundance_id)
  tax <- reactive(improxy$data_db$merged_taxonomy)


  # subset samples--------------------------------------------------------------
  # show sample table
  observeEvent(input$sample_select_prompt, {
    toggle("sample_filter_div", condition = input$sample_select_prompt != 'all')
  })


  output$sample_options_ui <- DT::renderDataTable(server = FALSE, {
    out <- met()
    DT::datatable(out, filter = 'top', rownames = FALSE,
                  options = list(scrollX = TRUE))
  })

  all_sample <- reactive(met()$sampleID)

  rows_selected <- reactiveVal()

  # include all samples
  observeEvent(input$submit_sample, {
    if(input$sample_select_prompt == 'all') {
      rows_selected(1:length(all_sample()))
    } else {
      rows_selected(input$sample_options_ui_rows_selected)
    }
  })

  # clear samples
  observeEvent(input$clear_sample, {
    rows_selected(NULL)
    selectRows(dataTableProxy('sample_options_ui'), NULL)
  })

  # map row index to sample names
  sample_include <- reactive({
    if(input$sample_select_prompt == 'exclude') {
      all_sample()[-rows_selected()]
    }
    else {
      all_sample()[rows_selected()]
    }
  })

  # filter samples in data set
  met_filtered <- reactive({
    withBusyIndicatorServer('submit_sample', "setup_ui_1", {
      Sys.sleep(1)
      met() %>%
        filter(sampleID %in% sample_include())
    })

  })


  output$preview_sample_title <- renderText({
    req(rows_selected())
    'Samples included in analysis:'
  })

  output$preview_sample <- DT::renderDataTable(server = FALSE, {
    validate(need(!is.null(rows_selected()), "No samples selected. If you wish to continue analysis with all samples, select 'Use all samples' and click 'filter samples'"))
    DT::datatable(met_filtered(), filter='top', extensions = 'Buttons',
                  rownames = FALSE,
                  options = list(scrollX = TRUE,
                                 pageLength = 30,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })


  # update ASV with filtered samples
  samp_filtered <- reactive({
    improxy$asv_gather %>%
      filter(sampleID %in% unique(met_filtered()$sampleID))
  })

  # store prepared data to pass on to next next module--------------------------
  working_set <- reactive({
    # keep in wide format to be consistent with db format
    list(met = met_filtered(),
         asv = samp_filtered() %>% spread(sampleID, read_count),
         tax = tax(),
         work = samp_filtered() %>%
           inner_join(tax() %>% mutate_all(as.character), 'featureID') %>%
           inner_join(met_filtered(), 'sampleID'),
         sample_select_prompt = input$sample_select_prompt,
         sample_select = all_sample()[rows_selected()],
         submit_sample = input$submit_sample
    )
  })

  # return dataset
  cross_module <- reactiveValues()
  observe({
    req(rows_selected())
    cross_module$work_db <- working_set()
  })
  return(cross_module)

}

## To be copied in the UI
# mod_qualityfilter_ui("qualityfilter_ui_1")

## To be copied in the server
# callModule(mod_qualityfilter_server, "qualityfilter_ui_1")

