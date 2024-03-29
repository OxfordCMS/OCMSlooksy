#' ov_permanova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sortable
#' @import permute
mod_ov_permanova_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1('PERMANOVA'),
    p('Uses ', code("vegan::adonis2"), "to partition sums of squares using dissimilarities. Terms are assessed sequentially from first to last. Blocks in the experiment design can be accounted for by stratifying the PERMANOVA on the block term. Stratifying by a term means the PERMANOVA will be performed independently within each group of that term"),
    # wellPanel(width = 12, h3('sub check'), br(), verbatimTextOutput(ns('check'))),
    fluidRow(
      column(
        width = 12,
        uiOutput(ns('formula_ui')),
        h4('Formula preview'),
        verbatimTextOutput(ns('formula_preview')),
        uiOutput(ns('stratify_ui')),
        uiOutput(ns('permanova_dist_ui')),
        br(), br(),
        actionButton(ns('permanova_calculate'), 'Calculate PERMANOVA'),
        
        ## Nick Ilott: R4.1.3 I was having issues with the hidden
        ## div so have put this in here. It's not pretty but
        ## works :)      
        br(), br(),
        h4('PERMANOVA Results'),
        DT::dataTableOutput(ns('permanova_summary'))
      )
    )
  )
} 

## Nick Ilott: Keeping this here commented out so 
## is easy to re-implement if required
#    hidden(div(ns('permanova_result_div'),
#      fluidRow(
#        column(
#          width = 12,
#          h3('PERMANOVA Results'),
#          DT::dataTableOutput(ns('permanova_summary'))

#' ov_permanova Server Function
#'
#' @noRd
mod_ov_permanova_server <- function(input, output, session, bridge){
  ns <- session$ns

  # bridge$asv_transform # transformed count
  # bridge$filtered$met # metadata table
  # bridge$filtered$tax # taxonomy table

  ## Nick Ilott: Keeping here in case we decide to re-implement
  # render input ui-------------------------------------------------------------
  #  observeEvent(input$permanova_calculate, {
  #    show('permanova_result_div')
  #  })

  output$formula_ui <- renderUI({
    bucket_list(
      header = "Groups to compare in PERMANOVA",
      add_rank_list(
        input_id = ns('variable_pool'),
        text = "Drag variables from here",
        labels = colnames(bridge$filtered$met)
      ),
      add_rank_list(
        input_id = ns('formula_terms'),
        text = "Drop and order variables here",
        labels = NULL
      )
    )
  })

  # stratify permanova
  output$stratify_ui <- renderUI({
    choices <- c('none', input$variable_pool)
    selectInput(ns("stratify"), "Stratify by", choices = choices,
                selected = 'none')
  })

  formula_preview <- reactive({
    req(input$formula_terms)
    sprintf("~ %s", paste(input$formula_terms, collapse = " + "))
  })

  output$formula_preview <- renderPrint({
    req(input$stratify)
    print(formula_preview())
    if(input$stratify != 'none') {
      cat(sprintf("Block: %s", input$stratify))
    }
  })

  output$permanova_dist_ui <- renderUI({
    if(bridge$transform_method == 'percent') choices <- 'bray'
    else choices <- c("manhattan", "euclidean", "canberra")

    selectInput(ns('permanova_dist'), "Distance method",
                choices = choices,
                selected = choices[1])
  })

  output$check <- renderPrint({

  })

  # perform permanova-----------------------------------------------------------
  fit <- eventReactive(input$permanova_calculate, {
    req(input$stratify)
    set.seed(123)
    validate(
      need(sum(is.na(bridge$filtered$met[,input$formula_terms])) == 0,
           "NA values found in formula variables"),
      need(ifelse(input$stratify != 'none',
                  sum(is.na(bridge$filtered$met[, input$stratify])) == 0,
                  TRUE),
           "NA values found in block variable")
    )
    # need sample in rows
    adonis_data <- as.data.frame(bridge$asv_transform)
    adonis_data <- t(adonis_data)
    f_terms <- paste(input$formula_terms, collapse = '+')
    f <- as.formula(sprintf("adonis_data~%s", f_terms))
    if(input$stratify == 'none') {
      out <- vegan::adonis2(formula = f,
                    data = bridge$filtered$met,
                    method = input$permanova_dist,
                    perm = 999)
    } else {
      perm <- permute::how(nperm=999)
      permute::setBlocks(perm) <- with(bridge$filtered$met,
                                       .data[[input$stratify]])

      out <- vegan::adonis2(formula = f,
                    data = bridge$filtered$met,
                    method = input$permanova_dist,
                    permutations = perm)
    }
    ## Nick Ilott: I think this is all we need and
    ## stops errors with calling as.data.frame on
    ## reactive objects later on
    as.data.frame(suppressWarnings(broom::tidy(out)))
  })

  # permanova result summary
  output$permanova_summary <- DT::renderDataTable({
    fit()
  })

  cross_module <- reactiveValues()
  observe({
    cross_module$output <- list(
      permanova_formula = formula_preview(),
      permanova_summary = fit()
    )
  })

  return(cross_module)
}

## To be copied in the UI
# mod_ov_permanova_ui("ov_permanova_ui_1")

## To be copied in the server
# callModule(mod_ov_permanova_server, "ov_permanova_ui_1")

