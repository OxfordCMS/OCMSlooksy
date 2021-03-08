#' aggregate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_aggregate_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Aggregate Features"),
    # wellPanel(width = 12, h3('sub check'), br(), verbatimTextOutput(ns('check'))),
    h4(textOutput(ns("agg_message"))),
    tags$div("The number of features collapsed is listed in the 'n_collapse' column"),
    DT::dataTableOutput(ns('agg_preview_tax')),
    DT::dataTableOutput(ns('agg_preview_count'))
  )
}
    
#' aggregate Server Function
#'
#' @noRd 
mod_aggregate_server <- function(input, output, session, bridge){
  ns <- session$ns
  
  output$check <- renderPrint({

  })
  
  
  output$agg_message <- renderText({
    sprintf("Aggregating feature counts at the %s level",
            bridge$agg_input$aggregate_by)
  })
  
  # perform aggregation with base aggregate
  aggregated_count <- eventReactive(bridge$agg_input$agg_calculate, {
    
    # set featureID in count_df to aggregation level
    count_df <- bridge$qualfilt_db$asv %>% arrange(featureID)
    
    # copy taxonomy table
    new_featID <- bridge$qualfilt_db$tax %>%
      arrange(featureID) %>%
      select(featureID, .data[[bridge$agg_input$aggregate_by]]) %>%
      mutate(newID = .data[[bridge$agg_input$aggregate_by]],
             newID = ifelse(is.na(newID), paste(bridge$agg_input$aggregate_by, 
                                                'NA', sep="."), newID))
    
    # updating count featureID
    count_df$featureID <- new_featID$newID
    sampleID <- colnames(count_df)
    sampleID <- sampleID[sampleID != 'featureID']
    sampleID <- sprintf("`%s`", sampleID)
    
    # build formula
    yvar <- paste(as.character(sampleID), collapse=',')
    f <- sprintf("cbind(%s) ~ featureID", yvar)
    
    # perform aggregation with base::aggregate  
    aggregate(formula = formula(f), data = count_df, FUN = sum)
  })
  
  # update taxonomy table-------------------------------------------------------
  aggregated_tax <- eventReactive(bridge$agg_input$agg_calculate, {
    tax_level <- c('Kingdom','Phylum','Class','Order', 'Family','Genus',
                   'Species','featureID')
    
    if(bridge$agg_input$aggregate_by != 'featureID') {
      
      # copy tax data over taxa up to aggregated level
      out <- as.data.frame(bridge$qualfilt_db$tax)
      
      # set all tax levels lower than aggregated level to NA
      ind <- which(tax_level == bridge$agg_input$aggregate_by) + 1
      to_na <- tax_level[ind:length(tax_level)]
      
      
      out[, to_na] <- NA
      
      # set sequence column to NA
      out$sequence <- NA
      
      # update Taxon column
      ind <- which(tax_level == bridge$agg_input$aggregate_by)
      
      Taxon <- stringr::str_split(out$Taxon, ";", simplify = TRUE)
      
      if(ind == 1) {
        Taxon <- Taxon[,ind]
      } else {
        Taxon <- apply(Taxon[,1:ind], 1, paste, collapse = ";")  
      }
      
      out$Taxon <- Taxon
      
      # remove redundant entries
      out <- unique(out)
      
      # record how many ASVs aggregated
      n_collapse <- bridge$qualfilt_db$tax %>%
        group_by(.data[[bridge$agg_input$aggregate_by]]) %>%
        summarise(n_collapse = n())
      
      out <- merge(out, n_collapse, bridge$agg_input$aggregate_by)
      
      # set new featureID
      out$featureID <- out[, bridge$agg_input$aggregate_by]
      
    } else {
      out <- as.data.frame(bridge$qualfilt_db$tax)
      out$n_collapse <- 1
    }
    
    # set column order
    out <- out[,c('featureID', 'n_collapse', tax_level[-length(tax_level)], 'Taxon','sequence')]
    out
  })
  
  # show aggregated tables
  output$agg_preview_tax <- DT::renderDataTable({
    
    out <- aggregated_tax() %>%
      mutate_all(as.character)
    DT::datatable(out,
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv')))
  })
  
  output$agg_preview_count <- DT::renderDataTable({
    DT::datatable(aggregated_count(),
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  

  # initiate return list
  cross_module <- reactiveValues()
  observe({
    cross_module$output <- list(
      aggregate_by = bridge$agg_input$aggregate_by,
      aggregated_count = aggregated_count(),
      aggregated_tax = aggregated_tax()
    )
  })

  return(cross_module)
}
    
## To be copied in the UI
# mod_aggregate_ui("aggregate_ui_1")
    
## To be copied in the server
# callModule(mod_aggregate_server, "aggregate_ui_1")
 
