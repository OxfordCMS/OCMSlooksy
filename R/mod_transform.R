#' transform UI Function
#'
#' @description A shiny Module.
#'
#' @param id,bridge$transform_input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_transform_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      h1('Normalise/Transform Read Counts'),
      # wellPanel(width = 12, h3('sub check'), br(), verbatimTextOutput(ns('check'))),
      tags$div("Briefly, surveying an ecosystem based on DNA sequence produces compositional data due to the constant sum constraint of sequencing platforms. Sequence read 'count' is not directly reflective of the absolute count of sequences in the sampled environment because the changes in the absolute abundance of a sequence can only be observed at the expense of other sequences. Lack of independance in sequence counts can result in spurious correlations, ultimately leading to false associations between variables (", a("Gloor et al., 2017", href="https://doi.org/10.3389/fmicb.2017.02224"), ")."),
      tags$div("As mentioned, data transformation aims to overcome the compositional nature of sequencing data. On the other hand, data normalisation is a means of controlling for varying read depth across samples. ", a("Quinn et al. 2018", href = "https://doi.org/10.1093/bioinformatics/bty175"), "describes both types of data correction in their review on analysis methods for compositiotnal data. Please consider each normalisation/transformation option, they are briefly described below. All normalisation/transformations are be applied to the filtered data. Transfomed data will only be applied for the analyses in the current task (horizontal tabs)"),
      br(), 
      p(tags$b("None"), "option does not perform any normalisation nor transformation, and read counts are proceeded with, as is."),
      p(tags$b("Centred log-ratio (CLR) transformation"), " is considered to be both a normalisation and transforamtion step. CLR-transformation is performed here using ", code("ALDEx2::aldex.clr()"), "Please note that this function also uses a probability-based method to impute zeroes in the dataset. In other words, it assumes that zeroes are true, and not missing values due to insufficient sequencing depth (", a("Fernandes et al., 2014", href="https://doi.org/10.1186/2049-2618-2-15"), "). Please note, this step will take a few minutes if data set is large (e.g. >100 samples or >2000 features)"),
      p(tags$b("Log10 of percent abundance"), "(of the sample) performs both sequence normalisation and count transformation"),
      p(tags$b("Percent abundance")," (of the sample) normalises for sequence depth but does not apply any transformation. As such, choosing this option limits the analysis options available in subsequent analyses. At the same time, Sample Dissimiliarity analysis is only available with this option."), br(),
      DT::dataTableOutput(ns('preview_transform')) %>%
        shinycssloaders::withSpinner())
  )
}
    
#' transform Server Function
#'
#' @noRd 
mod_transform_server <- function(input, output, session, bridge){
  ns <- session$ns
 
  asv_transform <- eventReactive(bridge$transform_input$submit_transform, {
    req(bridge$transform_input$transform_method)
    
    asv_df <- as.data.frame(bridge$filtered$asv)
    rownames(asv_df) <- asv_df$featureID
    asv_df <- asv_df[, colnames(asv_df) != 'featureID']
    
    if(bridge$transform_input$transform_method == 'clr') {
      ## generate Monte Carlo samples from Dirichlet distribution
      ## aldex2 zero handling: rows with 0 reads in each sample are deleted prior to analysis
      ## use geometric mean abundance of features
      
      asv_clr <- ALDEx2::aldex.clr(asv_df, conds = bridge$filtered$met$sampleID,
                                   useMC = TRUE)
      clr_instance <- lapply(ALDEx2::getMonteCarloInstances(asv_clr),
                             function(m){t(apply(m,1,median))})
      ## samples in columns
      clr_df <- data.frame(matrix(unlist(clr_instance),
                                  ncol = length(clr_instance),
                                  byrow = FALSE,
                                  dimnames = list(colnames(clr_instance[[1]]),
                                                  names(clr_instance))),
                           stringsAsFactors=FALSE)
      out <- clr_df
    }
    if(bridge$transform_input$transform_method == 'log10') {
      out <- apply(asv_df, 2, function(x) log10(x + 1*10^-6))
    }
    if(bridge$transform_input$transform_method == 'percent') {
      calc <- bridge$filtered$asv %>%
        gather('sampleID','read_count', -featureID) %>%
        group_by(sampleID) %>%
        mutate(sample_total = sum(read_count),
               samp_rel_abund = read_count / sample_total * 100) %>%
        ungroup() %>%
        select(-read_count, -sample_total) %>%
        spread(sampleID, samp_rel_abund)
      
      out <- as.data.frame(calc %>% select(-featureID))
      rownames(out) <- calc$featureID
      
    }
    if(bridge$transform_input$transform_method == 'none') {
      out <- asv_df
    }
    
    out
  })
  
  output$preview_transform <- DT::renderDataTable({
    req(bridge$transform_input$submit_transform, 
        bridge$transform_input$transform_method)
    
    DT::datatable(asv_transform(),
                  extensions = list('Buttons'),
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = "500px",
                    dom = 'Blfrtip',
                    buttons = list(c('copy','csv')))) %>%
      DT::formatRound(column = colnames(asv_transform()), digits = 3)
  })
  
  cross_module <- reactiveValues()
  observe({
    req(bridge$transform_input$submit_transform)
    cross_module$output <- list(
      asv_transform = asv_transform()
    )
  })
  
  return(cross_module)
}
    
## To be copied in the UI
# mod_transform_ui("transform_ui_1")
    
## To be copied in the server
# callModule(mod_transform_server, "transform_ui_1")
 
