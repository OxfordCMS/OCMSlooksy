#' transform UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_transform_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h1('Normalise/Transform Read Counts'),
      # wellPanel(width = 12, h3('sub check'), br(), verbatimTextOutput(ns('check'))),
      tags$div("Briefly, surveying an ecosystem based on DNA sequence produces compositional data due to the constant sum constraint of sequencing platforms. Sequence read 'count' is not directly reflective of the absolute count of sequences in the sampled environment because the changes in the absolute abundance of a sequence can only be observed at the expense of other sequences. Lack of independance in sequence counts can result in spurious correlations, ultimately leading to false associations between variables (", a("Gloor et al., 2017", href="https://doi.org/10.3389/fmicb.2017.02224"), ")."),
      tags$div("As mentioned, data transformation aims to overcome the compositional nature of sequencing data. On the other hand, data normalisation is a means of controlling for varying read depth across samples. ", a("Quinn et al. 2018", href = "https://doi.org/10.1093/bioinformatics/bty175"), "describes both types of data correction in their review on analysis methods for compositiotnal data. Please consider each normalisation/transformation option, they are briefly described below. All normalisation/transformations are be applied to the filtered data. Transfomed data will only be applied for the analyses in the Beta-diveristy analysis module"),
      br()
    ),
    fluidRow(
      # transform menu controls
      column(
        width = 3,
        wellPanel(
          radioButtons(ns('transform_method'),
                       tags$div(title = "CLR transforamtion may take a few minutes for large datasets", "Transformation method:"),
                       choices = c('none' = 'none',
                                   'centre log-ratio' = 'clr',
                                   'log10 of percent abundance' = 'log10',
                                   'percent abundance' = 'percent'),
                       selected = 'log10'),
          div(
            style="display:inline-block;",
            withBusyIndicatorUI(
              actionButton(ns('transform_submit'), "Apply change")
            )
          ),
          div(
            style="display:inline-block;",
            withBusyIndicatorUI(
              actionButton(ns('transform_clear'),
                           tags$div(title = "Reset transformation/normalization",
                                    icon("undo-alt"))
              )
            )
          )
        )
      ),
      # explanation
      column(
        width = 9,
        p(tags$b("None"), "option does not perform any normalisation nor transformation, and read counts are proceeded with, as is."),
        p(tags$b("Centred log-ratio (CLR) transformation"), " is considered to be both a normalisation and transforamtion step. CLR-transformation is performed here using ", code("ALDEx2::aldex.clr()"), "Please note that this function also uses a probability-based method to impute zeroes in the dataset. In other words, it assumes that zeroes are missing values due to insufficient sequencing depth, and not a measure of absence. (", a("Fernandes et al., 2014", href="https://doi.org/10.1186/2049-2618-2-15"), "). Please note, this step will take a few minutes if data set is large (e.g. >100 samples or >2000 features)"),
        p(tags$b("Log10 of percent abundance"), "(of the sample) performs both sequence normalisation and count transformation"),
        p(tags$b("Percent abundance")," (of the sample) normalises for sequence depth but does not apply any transformation. As such, choosing this option limits the analysis options available in subsequent analyses. At the same time, Sample Dissimiliarity analysis is only available with this option."),
        br()
      ),
      hr()
    ),
    hidden(div(
      id = ns('transform_result_div'),
      fluidRow(
        DT::dataTableOutput(ns('preview_transform')) %>%
          shinycssloaders::withSpinner())
      )
    ))

}

#' transform Server Function
#'
#' @noRd
mod_transform_server <- function(input, output, session, bridge){
  ns <- session$ns

  observeEvent(input$transform_submit, {
    show('transform_result_div')
  })

  # initiate reactive output
  asv_transform <- reactiveVal()

  withBusyIndicatorServer('transform_submit', 'mod_transform_ui_1', {
    observeEvent(input$transform_submit, {

      asv_df <- as.data.frame(bridge$filtered$asv)
      rownames(asv_df) <- asv_df$featureID
      asv_df <- asv_df[, colnames(asv_df) != 'featureID']

      if(input$transform_method == 'clr') {
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
      if(input$transform_method == 'log10') {
        out <- apply(asv_df, 2, function(x) log10(x + 1*10^-6))
      }
      if(input$transform_method == 'percent') {
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
      if(input$transform_method == 'none') {
        out <- asv_df
      }

      asv_transform(out)
    })
  })

  observeEvent(input$transform_clear, {
    asv_transform(NULL)
  })
  output$preview_transform <- DT::renderDataTable(server = FALSE, {
    validate(need(!is.null(asv_transform()), "No transformation/normatlization method applied. If you want to work with non-transformed/non-normalized counts, please select 'none' under 'Transformation method' and click 'Apply change'"))

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

  # output$check <- renderPrint({
  # })
  cross_module <- reactiveValues()

  observe({
    req(input$transform_submit)
    cross_module$output <-list(asv_transform = asv_transform())
  })

  return(cross_module)
}

## To be copied in the UI
# mod_transform_ui("transform_ui_1")

## To be copied in the server
# callModule(mod_transform_server, "transform_ui_1")

