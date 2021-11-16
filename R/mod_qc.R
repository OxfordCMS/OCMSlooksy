# Module UI

#' @title   mod_qc_ui and mod_qc_server
#' @description  Provides interactive version of QC report returned from dada2
#' analysis via CGAT-core pipeline
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_qc
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import tidyr
#' @import plotly
#' @import shinyFiles
#' @import htmlwidgets

#' @import shinyWidgets
mod_qc_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      navlistPanel(
        'sidebartitle',
        id = 'menu',
        well=FALSE,
        widths=c(3,9),

        # fluidRow(
        #   box(width = 12, h3('Check'),
        #       verbatimTextOutput(ns('check')))),

        # module info---------------------------------------------------------
        tabPanel(
          'Module Info',
          id = 'info_tab_qc',
          icon = icon('info-circle'), selected = TRUE,
          fluidRow(
            br(), br(),
            h1("QC Report"),
            div(
              p("Raw sequences were processed through the", a("OCMS 16S rRNA gene pipeline", href="https://ocms-16s.readthedocs.io/en/latest/"),"to assure all sequences used during analysis have been quality controlled. This report outlines the sequence processing steps that occured, and depicts the changes applied to the dataset throughout."),
              p("Sequence processing in the OCMS 16S pipeline follows the",
                a("dada2 tutorial", href="https://benjjneb.github.io/dada2/tutorial.html"), ". Briefly, the sequence processing steps and quality control analysis performed in the pipeline are:"),
              tags$ul(
                tags$li(tags$b("Trim and Filter:"), "Trim off primers, truncate reads, remove reads with 'N' bases, and remove reads based on maximum number of expected errors allowed."),
                tags$li(tags$b("Denoise Sequences:"), "Learn sequencing error rate, infer sample sequences, dereplicate and cluster sequences into ASVs, remove chimeric sequences, merge paired-end reads if applicable"),
                tags$li(tags$b("Sequence Prevalence:"), "Summarises number of ASVs observed in each sample, the prevalence of ASVs across samples, and relating ASV prevalence to mean relative abundance."),
                tags$li(tags$b("Sequence Rarefaction:"), "Assesses effect of read depth on number of features detected in a given sample."),
                tags$li(tags$b("Taxonomic Distribution:"), "Taxonomic classification of ASVs using reference databases. Summarises classification as total relative abundance (summed across all samples) at a given taxonomic level. Summarises proportion of ASVs classified at each taxonomic level"),
                tags$li(tags$b("Sample Distribution:"), "Summarises total read count within a group of samples.")
              ),

            )
          )
        ),
        # filtering-----------------------------------------------------------
        tabPanel(
          'Trim and Filter',
          id = 'dada2_filter',
          fluidRow(
            br(), br(),
            h1("Trim and Filter Reads"),
            tags$div('The first stage of the dada2 pipeline is filtering and trimming of reads. The number of reads that remain for downstream analysis is dependent on the parameters that were set for filtering and trimming. In most cases it would be expected that the vast majority of reads will remain after this step. It is noteworthy that dada2 does not accept any "N" bases and so will remove reads if there is an N in the sequence. Lastly reads are filtered based on the maximum number of expected errors.'),
            br(),
            h3("Filtering parameters applied:"),
            DT::dataTableOutput(ns('filter_yml')),
            h3("Filtering Effects"),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_filter"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('plot_filt'), width = '100%',
                         height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ) # end fluidRow
        ),
        # denoising-----------------------------------------------------------
        tabPanel(
          'Denoise Sequences',
          id = 'dada2_denoise',
          fluidRow(
            br(), br(),
            h1("Denoise Sequences"),
            tags$div("The next stage of the dada2 pipeline involves dereplication, sample inference, merging (if paired-end) and chimera removal. Again from the tutorial, dereplication combines all identical sequencing reads into into “unique sequences” with a corresponding “abundance” equal to the number of reads with that unique sequence. These are then taken forward into the sample inference stage and chimera removal. It is useful to see after this has been done how many sequences we are left with. The majority of reads should contribute to the final overall counts.)"),
            br(),
            h3("Denoising parameters applied:"),
            DT::dataTableOutput(ns('denoise_yml')),
            br(),
            h3('Denoising Effects'),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_nochim"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('plot_nochim'),
                           width = '100%', height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ) # end fluidRow
        ), # end tabPanel
        # featureID Prevalence----------------------------------------------
        tabPanel(
          'Sequence Prevalence',
          id = 'asv_prevalence',
          fluidRow(
            br(), br(),
            h1("Number of features called per sample and their prevalence"),
            tags$div("A useful metric is the number of features that were called per sample even though we may not no beforehand the expected diversity in the samples we are analysing. In addition to simply counting the number of features per sample we also plot the prevalence of these features i.e. the proportion of samples that each feature is observed in. By plotting the prevalence against the average relative abundance we get an idea of the presence of spurious features i.e. low prevalence and low abundance."),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_nasv"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('plot_nasv'),
                           width = '100%', height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ),
          fluidRow(
            column(
              width = 6, style = 'padding:0px;',
              h4('Distribution of Feature Prevalence'),
              column(
                width = 1, style = 'padding:0px;',
                mod_download_ui(ns("download_prevalence"))
              ),
              column(
                width = 11, style = 'padding:0px;',
                plotlyOutput(
                    ns('plot_prevalence'),
                    width = '100%', height = 'auto') %>%
                    shinycssloaders::withSpinner()
              )
            ), # end column 6
            column(
              width = 6,
              h4('Prevalence of features with respects to Relative Abundance'),
              column(
                width = 1, style = 'padding:0px;',
                mod_download_ui(ns("download_spur"))
              ),
              column(
                width=11, style = 'padding:0px;',
                plotlyOutput(ns('plot_spurious'),
                             width = '100%', height = 'auto') %>%
                  shinycssloaders::withSpinner()
              )
            ) # end column 6
          ) # end fluidRow
        ), # end tabPanel
        # rarefaction curve of number seq vs number of asv------------------
        tabPanel(
          'Sequence Rarefaction',
          id = 'rarefaction_tab',
          fluidRow(
            br(), br(),
            h1("featureID Rarefaction"),
            tags$div("The number of features identified is influenced by the sequencing depth. As such, variation in sequencing depth across samples has the potential to bias the diversity observed. One means of evaluating if sequencing depth is introducing bias in the dataset is by examining a rarefaction curve."),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_rare"))
            ),
            column(
              width = 11, stype = 'padding:0px;',
              plotlyOutput(ns('plot_rarefaction'),
                           width = '100%', height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ) # end fluidRow
        ), # end tabPanel
        # taxonomy overview-------------------------------------------------
        tabPanel(
          'Taxonomic Distribution',
          id = 'tax_distribution_tab',
          fluidRow(
            br(), br(),
              h1("Taxonomy Distribution"),
              tags$div("The next stage is to assign each of the sequence cluster (such as OTU or ASV), referred to as 'featureID', to a taxonomic group. Below are plots of the taxonomic assignments for each sample (relative abundance at the phylum level) as well as the proportion of all ASVs that could be assigned at each taxonomic rank (phylum-species). We would expect (in most cases) that the majority of ASVs would be assigned at high taxonomic ranks (e.g. phylum) and fewer at lower taxonomic ranks (e.g. species)."),
              br(),
              h3("Taxonomy assigment parameters applied:"),
              DT::dataTableOutput(ns('taxonomy_yml')) %>%
                shinycssloaders::withSpinner(),
              br(),
          ),
          fluidRow(
            column(
              width = 4,
              br(), br(),
              wellPanel(
               # taxonomy level
               radioButtons(ns('tax_level'), 'Taxonomic level',
                            c('Kingdom','Phylum',
                              'Class', 'Order', 'Family',
                              'Genus','Species'),
                            selected = 'Phylum')),
              br(),
              tags$b('Number of samples:'),
              textOutput(ns('n_sample'), inline = TRUE),
              br(),
              tags$b('Number of Features:'),
              textOutput(ns('n_asv'), inline = TRUE),
              br()
            ),
            column(
              width = 8,
              h2('Table of Distribution of Taxa'),
              DT::dataTableOutput(ns('tax_distrib_table')) %>%
                shinycssloaders::withSpinner()
            )
          ),
          fluidRow(
            h2('Distribution of Taxa'),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_taxdistr"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('tax_distribution'),
                           width = '100%', height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ),
          fluidRow(
            h2('Percent assigned:'),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_assigned"))
            ),
            column(
              width = 11, style = 'padding:0x;',
              plotlyOutput(ns('perc_assigned'), width = '100%',
                           height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ) # end fluidRow
        ), # end tabPanel
        # Read count distribution---------------------------------------------
        tabPanel(
          'Sample Distribution',
          id = 'group_distribution_tab',
          fluidRow(
            br(), br(),
            h1('Read Count Distribution'),
            tags$div("Examining how reads are distributed across samples can provide insight as to whether or not sequencing depth is even in all samples. If total read count of sample groupings is skewed, it may warrent further investigation. The reason can be biological (not as much DNA in some sample groups) or technical (sequencing was not successful, and should be omitted)"),
            br(),
            wellPanel(
              div(style="text-align: center",
                  tags$b('Input controls')),

              # choose sample group
              uiOutput(ns('sample_select_ui'))
            ), hr()
          ),
          fluidRow(
            tags$b('Total read counts across sample or sample groups'),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_grpdistr"))
            ),
            column(
              width = 11, style = 'padding:0px;',
                plotlyOutput(ns('group_distribution'),
                             width = '100%', height = 'auto') %>%
                  shinycssloaders::withSpinner()
            ),
          ),
          fluidRow(
            p("Similarly, examining the average read count of samples or sample groupings can impart information about any potential biases in the dataset"),
            strong("Distribution of average read counts across sample groups"),
            column(
              width = 1, style = 'padding:0px;',
              mod_download_ui(ns("download_samdistr"))
            ),
            column(
              width = 11, style = 'padding:0px;',
              plotlyOutput(ns('sample_distribution'),
                           width = '100%', height = 'auto') %>%
                shinycssloaders::withSpinner()
            )
          ) # end fluidrow
        ), # end tabPanel
        # report------------------------------------------------------------
        tabPanel(
          'Report',
          id = "qc_report",
          fluidRow(
            br(), br(),
            column(
              width = 12,
              mod_report_ui(ns("report_ui_1"))
            )

          )
        ) # end tabPanel
      ) # end navlistPanel
    ) # end fluidPage
  ) # end taglist
}

# Module Server

#' @rdname mod_qc
#' @export
#' @keywords internal

mod_qc_server <- function(input, output, session, improxy){
  ns <- session$ns

  # initiating report-------------------------------------------------------------
  for_report <- reactiveValues()
  # reading in tables ----------------------------------------------------------
  qc_filtered <- reactive({
    df <- improxy$data_db$merged_filter_summary
    df$reads.in <- df$reads.in - df$reads.out
    df
  })

  qc_nochim <- reactive({improxy$data_db$merged_qc_summary})
  asv <- reactive({improxy$data_db$merged_abundance_id})
  met <- reactive({improxy$data_db$metadata})
  tax <- reactive({improxy$data_db$merged_taxonomy})

  yml <- reactive({
    out <- improxy$data_db$parameter_table %>%
      # remove parameter for task paired since not really a function applied
      mutate(parameter = ifelse(task == 'paired', NA, parameter)) %>%
      # ådd dada2_fxn to help understand what's happening at each pipeline step
      mutate(dada2_fxn = NA) %>%
      mutate(dada2_fxn = ifelse(task == 'trim','filterAndTrim', NA),
             dada2_fxn = ifelse(task == 'sample_inference' &
                                  parameter =='nbases',
                                'learnErrors', dada2_fxn),
             # including all dada2 parameters incase they are included in the yml
             dada2_fxn = ifelse(task == 'sample_inference' &
                                  parameter != 'nbases', 'dada', dada2_fxn),
             # remove options as a parameter in dada function
             parameter = ifelse(task == 'sample_inference' & dada2_fxn == 'dada'
                                & parameter == 'options', NA, parameter),
             dada2_fxn = ifelse(task == "taxonomy" &
                                  parameter == 'taxonomy_file',
                                'assignTaxonomy', dada2_fxn),
             dada2_fxn = ifelse(task == "taxonomy" &
                                  parameter == 'species_file', 'addSpecies',
                                dada2_fxn)) %>%
      # adding drepFastq to yml to show dada2 function was used
      # but no parameters and value bc it's only argument is filtered fastq files
      add_row(task = "sample_inference", dada2_fxn = "drepFastq",
              parameter = NA, value = NA) %>%
      add_row(task = "sample_inference", dada2_fxn = "drepFastq",
              parameter = NA, value = NA) %>%
      # adding other dada2 function used in pipeline
      add_row(task = "sample_inference", dada2_fxn = "removeBimeraDenovo",
              parameter = "method", value = "consensus")
    # add mergePairs if paired end reads used
    check_pair <- out %>% filter(task == 'paired')
    if(check_pair$value %in% c('True', 1)) {
      out <- out %>%
        add_row(task = "sample_inference", dada2_fxn = "mergePairs",
                parameter = NA, value = NA)
    }
    out %>%
      select(task, dada2_fxn, parameter, value) %>%
      arrange(task, parameter)
  })


  # Render reactive widgets-----------------------------------------------------
  output$sample_select_ui <- renderUI({
    choices <- colnames(met())
    selectInput(ns('sample_select'), label = "Group samples by:",
                 choices = choices, selected = 'sampleID')
  })

  # Filtering-------------------------------------------------------------------
  output$filter_yml <- DT::renderDataTable(server = FALSE, {

    # add dada2 function that corresponds to yml parameters
    out <- yml() %>%
      filter(task %in% c('paired','trim'))

    DT::datatable(out, colnames = c('OCMS pipeline task','dada2 function',
                                    'parameter','value'),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))
  })

  pdata_filt <- reactive({
    qc_filtered() %>%
      mutate(reads.in = reads.in + reads.out) %>%
      gather(key = 'variable', value = 'value', -sample) %>%
      group_by(variable) %>%
      mutate(sample = forcats::fct_reorder(sample,-value))
  })
  p_filt <- reactive({
    p <- ggplot(pdata_filt(), aes(x=sample, y=value)) +
      geom_point(aes(colour = variable), alpha = 0.6) +
      scale_colour_manual(name = NULL, values=c("red4", "blue4")) +
      theme_bw(12) +
      theme(axis.text.x=element_text(angle=90)) +
      xlab("Sample") +
      ylab("Number of reads")
    p
  })
  output$plot_filt <- renderPlotly({

    ggplotly(p_filt()) %>%
      layout(legend = list(orientation = 'h', x = 0.5, y = -0.5))
  })


  # download data
  for_download1 <- reactiveValues()
  observe({
    for_download1$figure <- p_filt()
    for_download1$fig_data <- pdata_filt()
  })

  callModule(mod_download_server, "download_filter", bridge = for_download1,
             'qc-filtered')

  # add to report
  observe({
    for_report$params <- list(tax_level = input$tax_level,
                              yml = yml(),
                              p_filt = p_filt()
    )
  })
  # Chimera removal-------------------------------------------------------------
  output$denoise_yml <- DT::renderDataTable(server = FALSE, {

    out <- yml() %>%
      filter(task == 'sample_inference')

    DT::datatable(out, colnames = c('OCMS pipeline task','dada2 function',
                                    'parameter','value'),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))

  })
  pdata_nochim <- reactive({
    qc_nochim() %>%
      gather(key = 'variable', value = 'value', -sample) %>%
      group_by(variable) %>%
      mutate(sample = forcats::fct_reorder(sample, -value))
  })
  p_nochim <- reactive({
    p <- ggplot(pdata_nochim(), aes(x=sample, y=value, colour = variable)) +
      geom_point(alpha = 0.6) +
      theme_bw(12) +
      scale_colour_discrete(name = NULL) +
      theme(axis.text.x = element_text(angle=90)) +
      xlab("Sample") +
      ylab("Number of reads")
    p
  })

  output$plot_nochim <- renderPlotly({

    ggplotly(p_nochim()) %>%
      layout(legend = list(orientation = 'h', x = 0.5, y = -0.5))
  })


  for_download2 <- reactiveValues()
  observe({
    for_download2$figure <- p_nochim()
    for_download2$fig_data <- pdata_nochim()
  })

  callModule(mod_download_server, "download_nochim", bridge = for_download2, 'qc-nochim')

  # add to report
  observe({
    for_report$params$p_nochim <- p_nochim()
  })
  # prevalence of ASVs across samples-----------------------------------------
  pdata_nasv <- reactive({
    improxy$asv_gather %>%
      filter(read_count > 0) %>%
      group_by(sampleID) %>%
      summarize(n_asv = n()) %>%
      ungroup() %>%
      mutate(sampleID = fct_reorder(sampleID, -n_asv))
  })

  p_nasv <- reactive({
    ggplot(pdata_nasv(), aes(x = sampleID, y = n_asv)) +
      geom_bar(stat = 'identity') +
      xlab('sampleID') +
      ylab('Number of Features') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
  })

  output$plot_nasv <- renderPlotly({
    ggplotly(p_nasv())
  })


  for_download3 <- reactiveValues()
  observe({
    for_download3$figure <- p_nasv()
    for_download3$fig_data <- pdata_nasv()
  })

  callModule(mod_download_server, "download_nasv", bridge = for_download3, 'qc-nfeature')

  # add to report
  observe({
    for_report$params$p_nasv <- p_nasv()
  })
  # summarise prevalence--------------------------------------------------------
  n_sample <- reactive(length(met()$sampleID))
  prevalence <- reactive({
    improxy$asv_gather %>%
      filter(read_count > 0) %>%
      group_by(featureID) %>%
      summarize(n_observe = n(),
                Prevalence = n_observe / n_sample())
  })

  # tally frequency of prevalence values
  pdata_preval <- reactive({
    tally_table <- as.data.frame(table(prevalence()$n_observe))
    colnames(tally_table) <- c('n_observe', 'n_asv')
    tally_table$perc_prev <- as.numeric(tally_table$n_observe) / n_sample()

    tally_table
  })

  p_preval <- reactive({
    p <- ggplot(pdata_preval(), aes(x = perc_prev, y = n_asv)) +
      geom_point(size = 3, alpha = 0.6) +
      xlab('Feature Prevalence') +
      ylab('Number of ASVs') +
      theme_bw(12)
    p
  })

  output$plot_prevalence <- renderPlotly({
    ggplotly(p_preval())
  })


  for_download4 <- reactiveValues()
  observe({
    for_download4$figure <- p_preval()
    for_download4$fig_data <- pdata_preval()
  })

  callModule(mod_download_server, "download_prevalence", bridge = for_download4,
             'qc-prevalence')
  # add to report
  observe({
    for_report$params$p_preval <- p_preval()
  })

  # prevalence vs mean relative abundance --------------------------------------
  pdata_spur <- reactive({
    improxy$asv_met %>%
      group_by(sampleID) %>%
      mutate(tot_count = sum(read_count),
             rel_abund = round(read_count / tot_count * 100, 2)) %>%
      group_by(featureID) %>%
      mutate(avg_abund = mean(rel_abund)) %>%
      inner_join(prevalence(), 'featureID') %>%
      distinct(avg_abund, Prevalence)
  })

  p_spur <- reactive({
    p <- ggplot(pdata_spur(), aes(x = Prevalence, y = avg_abund)) +
      geom_point(size = 3, alpha = 0.6) +
      ylab('Mean Relative Abundance (%)') +
      xlab('Feature Prevalence') +
      theme_bw(12)
    p
  })

  output$plot_spurious <- renderPlotly({
    ggplotly(p_spur())
  })

  # download data
  for_download5 <- reactiveValues()
  observe({
    for_download5$figure <- p_spur()
    for_download5$fig_data <- pdata_spur()
  })

  callModule(mod_download_server, "download_spur", bridge = for_download5,
             'qc-prevabund')

  # add to report
  observe({
    for_report$params$p_spur <- p_spur()
  })
  # rarefaction curve-----------------------------------------------------------

  rare_df <- reactive({
    mat <- asv() %>% select(-featureID)
    rownames(mat) <- asv()$featureID
    mat <- as.matrix(mat)
    cms_rarefy(mat)
  })

  pdata_rare <- reactive({
    rare_df() %>%
      inner_join(met(), 'sampleID')

  })

  p_rare <- reactive({
    p <- ggplot(pdata_rare(), aes(x=Depth, y=Richness, color=sampleID)) +
      geom_line() +
      scale_colour_discrete(guide=FALSE) +
      theme_bw(12) +
      xlab('Number of sequences') +
      ylab('Number of features')

    p
  })
  output$plot_rarefaction <- renderPlotly({
    ggplotly(p_rare())
  })

  # download data
  for_download6 <- reactiveValues()
  observe({
    for_download6$figure <- p_rare()
    for_download6$fig_data <- pdata_rare()
  })

  callModule(mod_download_server, "download_rare", bridge = for_download6,
             'qc-rarefaction')

  # add to report
  observe({
    for_report$params$p_rare <- p_rare()
  })
  # read count distribution of taxa---------------------------------------------

  # add dada2 functions corresponding to yml parameters
  output$taxonomy_yml <- DT::renderDataTable(server = FALSE, {
    out <- yml() %>%
      filter(task == "taxonomy")

    DT::datatable(out, colnames = c('OCMS pipeline task','dada2 function',
                                    'parameter','value'),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))
  })
  # customize count data based on selected taxonomic level

  output$n_sample <- renderText({length(unique(met()$sampleID))})

  tax_distrb_df <- eventReactive(input$tax_level, {
    tot_count <- sum(improxy$asv_gather$read_count)
    improxy$asv_tax %>%
      group_by(.data[[input$tax_level]]) %>%
      select(.data[[input$tax_level]], read_count) %>%
      summarise(agg_count = sum(read_count)) %>%
      mutate(agg_perc = agg_count / tot_count * 100) %>%
      mutate(agg_perc = round(agg_perc, 2)) %>%
      ungroup() %>%
      distinct(.data[[input$tax_level]], agg_count, agg_perc)
  })

  observeEvent(input$tax_level, {
    output$n_asv <- renderText({
      nrow(unique(tax_distrb_df()[,input$tax_level]))
    })
  })

  output$tax_distrib_table <- DT::renderDataTable(server = FALSE, {
    out <- tax_distrb_df()
    colnames(out) <- c(input$tax_level, "Read Count", "Relative Abundance")
    DT::datatable(out,  extensions = 'Buttons',
                  rownames = FALSE,
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })

  p_taxdistr <- eventReactive(input$tax_level, {
    ggplot(tax_distrb_df(),
                aes(x = 1, y = agg_perc, fill = !!as.symbol(input$tax_level),
                    colour = !!as.symbol(input$tax_level))) +
      geom_bar(stat = 'identity') +
      scale_fill_discrete(name = input$tax_level) +
      scale_y_continuous(limits = c(0,100)) +
      ylab('Total relative abundance (%)') +
      theme_bw(12) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank())
  })

  output$tax_distribution <- renderPlotly({
    ggplotly(p_taxdistr())
  })

  # download data
  for_download7 <- reactiveValues()
  observe({
    for_download7$figure <- p_taxdistr()
    for_download7$fig_data <- tax_distrb_df()
  })

  callModule(mod_download_server, "download_taxdistr", bridge = for_download7,
             'qc-taxdistrib')

  # add to report
  observe({
    for_report$params$tax_distrb_df <- tax_distrb_df()
    for_report$params$p_taxdistr <- p_taxdistr()
  })
  # evaluate number ASVs assigned to taxonomy level-----------------------------
  n_assigned <- reactive({

    out <- tax() %>%
      select(-sequence, -Taxon) %>%
      gather('tax_class', 'assignment', -featureID) %>%
      group_by(tax_class) %>%
      summarise(n_NA = sum(grepl('unclassified',assignment)),
                n_ass = sum(!grepl('unclassified',assignment)))
    out
  })

  pdata_assigned <- reactive({
    n_assigned() %>%
      mutate(n_asv = n_NA + n_ass,
             perc_assigned = n_ass / n_asv * 100,
             tax_class = factor(tax_class,
                                levels = c('Kingdom','Phylum','Class',
                                           'Order','Family','Genus',
                                           'Species')))

  })

  p_assigned <- reactive({
    p <- ggplot(pdata_assigned(), aes(x = tax_class, y = perc_assigned)) +
      geom_bar(stat = 'identity') +
      xlab('Taxonomic Level') +
      ylab('Percent of Features Classified') +
      theme_bw(12)
    p
  })
  output$perc_assigned <- renderPlotly({
    ggplotly(p_assigned())
  })

  # download data
  for_download8 <- reactiveValues()
  observe({
    for_download8$figure <- p_assigned()
    for_download8$fig_data <- pdata_assigned()
  })

  callModule(mod_download_server, "download_assigned", bridge = for_download8, 'qc-assigned')

  observe({
    for_report$params$p_assigned <- p_assigned()
  })
  # read count distribution of samples------------------------------------------

  pdata_grpdistr <- reactive({
    req(input$sample_select)
    improxy$asv_met %>%
      group_by(.data[[input$sample_select]]) %>%
      summarize(group_tot = sum(read_count))
  })

  p_grpdistr <- reactive({
    p <- ggplot(pdata_grpdistr(), aes(x = .data[[input$sample_select]], y = group_tot)) +
      geom_bar(stat = 'identity') +
      xlab(input$sample_select) +
      ylab('Total read count within group') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
  })

  output$group_distribution <- renderPlotly({

    ggplotly(p_grpdistr())

  })

  # download data
  for_download9 <- reactiveValues()
  observe({
    for_download9$figure <- p_grpdistr()
    for_download9$fig_data <- pdata_grpdistr()
  })

  callModule(mod_download_server, "download_grpdistr", bridge = for_download9,
             'qc-grpdistrib')

  observe({
    for_report$params$p_grpdistr <- p_grpdistr()
  })
  # sample distribution --------------------------------------------------------
  pdata_samdistr <- reactive({
    req(input$sample_select)
    met <- improxy$data_db$metadata
    asv <- improxy$data_db$merged_abundance_id[,as.character(met$sampleID)]
    rownames(met) <- met$sampleID
    samdistr <- data.frame(count=colSums(asv))
    rownames(samdistr) <- met$sampleID
    samdistr[,input$sample_select] <- met[,input$sample_select]
    samdistr
  })

  p_samdistr <- reactive({
    ggplot(pdata_samdistr(), aes(x = .data[[input$sample_select]],
                                 y = count,
                                 group = .data[[input$sample_select]])) +
      geom_boxplot() +
      geom_jitter(height=0, width=0.2) +
      xlab(input$sample_select) +
      ylab('Read count') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
  })

  output$sample_distribution <- renderPlotly({
    ggplotly(p_samdistr())
  })

  # download data
  for_download10 <- reactiveValues()
  observe({
    for_download10$figure <- p_samdistr()
    for_download10$fig_data <- pdata_samdistr()
  })

  callModule(mod_download_server, "download_samdistr", bridge = for_download10,
             'qc-samdistrib')

  # add to report
  observe({
    for_report$params$p_samdistr <- p_samdistr()
  })


  # # Check
  # output$check <- renderPrint({
  #
  # })

  callModule(mod_report_server, "report_ui_1", bridge = for_report,
             template = "qc_report",
             file_name = "qc-report")
}

## To be copied in the UI
# mod_qc_ui("qc_ui_1")

## To be copied in the server
# callModule(mod_qc_server, "qc_ui_1")