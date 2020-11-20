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
#' @import shinyFiles
#' @import shinyjqui
#' @import htmlwidgets

#' @import shinyWidgets
mod_qc_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      #sidebar------------------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'menu',
          br(),
          menuItem('Task Info', tabName = 'info_tab_qc',
                   icon = icon('info-circle'), selected = TRUE),
          menuItem('dada2 Filtering', tabName = 'dada2_filter'),
          menuItem('dada2 Denoising', tabName = 'dada2_denoise'),
          menuItem('Sequence Prevalence', tabName = 'asv_prevalence'),
          menuItem('Sequence Rarefaction', tabName = 'rarefaction_tab'),
          menuItem('Taxonomic Distribution', tabName = 'tax_distribution_tab'),
          menuItem('Sample Distribution', tabName = 'group_distribution_tab'),
          
          conditionalPanel(
            condition = "input.menu === 'group_distribution_tab'",
            br(), hr(),
            div(style="text-align: center",
                tags$b('Input controls')),
            
            fixedPanel(
              # choose sample group
              uiOutput(ns('sample_select_ui'))
            )
          )
      )), 
      # dashboard---------------------------------------------------------------
      dashboardBody(
        box(width = '100%', height = 'auto', br(),br(), br(),
          # fluidRow(
          #   box(width = 12, h3('Check'),
          #       verbatimTextOutput(ns('check')))),
          tabItems(
            # main page---------------------------------------------------------
            tabItem(
              tabName = 'info_tab_qc',
              column(width = 12, h1("QC Report"),
              tags$div("Raw sequences were processed through the OCMS 16S rRNA gene pipeline to assure all sequences used during analysis have been quality controlled. This report outlines the processing steps that occured, and depicts the changes applied to the dataset through this process."))
            ),
            # filtering-----------------------------------------------------------
            tabItem(
              tabName = 'dada2_filter',
              fluidRow(
                column(
                  width = 12,
                  h1("Filtering Reads"),
                  tags$div('The first stage of the dada2 pipeline is filtering and trimming of reads. The number of reads that remain for downstream analysis is dependent on the parameters that were set for filtering and trimming. In most cases it would be expected that the vast majority of reads will remain after this step. It is noteworthy that dada2 does not accept any "N" bases and so will remove reads if there is an N in the sequence.'),
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
                    jqui_resizable(
                      plotlyOutput(ns('plot_filt'), width = '100%', 
                                 height = 'auto') %>%
                        shinycssloaders::withSpinner()
                    )
                  )
                ) # end column 12
              ) # end fluidRow
            ),
            # denoising-----------------------------------------------------------
            tabItem(
              tabName = 'dada2_denoise',
              fluidRow(
                column(
                  width = 12,
                  h1("Denoising Sequences"),
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
                    jqui_resizable(
                      plotlyOutput(ns('plot_nochim'), 
                                   width = '100%', height = 'auto') %>%
                        shinycssloaders::withSpinner()
                    )
                  )
                ) # end column 12
              ) # end fluidRow
            ), # end tabitem
            
            # featureID Prevalence------------------------------------------------------
            tabItem(
              tabName = 'asv_prevalence',
              fluidRow(
                column(
                  width = 12,
                  h1("Number of features called per sample and their prevalence"),
                  tags$div("A useful metric is the number of features that were called per sample even though we may not no beforehand the expected diversity in the samples we are analysing. In addition to simply counting the number of features per sample we also plot the prevalence of these features i.e. the proportion of samples that each feature is observed in. By plotting the prevalence against the average relative abundance we get an idea of the presence of spurious features i.e. low prevalence and low abundance."),
                  column(
                    width = 1, style = 'padding:0px;', 
                    mod_download_ui(ns("download_nasv"))
                  ),
                  column(
                    width = 11, style = 'padding:0px;',
                    jqui_resizable(
                      plotlyOutput(ns('plot_nasv'), 
                                   width = '100%', height = 'auto') %>%
                        shinycssloaders::withSpinner()
                    )
                  ),
                  br(),
                  column(
                    width = 6, style = 'padding:0px;', 
                    h4('Distribution of Feature Prevalence'),
                    column(
                      width = 1, style = 'padding:0px;', 
                      mod_download_ui(ns("download_prevalence"))
                    ),
                    column(
                      width = 10, style = 'padding:0px;', 
                      jqui_resizable(
                        plotlyOutput(
                          ns('plot_prevalence'), 
                          width = '100%', height = 'auto') %>%
                          shinycssloaders::withSpinner()
                      )
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
                      width=10, style = 'padding:0px;',
                      jqui_resizable(
                        plotlyOutput(ns('plot_spurious'), 
                                     width = '100%', height = 'auto') %>%
                          shinycssloaders::withSpinner()
                      )
                    )
                  ) # end column 6
                ) # end column 12
              ) # end fluidRow
            ), # end tabitem
            # rarefaction curve of number seq vs number of asv------------------
            tabItem(
              tabName = 'rarefaction_tab',
              column(width = 12,
                h1("featureID Rarefaction"),
                tags$div("The number of features identified is influenced by the sequencing depth. As such, variation in sequencing depth across samples has the potential to bias the diversity observed. One means of evaluating if sequencing depth is introducing bias in the dataset is by examining a rarefaction curve."),
                column(
                  width = 1, style = 'padding:0px;',
                  mod_download_ui(ns("download_rare"))
                ),
                column(
                  width = 11, stype = 'padding:0px;',
                  jqui_resizable(
                    plotlyOutput(ns('plot_rarefaction'),
                                 width = '100%', height = 'auto') %>%
                      shinycssloaders::withSpinner()
                  )  
                )
                
              )
            ),
            # taxonomy overview-------------------------------------------------
            tabItem(
              tabName = 'tax_distribution_tab',
              fluidRow(
                column(
                  width = 12,
                  h1("Taxonomy Distribution"),
                  tags$div("The next stage is to assign each of the sequence cluster (such as OTU or ASV), referred to as 'featureID', to a taxonomic group. Below are plots of the taxonomic assignments for each sample (relative abundance at the phylum level) as well as the proportion of all ASVs that could be assigned at each taxonomic rank (phylum-species). We would expect (in most cases) that the majority of ASVs woild be assigned at high taxonomic ranks (e.g. phylum) and fewer at lower taxonomic ranks (e.g. species)."),
                  br(),
                  h3("Taxonomy assigment parameters applied:"),
                  DT::dataTableOutput(ns('taxonomy_yml')) %>%
                    shinycssloaders::withSpinner(),
                  br(),
                ),
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
                ),
                column(
                  width = 12,
                  h2('Distribution of Taxa'),
                  column(
                    width = 1, style = 'padding:0px;', 
                    mod_download_ui(ns("download_taxdistr"))
                  ),
                  column(
                    width = 11, style = 'padding:0px;',
                    jqui_resizable(
                      plotlyOutput(ns('tax_distribution'), 
                                   width = '100%', height = 'auto') %>%
                        shinycssloaders::withSpinner()
                    )
                  )
                ),
                column(
                  width = 12,
                  h2('Percent assigned:'),
                  column(
                    width = 1, style = 'padding:0px;',
                    mod_download_ui(ns("download_assigned"))
                  ),
                  column(
                    width = 11, style = 'padding:0x;',
                    jqui_resizable(
                      plotlyOutput(ns('perc_assigned'), width = '100%', 
                                   height = 'auto') %>%
                        shinycssloaders::withSpinner()
                    )  
                  )
                )
              ) # end fluidRow
            ), # end tabitem
            
            # Read count distribution---------------------------------------------
            tabItem(
              tabName = 'group_distribution_tab',
              fluidRow(
                column(
                  width = 12,
                  h1('Read Count Distribution'),
                  tags$div("Examining how reads are distributed across samples can provide insight as to whether or not sequencing depth is even in all samples. If total read count of sample groupings is skewed, it may warrent further investigation. The reason can be biological (not as much DNA in some sample groups) or technical (sequencing was not successful, and should be omitted)"),
                  br(),
                  tags$b('Total read counts across sample or sample groups')
                ),
                column(
                  width = 1, style = 'padding:0px;', 
                  mod_download_ui(ns("download_grpdistr"))
                ),
                column(
                  width = 11, style = 'padding:0px;',
                  jqui_resizable(
                    plotlyOutput(ns('group_distribution'), 
                                 width = '100%', height = 'auto') %>%
                      shinycssloaders::withSpinner()
                    )
                ),
                column(
                  width = 12, 
                  br(),
                  tags$div("Similarly, examining the average read count of samples or sample groupings can impart information about any potential biases in the dataset"),
                  br(),
                  tags$b("Distribution of average read counts across sample groups")
                ),
                column(
                  width = 1, style = 'padding:0px;', 
                  mod_download_ui(ns("download_samdistr"))
                ),
                column(
                  width = 11, style = 'padding:0px;',
                  jqui_resizable(
                    plotlyOutput(ns('sample_distribution'), 
                                 width = '100%', height = 'auto') %>%
                      shinycssloaders::withSpinner()
                  )
                )
              ) # end column 12
            ) # end fluidrow
          ) # end tabitem
          
        ) # end tabitems
      ) # end box
    ) # end dashboard body
  ) # end taglist
}
    
# Module Server
    
#' @rdname mod_qc
#' @export
#' @keywords internal
    
mod_qc_server <- function(input, output, session, improxy){
  ns <- session$ns 
  

  # reading in tables ----------------------------------------------------------
  qc_filtered <- reactive({
    df <- improxy$data_db$merged_filter_summary
    df$reads.in <- df$reads.in - df$reads.out
    df
  })
  
  qc_nochim <- reactive({improxy$data_db$merged_qc_summary})
  yml <- reactive({improxy$data_db$parameter_table})
  asv <- reactive({improxy$data_db$merged_abundance_id})
  met <- reactive({improxy$data_db$metadata})
  tax <- reactive({improxy$data_db$merged_taxonomy})
  
  
  # Render reactive widgets-----------------------------------------------------
  output$sample_select_ui <- renderUI({
    choices <- colnames(met())
    radioButtons(ns('sample_select'), label = "Group samples by:",
                 choices = choices, selected = 'sampleID')
  })
  
  # Filtering-------------------------------------------------------------------
  output$filter_yml <- DT::renderDataTable({
    
    # add dada2 function that corresponds to yml parameters
    out <- yml() %>%
      filter(task == 'trim') %>%
      arrange(parameter) %>%
      mutate(dada2_fxn = 'filterAndTrim') %>%
      select(task, dada2_fxn, parameter, value)
    
    DT::datatable(out, colnames = c('pipeline task','dada2 function',
                                    'parameter','value'), 
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
      scale_fill_manual(name = NULL, values=c("red4", "blue4")) +
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
  
  # Chimera removal-------------------------------------------------------------
  output$denoise_yml <- DT::renderDataTable({
    
    # add dada2 functions that correspond to yml parameters
    ## note: this would be safer if all parameters are included in yml
    ## but still works if yml is missing any parameters
    ##### these parameters would be listed under options? #########
    ###### if so, then need to handle differently in yml2Table ####
    out <- yml() %>%
      filter(task == 'sample_inference') %>%
      arrange(parameter) %>%
      mutate(dada2_fxn = ifelse(parameter == 'nbases', 'learnErrors', NA),
             dada2_fxn = ifelse(parameter == 'filtF', 'learnErrors', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'filtR', 'learnErrors', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'omega-a', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'use-quals', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'use-kmers', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'kdist-cutoff', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'band-size', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'gap-penalty', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'homopolymer-ga-penalty', 
                                  'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'min-fold', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'min-hamming', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'min-abundance', 'dada', 
                                  dada2_fxn),
             dada2_fxn = ifelse(parameter == 'max-clust', 'dada', dada2_fxn),
             dada2_fxn = ifelse(parameter == 'max-consist', 'dada', 
                                  dada2_fxn)) %>%
      ## filtF and filtR values for drepFastq should be same as those used in
      ## learnErrors (and not NA) -- need to fix this
      add_row(task = "sample_inference", parameter = "filtF", 
              dada2_fxn = "dreqFastq", value = NA) %>%
      add_row(task = "sample_inference", parameter = "filtR",
              dada2_fxn = "drepFastq", value = NA) %>%
      add_row(task = "sample_inference", parameter = "method", 
              dada2_fxn = "removeBimeraDenovo", value = "consensus") %>%
      add_row(task = "sample_inference", parameter = NA, 
              dada2_fxn = "mergePairs", value = NA) %>%
      select(task, dada2_fxn, parameter, value)
    
    DT::datatable(out, colnames = c('pipeline task','dada2 function',
                                    'parameter','value'), 
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
  
  # rarefaction curve-----------------------------------------------------------
  
  # Check
  # output$check <- renderPrint({
  #   names(improxy)
  # })
  
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
      guides(color = FALSE)
    # geom_label not compatible with plotly
    # geom_label(aes(label=sampleID, colour = sampleID),
    #            fill = alpha(c("white"), 0.2), 
    #            nudge_y = max(rare_df()$Richness)*0.01)
    
    p <- p +
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
  
  # read count distribution of taxa---------------------------------------------
  
  # add dada2 functions corresponding to yml parameters
  output$taxonomy_yml <- DT::renderDataTable({
    out <- yml() %>%
      filter(task == "taxonomy") %>%
      mutate(dada2_fxn = ifelse(parameter == 'taxonomy_file', 'assignTaxonomy', NA),
             dada2_fxn = ifelse(parameter == 'species_file', 'addSpecies', 
                                dada2_fxn)) %>%
      select(task, dada2_fxn, parameter, value)
    
    DT::datatable(out, colnames = c('pipeline task','dada2 function',
                                    'parameter','value'), 
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
  
  output$tax_distrib_table <- DT::renderDataTable({
    out <- tax_distrb_df()
    colnames(out) <- c(input$tax_level, "Read Count", "Relative Abundance")
    DT::datatable(out,  extensions = 'Buttons', 
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

  # evaluate number ASVs assigned to taxonomy level-----------------------------
  n_assigned <- reactive({
    
    out <- tax() %>% 
      select(-sequence, -Taxon) %>%
      gather('tax_class', 'assignment', -featureID) %>%
      group_by(tax_class) %>%
      summarise(n_NA = sum(is.na(assignment)),
                n_ass = sum(!is.na(assignment)))
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
  
  # sample distribution --------------------------------------------------------
  pdata_samdistr <- reactive({
    req(input$sample_select)
    met <- improxy$data_db$metadata
    asv <- improxy$data_db$merged_abundance_id[,met$sampleID]
    rownames(met) <- met$sampleID
    samdistr <- data.frame(count=colSums(asv))
    rownames(samdistr) <- met$sampleID
    samdistr[,input$sample_select] <- met[,input$sample_select]
    samdistr

  })

  p_samdistr <- reactive({
    variable <- input$sample_select
    ggplot(pdata_samdistr(), aes(x=eval(parse(text=variable)), y=count, group=eval(parse(text=variable)))) +
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
  
}
    
## To be copied in the UI
# mod_qc_ui("qc_ui_1")
    
## To be copied in the server
# callModule(mod_qc_server, "qc_ui_1")