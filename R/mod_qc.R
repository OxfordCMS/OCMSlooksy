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
#' @import readr
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
                column(width = 12,
                      h1("Filtering Reads"),
                      tags$div('The first stage of the dada2 pipeline is filtering and trimming of reads. The number of reads that remain for downstream analysis is dependent on the parameters that were set for filtering and trimming. In most cases it would be expected that the vast majority of reads will remain after this step. It is noteworthy that dada2 does not accept any "N" bases and so will remove reads if there is an N in the sequence.'),
                      br(),
                      h3("Filtering parameters applied:"),
                      DT::dataTableOutput(ns('filter_yml'))  %>%
                        shinycssloaders::withSpinner(),
                      h3("Filtering Effects"),
                      column(width = 1, style = 'padding:0px;', dropdown(
                        size = 'xs', icon = icon('save'), inline = TRUE, 
                        style = 'material-circle', width = 160,
                        animate = animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                        
                        downloadBttn(ns('dl_filt_original'), 
                                     list(icon('file-image'), "Original plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_filt_html'), 
                                     list(icon('file-code'), "Interactive plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_filt_data'), 
                                     list(icon('file-alt'), "Plot data"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_filt_rds'), 
                                     list(icon('file-prescription'), "RDS"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_filt_all'), 
                                     list(icon('file-archive'), "All"),
                                     size = 'xs', style = 'minimal')
                      )),
                      column(width = 11, style = 'padding:0px;', 
                             jqui_resizable(
                               plotlyOutput(ns('plot_filt'), width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner())
                      )))
              ),
            # denoising-----------------------------------------------------------
            tabItem(
              tabName = 'dada2_denoise',
              fluidRow(
                column(width = 12,
                      h1("Denoising Sequences"),
                      tags$div("The next stage of the dada2 pipeline involves dereplication, sample inference, merging (if paired-end) and chimera removal. Again from the tutorial, dereplication combines all identical sequencing reads into into “unique sequences” with a corresponding “abundance” equal to the number of reads with that unique sequence. These are then taken forward into the sample inference stage and chimera removal. It is useful to see after this has been done how many sequences we are left with. The majority of reads should contribute to the final overall counts.)"),
                      br(),
                      h3("Denoising parameters applied:"),
                      DT::dataTableOutput(ns('denoise_yml')) %>%
                        shinycssloaders::withSpinner(),
                      br(),
                      h3('Denoising Effects'),
                      column(width = 1, style = 'padding:0px;', dropdown(
                        size = 'xs', icon = icon('save'), inline = TRUE, 
                        style = 'material-circle', width = 160,
                        animate = animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                        
                        downloadBttn(ns('dl_nochim_original'), 
                                     list(icon('file-image'), "Original plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nochim_html'), 
                                     list(icon('file-code'), "Interactive plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nochim_data'), 
                                     list(icon('file-alt'), "Plot data"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nochim_rds'), 
                                     list(icon('file-prescription'), "RDS"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nochim_all'), 
                                     list(icon('file-archive'), "All"),
                                     size = 'xs', style = 'minimal')
                      )),
                      column(width = 11, style = 'padding:0px;', 
                             jqui_resizable(
                               plotlyOutput(ns('plot_nochim'), width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner())
                      )))
              ),
            
            # featureID Prevalence------------------------------------------------------
            tabItem(
              tabName = 'asv_prevalence',
              fluidRow(
                column(width = 12,
                      h1("Number of features called per sample and their prevalence"),
                      tags$div("A useful metric is the number of features that were called per sample even though we may not no beforehand the expected diversity in the samples we are analysing. In addition to simply counting the number of features per sample we also plot the prevalence of these features i.e. the proportion of samples that each feature is observed in. By plotting the prevalence against the average relative abundance we get an idea of the presence of spurious features i.e. low prevalence and low abundance."),
                      column(width = 1, style = 'padding:0px;', dropdown(
                        size = 'xs', icon = icon('save'), inline = TRUE, 
                        style = 'material-circle', width = 160,
                        animate = animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                        
                        downloadBttn(ns('dl_nasv_original'), 
                                     list(icon('file-image'), "Original plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nasv_html'), 
                                     list(icon('file-code'), "Interactive plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nasv_data'), 
                                     list(icon('file-alt'), "Plot data"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nasv_rds'), 
                                     list(icon('file-prescription'), "RDS"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_nasv_all'), 
                                     list(icon('file-archive'), "All"),
                                     size = 'xs', style = 'minimal')
                      )),
                      column(width = 11, style = 'padding:0px;',
                             jqui_resizable(
                               plotlyOutput(ns('plot_nasv'), width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner())),
                      br(),
                      column(width = 6, style = 'padding:0px;', 
                        h4('Distribution of Feature Prevalence'),
                        column(width = 1, style = 'padding:0px;', 
                          dropdown(
                            size = 'xs', icon = icon('save'), inline = TRUE, 
                            style = 'material-circle', width = 160,
                           animate = animateOptions(
                             enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                             exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                           
                           downloadBttn(ns('dl_preval_original'), 
                                        list(icon('file-image'), "Original plot"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_preval_html'), 
                                        list(icon('file-code'), "Interactive plot"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_preval_data'), 
                                        list(icon('file-alt'), "Plot data"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_preval_rds'), 
                                        list(icon('file-prescription'), "RDS"),
                                        size = 'xs', style = 'minimal'), br(),
                           downloadBttn(ns('dl_preval_all'), 
                                        list(icon('file-archive'), "All"),
                                        size = 'xs', style = 'minimal'))
                        ),
                        column(width = 10, style = 'padding:0px;', 
                          jqui_resizable(
                            plotlyOutput(
                              ns('plot_prevalence'), width = '100%', height = 'auto') %>% 
                              shinycssloaders::withSpinner())
                        )
                      ),
                      column(width = 6, 
                             h4('Prevalence of features with respects to Relative Abundance'),
                             column(width = 1, style = 'padding:0px;', dropdown(
                               size = 'xs', icon = icon('save'), inline = TRUE, 
                               style = 'material-circle', width = 160,
                               animate = animateOptions(
                                 enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                                 exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                               
                               downloadBttn(ns('dl_spur_original'), 
                                            list(icon('file-image'), "Original plot"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_spur_html'), 
                                            list(icon('file-code'), "Interactive plot"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_spur_data'), 
                                            list(icon('file-alt'), "Plot data"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_spur_rds'), 
                                            list(icon('file-prescription'), "RDS"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_spur_all'), 
                                            list(icon('file-archive'), "All"),
                                            size = 'xs', style = 'minimal')
                             )),
                             column(width=10, style = 'padding:0px;',
                                    jqui_resizable(
                                      plotlyOutput(ns('plot_spurious'), 
                                                   width = '100%', height = 'auto') %>% 
                                        shinycssloaders::withSpinner()
                                      )))
                      ))
              ),
            # rarefaction curve of number seq vs number of asv------------------
            tabItem(
              tabName = 'rarefaction_tab',
              column(width = 12,
                h1("featureID Rarefaction"),
                tags$div("The number of features identified is influenced by the sequencing depth. As such, variation in sequencing depth across samples has the potential to bias the diversity observed. One means of evaluating if sequencing depth is introducing bias in the dataset is by examining a rarefaction curve."),
                actionButton(ns('rare_calculate'), 'Calculate'),
                jqui_resizable(
                  plotlyOutput(ns('plot_rarefaction'),
                               width = '100%', height = 'auto') %>% 
                    shinycssloaders::withSpinner())
              )
            ),
            # taxonomy overview-------------------------------------------------
            tabItem(
              tabName = 'tax_distribution_tab',
              fluidRow(
                column(width = 12,
                      h1("Taxonomy Distribution"),
                      tags$div("The next stage is to assign each of the sequence cluster (such as OTU or ASV), referred to as 'featureID', to a taxonomic group. Below are plots of the taxonomic assignments for each sample (relative abundance at the phylum level) as well as the proportion of all ASVs that could be assigned at each taxonomic rank (phylum-species). We would expect (in most cases) that the majority of ASVs woild be assigned at high taxonomic ranks (e.g. phylum) and fewer at lower taxonomic ranks (e.g. species)."),
                      br(),
                      h3("Taxonomy assigment parameters applied:"),
                      DT::dataTableOutput(ns('taxonomy_yml'))  %>%
                        shinycssloaders::withSpinner(),
                      br(),
                    
                      column(width = 4,
                             br(), br(),
                             wellPanel(
                               # taxonomy level
                               radioButtons(ns('tax_level'), 'Taxonomic level',
                                            c('featureID','Kingdom','Phylum',
                                              'Class', 'Order', 'Family', 
                                              'Genus','Species', 'Taxon'),
                                            selected = 'Phylum')),
                             br(),
                             tags$b('Number of samples:'),
                             textOutput(ns('n_sample'), inline = TRUE),
                             br(),
                             tags$b('Number of Features:'), 
                             textOutput(ns('n_asv'), inline = TRUE),
                             br()),
                      column(width = 8,
                             h2('Table of Distribution of Taxa'),
                             DT::dataTableOutput(ns('tax_distrib_table'))  %>%
                               shinycssloaders::withSpinner()
                            ),
                      column(width = 12,
                             h2('Distribution of Taxa'),
                             column(width = 1, style = 'padding:0px;', dropdown(
                               size = 'xs', icon = icon('save'), inline = TRUE, 
                               style = 'material-circle', width = 160,
                               animate = animateOptions(
                                 enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                                 exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                               
                               downloadBttn(ns('dl_taxdistr_original'), 
                                            list(icon('file-image'), "Original plot"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_taxdistr_html'), 
                                            list(icon('file-code'), "Interactive plot"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_taxdistr_data'), 
                                            list(icon('file-alt'), "Plot data"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_taxdistr_rds'), 
                                            list(icon('file-prescription'), "RDS"),
                                            size = 'xs', style = 'minimal'), br(),
                               downloadBttn(ns('dl_taxdistr_all'), 
                                            list(icon('file-archive'), "All"),
                                            size = 'xs', style = 'minimal')
                             )),
                             column(width = 11, style = 'padding:0px;',
                                    jqui_resizable(
                                      plotlyOutput(ns('tax_distribution'), 
                                                   width = '100%', height = 'auto') %>% 
                                        shinycssloaders::withSpinner()
                                      ))),
                      column(width = 12,
                             h2('Percent assigned:'),
                             jqui_resizable(
                               plotlyOutput(ns('perc_assigned'), width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner()
                               ))
                      
              ))
            ),
            
            # Read count distribution---------------------------------------------
            tabItem(
              tabName = 'group_distribution_tab',
              fluidRow(
                column(width = 12,
                      h1('Read Count Distribution'),
                      tags$div("Examining how reads are distributed across samples can provide insight as to whether or not sequencing depth is even in all samples. If total read count of sample groupings is skewed, it may warrent further investigation. The reason can be biological (not as much DNA in some sample groups) or technical (sequencing was not successful, and should be omitted)"),
                      br(),
                      tags$b('Total read counts across sample or sample groups'),
                      column(width = 1, style = 'padding:0px;', dropdown(
                        size = 'xs', icon = icon('save'), inline = TRUE, 
                        style = 'material-circle', width = 160,
                        animate = animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                        
                        downloadBttn(ns('dl_grpdistr_original'), 
                                     list(icon('file-image'), "Original plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_grpdistr_html'), 
                                     list(icon('file-code'), "Interactive plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_grpdistr_data'), 
                                     list(icon('file-alt'), "Plot data"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_grpdistr_rds'), 
                                     list(icon('file-prescription'), "RDS"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_grpdistr_all'), 
                                     list(icon('file-archive'), "All"),
                                     size = 'xs', style = 'minimal')
                      )),
                      column(width = 11, style = 'padding:0px;',
                             jqui_resizable(
                               plotlyOutput(ns('group_distribution'), width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner()
                               )),
                      br(),
                      tags$div("Similarly, examining the average read count of samples or sample groupings can impart information about any potential biases in the dataset"),
                      br(),
                      tags$b("Distribution of average read counts across sample groups"),
                      column(width = 1, style = 'padding:0px;', dropdown(
                        size = 'xs', icon = icon('save'), inline = TRUE, 
                        style = 'material-circle', width = 160,
                        animate = animateOptions(
                          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
                        
                        downloadBttn(ns('dl_samdistr_original'), 
                                     list(icon('file-image'), "Original plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_samdistr_html'), 
                                     list(icon('file-code'), "Interactive plot"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_samdistr_data'), 
                                     list(icon('file-alt'), "Plot data"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_samdistr_rds'), 
                                     list(icon('file-prescription'), "RDS"),
                                     size = 'xs', style = 'minimal'), br(),
                        downloadBttn(ns('dl_samdistr_all'), 
                                     list(icon('file-archive'), "All"),
                                     size = 'xs', style = 'minimal')
                      )),
                      column(width = 11, style = 'padding:0px;',
                             jqui_resizable(
                               plotlyOutput(ns('sample_distribution'), 
                                            width = '100%', height = 'auto') %>% 
                                 shinycssloaders::withSpinner()
                               )))
              ))
          )
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_qc
#' @export
#' @keywords internal
    
mod_qc_server <- function(input, output, session, improxy){
  ns <- session$ns 
  

  # reading in tables ----------------------------------------------------------
  data_set <- reactive({improxy$data_db})
  
  qc_filtered <- reactive({
    df <- data_set()$merged_filter_summary
    df$reads.in <- df$reads.in - df$reads.out
    df
  })
  
  qc_nochim <- reactive({data_set()$merged_qc_summary})
  yml <- reactive({data_set()$parameter_table})
  asv <- reactive({data_set()$merged_abundance_id})
  met <- reactive({data_set()$metadata})
  tax <- reactive({data_set()$merged_taxonomy})

  # combine tables into working dataframe
  work <- reactive({
    asv() %>%
      gather('sampleID','read_count', -featureID) %>%
      inner_join(tax(), by = 'featureID') %>%
      select(-sequence) %>%
      mutate(read_count = as.numeric(read_count))
  })
  
  
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
  
  output$dl_filt_original <- downloadHandler(
    fname <- function() {"qc_filtered.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_filt())}
  )
  
  output$dl_filt_html <- downloadHandler(
    fname <- function() {"qc_filtered.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_filt())), file)
    }
  )
  
  output$dl_filt_data <- downloadHandler(
    fname <- function() {"qc_filtered.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_filt(), file)
    }
  )
  
  output$dl_filt_rds <- downloadHandler(
    fname <- function() {"qc_filtered.rds"},
    content <- function(file) {
      saveRDS(p_filt(), file)
    }
  )

  output$dl_filt_all <- downloadHandler(
    fname <- function() {"qc_filtered.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_filtered.tiff", "qc_filtered.html",
                  "qc_filtered.csv", "qc_filtered.rds")
      ggsave(to_zip[1], plot=p_filt())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_filt())), to_zip[2])
      write.csv(pdata_filt(), to_zip[3])
      saveRDS(p_filt(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
      }
  )
  
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
  
  output$dl_nochim_original <- downloadHandler(
    fname <- function() {"qc_nochim.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_nochim())}
  )
  
  output$dl_nochim_html <- downloadHandler(
    fname <- function() {"qc_nochim.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_nochim())), file)
    }
  )
  
  output$dl_nochim_data <- downloadHandler(
    fname <- function() {"qc_nochim.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_nochim(), file)
    }
  )
  
  output$dl_nochim_rds <- downloadHandler(
    fname <- function() {"qc_nochim.rds"},
    content <- function(file) {
      saveRDS(p_nochim(), file)
    }
  )
  
  output$dl_nochim_all <- downloadHandler(
    fname <- function() {"qc_nochim.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_nochim.tiff", "qc_nochim.html",
                  "qc_nochim.csv", "qc_nochim.rds")
      ggsave(to_zip[1], plot=p_nochim())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_nochim())), to_zip[2])
      write.csv(pdata_nochim(), to_zip[3])
      saveRDS(p_nochim(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  

  # prevalence of ASVs across samples-----------------------------------------
  pdata_nasv <- reactive({
    work() %>%
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
  
  
  output$dl_nasv_original <- downloadHandler(
    fname <- function() {"qc_nfeature.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_nasv())}
  )
  
  output$dl_nasv_html <- downloadHandler(
    fname <- function() {"qc_nfeature.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_nasv())), file)
    }
  )
  
  output$dl_nasv_data <- downloadHandler(
    fname <- function() {"qc_nfeature.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_nasv(), file)
    }
  )
  
  output$dl_nasv_rds <- downloadHandler(
    fname <- function() {"qc_nfeature.rds"},
    content <- function(file) {
      saveRDS(p_nasv(), file)
    }
  )
  
  output$dl_nasv_all <- downloadHandler(
    fname <- function() {"qc_nfeature.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_nfeature.tiff", "qc_nfeature.html",
                  "qc_nfeature.csv", "qc_nfeature.rds")
      ggsave(to_zip[1], plot=p_nasv())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_nasv())), to_zip[2])
      write.csv(pdata_nasv(), to_zip[3])
      saveRDS(p_nasv(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
  # calculate prevalence
  pprev <- function(d){
    
    # total number of samples
    nsamples <- length(unique(d$sampleID))
    
    # count number of samples with count > 0
    df <- d %>%
      filter(read_count > 0) 
    prev <- (nrow(df)/nsamples)*100
    prev.df <- data.frame(featureID=as.character(unique(d$featureID)),
                          Prevalence=prev)
    return(prev.df)
  }
  
  prevalence <- reactive({
    
    n_sample <- length(unique(work()$sampleID))
    work() %>%
      filter(read_count > 0) %>%
      group_by(featureID) %>%
      summarize(n_observe = n(), 
                Prevalence = n_observe / n_sample)
  })
  
  # tally frequency of prevalence values
  pdata_preval <- reactive({
    hist_table <- hist(prevalence()$Prevalence, plot = FALSE)
    out <- data.frame(bin_mid = hist_table$mids,
                      n_asv = hist_table$counts)
    out
  })
  
  p_preval <- reactive({
    p <- ggplot(pdata_preval(), aes(x = bin_mid, y = n_asv)) +
      geom_point() +
      xlab('Feature Prevalence') +
      ylab('Number of ASVs') +
      theme_bw(12)
    p
  })
  
  output$plot_prevalence <- renderPlotly({
    ggplotly(p_preval())
  })
  
  output$dl_preval_original <- downloadHandler(
    fname <- function() {"qc_preval.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_preval())}
  )
  
  output$dl_preval_html <- downloadHandler(
    fname <- function() {"qc_preval.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_preval())), file)
    }
  )
  
  output$dl_preval_data <- downloadHandler(
    fname <- function() {"qc_preval.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_preval(), file)
    }
  )
  
  output$dl_preval_rds <- downloadHandler(
    fname <- function() {"qc_preval.rds"},
    content <- function(file) {
      saveRDS(p_preval(), file)
    }
  )
  
  output$dl_preval_all <- downloadHandler(
    fname <- function() {"qc_preval.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_preval.tiff", "qc_preval.html",
                  "qc_preval.csv", "qc_preval.rds")
      ggsave(to_zip[1], plot=p_preval())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_preval())), to_zip[2])
      write.csv(pdata_preval(), to_zip[3])
      saveRDS(p_preval(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )

  pdata_spur <- reactive({
    work() %>%
      group_by(sampleID) %>%
      mutate(tot_count = sum(read_count),
             rel_abund = read_count / tot_count * 100) %>%
      ungroup() %>%
      group_by(featureID) %>%
      mutate(avg_abund = mean(rel_abund)) %>%
      left_join(prevalence(), 'featureID') %>%
      distinct(avg_abund, Prevalence)
  })
  
  p_spur <- reactive({
    p <- ggplot(pdata_spur(), aes(x = avg_abund, y = Prevalence)) +
      geom_point(size = 3, alpha = 0.6) +
      scale_y_continuous(limits=c(0,1)) +
      xlab('Mean Relative Abundance (%)') +
      ylab('Feature Prevalence') +
      theme_bw(12)
    p
  })
  
  output$plot_spurious <- renderPlotly({
    ggplotly(p_spur())
  })

  output$dl_spur_original <- downloadHandler(
    fname <- function() {"qc_spur.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_spur())}
  )
  
  output$dl_spur_html <- downloadHandler(
    fname <- function() {"qc_spur.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_spur())), file)
    }
  )
  
  output$dl_spur_data <- downloadHandler(
    fname <- function() {"qc_spur.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_spur(), file)
    }
  )
  
  output$dl_spur_rds <- downloadHandler(
    fname <- function() {"qc_spur.rds"},
    content <- function(file) {
      saveRDS(p_spur(), file)
    }
  )
  
  output$dl_spur_all <- downloadHandler(
    fname <- function() {"qc_spur.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_nochim.tiff", "qc_nochim.html",
                  "qc_nochim.csv", "qc_nochim.rds")
      ggsave(to_zip[1], plot=p_nochim())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_nochim())), to_zip[2])
      write.csv(pdata_nochim(), to_zip[3])
      saveRDS(p_nochim(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
  # rarefaction curve-----------------------------------------------------------
  
  # Check
  # output$check <- renderPrint({
  # })
  
  rare_df <- eventReactive(input$rare_calculate, {
    mat <- asv() %>% select(-featureID)
    rownames(mat) <- asv()$featureID
    mat <- as.matrix(mat)
    cms_rarefy(mat)
  })

  output$plot_rarefaction <- renderPlotly({
    
    pdata <- rare_df() %>%
      inner_join(met(), 'sampleID')
    
    p <- ggplot(pdata, aes(x=Depth, y=Richness, color=sampleID)) +
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
      
    ggplotly(p)
    
  })
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
    work() %>%
      group_by(.data[[input$tax_level]]) %>%
      select(.data[[input$tax_level]], read_count) %>%
      summarise(agg_count = sum(read_count)) %>%
      mutate(agg_perc = agg_count / sum(work()$read_count) * 100) %>%
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
  
  
  output$dl_taxdistr_original <- downloadHandler(
    fname <- function() {"qc_taxdistr.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_taxdistr())}
  )
  
  output$dl_taxdistr_html <- downloadHandler(
    fname <- function() {"qc_taxdistr.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_taxdistr())), file)
    }
  )
  
  output$dl_taxdistr_data <- downloadHandler(
    fname <- function() {"qc_taxdistr.csv"}, 
    content <- function(file) {
      readr::write_csv(tax_distrb_df(), file)
    }
  )
  
  output$dl_taxdistr_rds <- downloadHandler(
    fname <- function() {"qc_taxdistr.rds"},
    content <- function(file) {
      saveRDS(p_taxdistr(), file)
    }
  )
  
  output$dl_taxdistr_all <- downloadHandler(
    fname <- function() {"qc_taxdistr.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_taxdistr.tiff", "qc_taxdistr.html",
                  "qc_taxdistr.csv", "qc_taxdistr.rds")
      ggsave(to_zip[1], plot=p_taxdistr())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_taxdistr())), to_zip[2])
      write.csv(pdata_taxdistr(), to_zip[3])
      saveRDS(p_taxdistr(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  

  # evaluate number ASVs assigned to taxonomy level
  n_assigned <- reactive({
    
    out <- tax() %>% 
      select(-sequence, -Taxon) %>%
      gather('tax_class', 'assignment', -featureID) %>%
      group_by(tax_class) %>%
      summarise(n_NA = sum(is.na(assignment)),
                n_ass = sum(!is.na(assignment)))
    out
  })
  
  output$perc_assigned <- renderPlotly({
    pdata <- n_assigned() %>%
      mutate(n_asv = n_NA + n_ass,
             perc_assigned = n_ass / n_asv * 100,
             tax_class = factor(tax_class, 
                                levels = c('Kingdom','Phylum','Class',
                                           'Order','Family','Genus',
                                           'Species')))
    
    p <- ggplot(pdata, aes(x = tax_class, y = perc_assigned)) +
      geom_bar(stat = 'identity') +
      xlab('Taxonomic Level') +
      ylab('Percent of Features Classified') +
      theme_bw(12)
    
    ggplotly(p)
  })
  
  # read count distribution of samples------------------------------------------

  pdata_grpdistr <- reactive({
    work() %>%
      inner_join(met(), 'sampleID') %>%
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
  
  output$dl_grpdistr_original <- downloadHandler(
    fname <- function() {"qc_grpdistr.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_grpdistr())}
  )
  
  output$dl_grpdistr_html <- downloadHandler(
    fname <- function() {"qc_grpdistr.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_grpdistr())), file)
    }
  )
  
  output$dl_grpdistr_data <- downloadHandler(
    fname <- function() {"qc_grpdistr.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_grpdistr(), file)
    }
  )
  
  output$dl_grpdistr_rds <- downloadHandler(
    fname <- function() {"qc_grpdistr.rds"},
    content <- function(file) {
      saveRDS(p_grpdistr(), file)
    }
  )
  
  output$dl_grpdistr_all <- downloadHandler(
    fname <- function() {"qc_grpdistr.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_grpdistr.tiff", "qc_grpdistr.html",
                  "qc_grpdistr.csv", "qc_grpdistr.rds")
      ggsave(to_zip[1], plot=p_grpdistr())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_grpdistr())), to_zip[2])
      write.csv(pdata_grpdistr(), to_zip[3])
      saveRDS(p_grpdistr(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  

  pdata_samdistr <- reactive({
    asv() %>%
      gather('sampleID', 'read_count', -featureID) %>%
      inner_join(met() %>% mutate_all(as.factor), 'sampleID') %>%
      mutate(sampleID = as.factor(sampleID)) %>%
      group_by(sampleID) %>%
      mutate(selected_var = as.factor(.data[[input$sample_select]]),
             sample_tot = sum(read_count)) %>%
      ungroup() %>%
      group_by(.data[[input$sample_select]]) %>%
      mutate(group_tot = sum(read_count), avg = mean(sample_tot),
             x = as.numeric(selected_var), xavg1 = x - 0.5, xavg2 = x + 0.5) %>%
      select(-read_count, -featureID) %>%
      distinct() %>%
      ungroup()
  })
  
  
  p_samdistr <- reactive({
    ggplot(pdata_samdistr(), aes(x = x, y = sample_tot)) +
      geom_segment(aes(x = xavg1, xend = xavg2, y = avg, yend = avg)) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(breaks = seq(1, length(levels(pdata_samdistr()$selected_var))),
                         labels = levels(pdata_samdistr()$selected_var)) +
      xlab(input$sample_select) +
      ylab('Mean read count within group') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$sample_distribution <- renderPlotly({
    ggplotly(p_samdistr())
  })
  
  
  output$dl_samdistr_original <- downloadHandler(
    fname <- function() {"qc_samdistr.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_samdistr())}
  )
  
  output$dl_samdistr_html <- downloadHandler(
    fname <- function() {"qc_samdistr.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_samdistr())), file)
    }
  )
  
  output$dl_samdistr_data <- downloadHandler(
    fname <- function() {"qc_samdistr.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_samdistr(), file)
    }
  )
  
  output$dl_samdistr_rds <- downloadHandler(
    fname <- function() {"qc_samdistr.rds"},
    content <- function(file) {
      saveRDS(p_samdistr(), file)
    }
  )
  
  output$dl_samdistr_all <- downloadHandler(
    fname <- function() {"qc_samdistr.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("qc_samdistr.tiff", "qc_samdistr.html",
                  "qc_samdistr.csv", "qc_samdistr.rds")
      ggsave(to_zip[1], plot=p_samdistr())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_samdistr())), to_zip[2])
      write.csv(pdata_samdistr(), to_zip[3])
      saveRDS(p_samdistr(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
}
    
## To be copied in the UI
# mod_qc_ui("qc_ui_1")
    
## To be copied in the server
# callModule(mod_qc_server, "qc_ui_1")