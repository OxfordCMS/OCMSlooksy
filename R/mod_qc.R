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
#' 
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
          menuItem('ASV Prevalence', tabName = 'asv_prevalence'),
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
        box(width = '100%', br(),br(), br(),
          
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
                      h1("dada2 filtering reads"),
                      tags$div('The first stage of the dada2 pipeline is filtering and trimming of reads. The number of reads that remain for downstream analysis is dependent on the parameters that were set for filtering and trimming. In most cases it would be expected that the vast majority of reads will remain after this step. It is noteworthy that dada2 does not accept any "N" bases and so will remove reads if there is an N in the sequence.'),
                      shinyjqui::jqui_resizable(plotlyOutput(ns('plot_filt')))
                      ))
              ),
            # denoising-----------------------------------------------------------
            tabItem(
              tabName = 'dada2_denoise',
              fluidRow(
                column(width = 12,
                      h1("De-replication, sample inference, merging and chimera removal"),
                      tags$div("The next stage of the dada2 pipeline involves dereplication, sample inference, merging (if paired-end) and chimera removal. Again from the tutorial, dereplication combines all identical sequencing reads into into “unique sequences” with a corresponding “abundance” equal to the number of reads with that unique sequence. These are then taken forward into the sample inference stage and chimera removal. It is useful to see after this has been done how many sequences we are left with. The majority of reads should contribute to the final overall counts.)"),
                      br(),
                      shinyjqui::jqui_resizable(plotlyOutput(ns('plot_nochim')))
                      ))
              ),
            
            # ASV Prevalence------------------------------------------------------
            tabItem(
              tabName = 'asv_prevalence',
              fluidRow(
                column(width = 12,
                      h1("Number of ASVs called per sample and their prevalence"),
                      tags$div("A useful metric is the number of ASVs that were called per sample even though we may not no beforehand the expected diversity in the samples we are analysing. In addition to simply counting the number of ASVs per sample we also plot the prevalence of these ASVs i.e. the proportion of samples that each ASV is observed in. By plotting the prevalence against the average relative abundance we get an idea of the presence of suprious ASVs i.e. low prevalence and low abundance."),
                      shinyjqui::jqui_resizable(plotlyOutput(ns('plot_nasv'))),
                      br(),
                      column(width = 6, 
                             tags$b('Distribution of ASV Prevalence'),
                             shinyjqui::jqui_resizable(
                               plotlyOutput(ns('plot_prevalence')))),
                      column(width = 6, 
                             tags$b('Prevalence of ASV with respects to Relative Abundance'),
                             shinyjqui::jqui_resizable(plotlyOutput(ns('plot_spurious'))))
                      ))
              ),
            
            # taxonomy overview---------------------------------------------------
            tabItem(
              tabName = 'tax_distribution_tab',
              fluidRow(
                column(width = 12,
                      h1("Taxonomic Distribution"),
                      tags$div("The next stage is to assign each of the amplicon sequence variants (ASV) to a taxonomic group. Below are plots of the taxonomic assignments for each sample (relative abundance at the phylum level) as well as the proportion of all ASVs that could be assigned at each taxonomic rank (phylum-species). We would expect (in most cases) that the majority of ASVs woild be assigned at high taxonomic ranks (e.g. phylum) and fewer at lower taxonomic ranks (e.g. species)."),
                    
                      column(width = 4,
                             br(), br(),
                             wellPanel(
                               # taxonomy level
                               radioButtons(ns('tax_level'), 'Taxonomic level',
                                            c('ASV'='Taxon','Phylum'='Phylum',
                                              'Class'='Class', 'Order'='Order',
                                              'Family'='Family','Genus'='Genus',
                                              'Species'='Species'),
                                            selected = 'Taxon')),
                             br(),
                             tags$b('Number of samples:'),
                             textOutput(ns('n_sample'), inline = TRUE),
                             br(),
                             tags$b('Number of ASVs:'), 
                             textOutput(ns('n_asv'), inline = TRUE),
                             br(),
                             tags$b('Reference database:'), 
                             textOutput(ns('ref_tax'), inline = TRUE)),
                      column(width = 8,
                             h2('Table of Distribution of Taxa'),
                             DT::dataTableOutput(ns('tax_distrib_table'))),
                      column(width = 12,
                             h2('Distribution of Taxa'),
                             shinyjqui::jqui_resizable(
                               plotlyOutput(ns('tax_distribution')))),
                      column(width = 12,
                             h2('Percent assigned:'),
                             shinyjqui::jqui_resizable(plotlyOutput(ns('perc_assigned')))))
                      
              )),
            
            # Read count distribution---------------------------------------------
            tabItem(
              tabName = 'group_distribution_tab',
              fluidRow(
                column(width = 12,
                      h1('Read Count Distribution'),
                      tags$div("Examining how reads are distributed across samples can provide insight as to whether or not sequencing depth is even in all samples. If total read count of sample groupings is skewed, it may warrent further investigation. The reason can be biological (not as much DNA in some sample groups) or technical (sequencing was not successful, and should be omitted)"),
                      br(),
                      tags$b('Total read counts across sample or sample groups'),
                      shinyjqui::jqui_resizable(plotlyOutput(ns('group_distribution'))),
                      br(),
                      tags$div("Similarly, examining the average read count of samples or sample groupings can impart information about any potential biases in the dataset"),
                      br(),
                      tags$b("Distribution of average read counts across sample groups"),
                      shinyjqui::jqui_resizable(plotlyOutput(ns('sample_distribution'))))
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
  
  # Check
  output$check <- renderPrint({
    
  })
  
  # Render reactive widgets
  output$sample_select_ui <- renderUI({
    choices <- colnames(met())
    radioButtons(ns('sample_select'), label = "Group samples by:",
                 choices = choices, selected = 'sampleID')
  })
  
  # reading in tables ----------------------------------------------------------
  data_set <- reactive({improxy$data_db})
  
  qc_filtered <- reactive({
    df <- data_set()$qc_filtered
    df$reads.in <- df$reads.in - df$reads.out
    df
  })
  
  qc_nochim <- reactive({data_set()$qc_nochim})
  
  asv <- reactive({data_set()$species_abundance})
  met <- reactive({data_set()$metadata})
  tax <- reactive({data_set()$merged_taxonomy})

  # format taxonomy table
  tax_format <- reactive({
    tax() %>% 
      mutate(Taxon = paste(Phylum, Class, Order, Family, 
                                Genus, Species, sep=";"),
             Taxon = stringr::str_replace_all(Taxon, "_", "__"))
  })
  
  # combine tables into working dataframe
  work <- reactive({
    asv() %>%
      gather('sampleID','read_count', -Taxon) %>%
      inner_join(tax_format(), by = 'Taxon') %>%
      mutate(read_count = as.numeric(read_count)) %>%
      # adding arbitrary ASV number to use as sequence ID
      group_by(sampleID) %>%
      mutate(ASV = paste('ASV', stringr::str_pad(1:n(), 3, pad = '0'), 
                         sep = '')) %>%
      ungroup()
  })
  

  # Filtering-------------------------------------------------------------------
  # define reads.in as the difference between the starting number and the finishing number. This enables visualisation in a stacked bar chart
  
  output$plot_filt <- renderPlotly({
    pdata <- qc_filtered() %>%
      gather(key = 'variable', value = 'value', -sample) %>%
      group_by(variable) %>%
      mutate(sample = forcats::fct_reorder(sample, -value))

    p <- ggplot(pdata, aes(x=reorder(sample, as.numeric(sample)),
                            y=value, fill=variable)) +
         geom_bar(stat="identity") +
         scale_fill_manual(name = NULL, values=c("red4", "blue4")) +
         theme_bw(12) +
         theme(axis.text.x=element_text(angle=90)) +
         xlab("Sample") + 
         ylab("Number of reads")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = 'h', x = 0.5, y = -0.5))
  })
  
  # Chimera removal-------------------------------------------------------------
  output$plot_nochim <- renderPlotly({
    
    pdata <- qc_nochim() %>% 
      gather(key = 'variable', value = 'value', -sample) %>%
      group_by(variable) %>%
      mutate(sample = forcats::fct_reorder(sample, -value))
    
    p <- ggplot(pdata, aes(x=sample, y=value, colour = variable)) +
      geom_point(alpha = 0.6) +
      theme_bw(12) +
      scale_colour_discrete(name = NULL) +
      theme(axis.text.x = element_text(angle=90)) +
      xlab("Sample") + 
      ylab("Number of reads")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = 'h', x = 0.5, y = -0.5))
  })
  
  
  # prevalence of ASVs across samples-----------------------------------------
  output$plot_nasv <- renderPlotly({
    pdata <- work() %>%
      group_by(sampleID) %>%
      distinct(sampleID, ASV) %>%
      summarize(n_asv = n()) %>%
      ungroup() %>%
      mutate(sampleID = fct_reorder(sampleID, -n_asv))
      

    y_breaks <- seq(0, max(pdata$n_asv), 10)
    p <- ggplot(pdata, aes(x = sampleID, y = n_asv)) +
      geom_bar(stat = 'identity') +
      xlab('sampleID') +
      ylab('Number of ASVs') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p)
  })
  
  # calculate prevalence
  pprev <- function(d){
    
    # total number of samples
    nsamples <- length(unique(d$sampleID))
    
    # count number of samples with count > 0
    df <- d %>%
      filter(read_count > 0) 
    prev <- (nrow(df)/nsamples)*100
    prev.df <- data.frame(ASV=unique(d$ASV), Prevalence=prev)
    return(prev.df)
  }
  
  prevalence <- reactive({
    work() %>%
      group_by(ASV) %>%
      do(pprev(.))
  })
  
  output$plot_prevalence <- renderPlotly({
    
    # tally frequency of prevalence values
    pdata <- as.data.frame(table(prevalence()$Prevalence))
    pdata$Var1 <- as.numeric(as.character(pdata$Var1))
 
    p <- ggplot(pdata, aes(x = Var1, y = Freq)) +
      geom_bar(stat = 'identity') +
      xlab('Prevalence of ASV across Samples (%)') +
      ylab('Number of ASVs') +
      scale_x_continuous(breaks = seq(0,100, 10), labels = seq(0, 100, 10),
                         limits = c(-2,102)) +
      theme_bw(12)
    ggplotly(p)
  })

  output$plot_spurious <- renderPlotly({
    
    pdata <- work() %>%
      group_by(sampleID) %>%
      mutate(tot_count = sum(read_count),
             rel_abund = read_count / tot_count * 100) %>%
      ungroup() %>%
      group_by(ASV) %>%
      mutate(avg_abund = mean(rel_abund)) %>%
      left_join(prevalence(), 'ASV') %>%
      distinct(avg_abund, Prevalence)
    
    p <- ggplot(pdata, aes(x = avg_abund, y = Prevalence)) +
      geom_point(size = 3, alpha = 0.6) +
      scale_y_continuous(limits=c(0,100)) +
      xlab('Mean Relative Abundance (%)') +
      ylab('Prevalence of ASV across Samples (%)') +
      theme_bw(12)
    ggplotly(p)
  })

  # read count distribution of taxa---------------------------------------------
  # customize count data based on selected taxonomic level

  output$n_sample <- renderText({length(unique(met()$sampleID))})
  output$ref_tax <- renderText({random_text(nwords = 1)})
  
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
    DT::datatable(out, options = list(scrollX = TRUE))
  })
  
  output$tax_distribution <- renderPlotly({
    
    p <- ggplot(tax_distrb_df(), 
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
    
    ggplotly(p) 
  })
  
  
  # evaluate number ASVs assigned to taxonomy level
  n_assigned <- reactive({
    
    df <- select(tax(), -sequence)
    out <- data.frame(tax_class = colnames(df),
                      n_NA = apply(df, 2, function(x) sum(is.na(unique(x)))),
                      n_ass = apply(df, 2, function(x) sum(!is.na(unique(x)))))
    out
  })
  
  
  output$perc_assigned <- renderPlotly({
    pdata <- n_assigned() %>%
      mutate(n_asv = n_NA + n_ass,
             perc_assigned = n_ass / n_asv * 100,
             tax_class = factor(tax_class, 
                                levels = c('Kingdom','Phylum','Class','Order',
                                           'Family','Genus','Species')))
    
    p <- ggplot(pdata, aes(x = tax_class, y = perc_assigned)) +
      geom_bar(stat = 'identity') +
      xlab('Taxonomic level') +
      ylab('Percent of ASVs classified') +
      theme_bw(12)
    
    ggplotly(p)
  })
  
  # read count distribution of samples------------------------------------------

  output$group_distribution <- renderPlotly({
    
    pdata <- work() %>%
      inner_join(met(), 'sampleID') %>%
      group_by(.data[[input$sample_select]]) %>%
      summarize(group_tot = sum(read_count)) 

    p <- ggplot(pdata, aes(x = .data[[input$sample_select]], y = group_tot)) +
      geom_bar(stat = 'identity') +
      xlab(input$sample_select) +
      ylab('Total read count within group') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p)
      
  })
  
  output$sample_distribution <- renderPlotly({
    
    pdata <- work() %>%
      inner_join(met() %>% mutate_all(as.factor), 'sampleID') %>%
      mutate(sampleID = as.factor(sampleID)) %>%
      group_by(sampleID) %>%
      mutate(selected_var = as.factor(.data[[input$sample_select]]),
             sample_tot = sum(read_count)) %>%
      ungroup() %>%
      group_by(.data[[input$sample_select]]) %>%
      mutate(group_tot = sum(read_count), avg = mean(sample_tot),
             x = as.numeric(selected_var), xavg1 = x - 0.5, xavg2 = x + 0.5) %>%
      select(-Taxon, -sequence, -(Kingdom:Species), -ASV, -read_count) %>%
      distinct()
    
    p <- ggplot(pdata, aes(x = x, y = sample_tot)) +
      geom_segment(aes(x = xavg1, xend = xavg2, y = avg, yend = avg)) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(breaks = seq(1, length(levels(pdata$selected_var))),
                         labels = levels(pdata$selected_var)) +
      xlab(input$sample_select) +
      ylab('Mean read count within group') +
      theme_bw(12) +
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p)
  })
}
    
## To be copied in the UI
# mod_qc_ui("qc_ui_1")
    
## To be copied in the server
# callModule(mod_qc_server, "qc_ui_1")