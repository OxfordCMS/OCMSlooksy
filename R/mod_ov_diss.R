#' ov_diss UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ov_diss_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('Sub check'), br(), verbatimTextOutput(ns('check'))),
    h1('Sample Dissimilarity'),
      tags$div("Using Bray-Curtis distance metric to measure the sample-wise dissimilarity. Dissimilarity is calculated for pair-wise samples within the groups of the selected metadata variable (phenotype). "),
    fluidRow(
      width = 12,
      column(
        width = 12,
        tags$div(tags$b(textOutput(ns('validation_msg')))),
        br(),
        htmlOutput(ns('diss_message_ui'))
      )
    ),
    fluidRow(
      column(
        width = 1, style = 'padding:0px;',
        mod_download_ui(ns('download_diss')),
      ),
      column(
        width = 11, style = 'padding:0px;',
        shinyjqui::jqui_resizable(
          plotlyOutput(ns('diss_plot'), width = '100%',
                       height= 'auto') %>%
            shinycssloaders::withSpinner()
        )
      )
    ),
    fluidRow(
      width = 12,
      box(
        width = 12,
        DT::dataTableOutput(ns("diss_stat"))
      )
    ), 
    fluidRow(
      width = 12,
      column (
        width = 12,
        DT::dataTableOutput(ns('diss_result'))  %>%
          shinycssloaders::withSpinner()
      )
    )
  )
}
    
#' ov_diss Server Function
#'
#' @noRd 
mod_ov_diss_server <- function(input, output, session, bridge){
  ns <- session$ns
 
  # bridge$asv_transform # transformed count
  # bridge$filtered$met # metadata table
  # bridge$filtered$tax # taxonomy table
  
  # prepare data----------------------------------------------------------------
  # add panel column to met
  met_diss <- eventReactive(bridge$diss_input$diss_calculate, {
    if(bridge$diss_input$diss_panel != 'none') {
      bridge$filtered$met %>% mutate(panel = .data[[bridge$diss_input$diss_panel]]) 
    } else {
      bridge$filtered$met %>% mutate(panel = 'none')
    }
    
  })
  

  # perform checks--------------------------------------------------------------
  # check for number of samples per group
  grp_tally <- eventReactive(bridge$diss_input$diss_calculate, {
    table(met_diss()[,c('panel', bridge$diss_input$diss_grp)], useNA='ifany')
  })
  
  # send by group for distance calculation
  iter <- eventReactive(bridge$diss_input$diss_calculate, {
    list(panel = rownames(grp_tally()), grouping = colnames(grp_tally()))
  })
  
  # check panels and groups
  valid_diss <- eventReactive(bridge$diss_input$diss_calculate, {
    apply(grp_tally(), 1:2, function(x) {x >= 2})
  })
  
  # check number of groups to determine how dissimilarity is calculated
  ngroup <- eventReactive(bridge$diss_input$diss_calculate, {
    if((max(grp_tally()) == 1) | (length(grp_tally()) == 1) |
       (ncol(grp_tally()) == 1)) {
      "onegroup"
    } else {
      "bygroup"
    }
  })
  
  
  validation_msg <- eventReactive(bridge$diss_input$diss_calculate, {
    switch(ngroup(), "onegroup" = "All observations are in the same group. Measuring sample dissimilarity on all sample pairs within the group.",
           "bygroup" = 'Measuring pairwise sample dissimilarity on sample pairs within each group')
  })
  output$validation_msg <- renderText({
    validation_msg()
  })
  
  # initiate message about statistic calculation
  diss_check <- reactiveValues()
  
  # pairwise dissimilarity------------------------------------------------------
  diss_result <- eventReactive(bridge$diss_input$diss_calculate, {
    
    diss_check$empty_panel <- c()
    diss_check$empty_group <- c()

    out <- c()
    
    for(i in 1:length(iter()$panel)) {
      
      # get sampleID in current panel group
      panel_sample <-  met_diss() %>%
        filter(panel %in% c(iter()$panel[i])) # making this %in% statement even though only one value to help search for NAs. filtering for NA with %in% vector keeps NAs
      # get count data in current panel group
      panel_data <- bridge$asv_transform[,panel_sample$sampleID]

      validate(
        need(nrow(panel_sample) >= 2, "Cannot assess sample pairwise dissimilarity within the group. Must have at least 2 samples per panel"),
        need(ifelse(bridge$diss_input$diss_panel != 'none', 
                    sum(valid_diss()[i,]) > 0, 
                    TRUE),
             "Cannot assess sample pairwise dissimilarity within the group. Must have at least 2 samples per panel")
      )
      
      # pairwise dissimilarity of all samples-----------------------------------
      if(ngroup() == 'onegroup') {
        
        # get sampleID in current group
        curr_sample <-  panel_sample %>%
          filter(.data[[bridge$diss_input$diss_grp]] %in% c(iter()$grouping))
        
        # get count data in current group
        curr_data <- panel_data[,curr_sample$sampleID]
        
        # handle empty subgroup
        if(nrow(curr_sample > 0)) {
        
          # calculate dissimilarity distances
          curr_dist <- vegan::vegdist(t(curr_data), method='bray')
          curr_dist <- as.matrix(curr_dist)
          sample_pair <- t(combn(colnames(curr_dist), 2))
          
          # put distance matrix in long dataframe
          entry <- data.frame(sample_pair, bray=curr_dist[sample_pair])
          colnames(entry)[1:2] <- c('row','col')
          entry$panel <- iter()$panel[i]
          entry$grouping <- bridge$diss_input$diss_grp
          entry$pairID <- paste(entry$row, entry$col, entry$panel, entry$grouping,
                                sep="_")
          
          out <- rbind(out, entry)
        } else {
         diss_check$empty_panel <- c(diss_check$empty_panel, iter()$panel[i])
        }
      # pairwise sample dissimilarity within each group-------------------------
      } else {
        inner_out <- c()
        for(j in 1:length(iter()$grouping)) {

          # handle empty subgroup
          if(valid_diss()[i, j]) {
            # get sampleID in current group
            curr_sample <-  panel_sample %>%
              filter(.data[[bridge$diss_input$diss_grp]] %in% 
                       c(iter()$grouping[j])) # making this %in% statement even though only one value to help search for NAs. filtering for NA with %in% vector keeps NAs
            
            # get count data in current group
            curr_data <- panel_data[,curr_sample$sampleID]

            # calculate pairwise dissimilarity
            ## samples as rows
            curr_dist <- vegan::vegdist(t(curr_data), method = 'bray')
            curr_dist <- as.matrix(curr_dist)
            sample_pair <- t(combn(colnames(curr_dist), 2))
            entry <- data.frame(sample_pair, bray=curr_dist[sample_pair])
            colnames(entry)[1:2] <- c('row','col')
            entry$panel <- iter()$panel[i]
            entry$grouping <- iter()$grouping[j]
            entry$pairID <- paste(entry$row, entry$col, entry$panel, entry$grouping,
                                  sep='_')
            
            inner_out <- rbind(inner_out, entry)
          } else {
            diss_check$empty_group <- rbind(diss_check$empty_group, 
                                          c(panel=iter()$panel[i],
                                            grouping=iter()$grouping[j]))
          }

        } # end inner loop
        out <- rbind(out, inner_out)
      } # end outer if / else
    } # end outer loop
    out
  })
  
  output$diss_result <- DT::renderDataTable({
    DT::datatable(diss_result(),
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
      DT::formatRound(column = 'bray', digits = 3)
  })

  
  diss_msg <- eventReactive(bridge$diss_input$diss_calculate, {
    out <- c()
    if(!is.null(diss_check$empty_panel)) {
      entry <- sprintf("%s panel does not contain enough samples to measure pairwise sample dissimilarity", diss_check$empty_panel)
      out <- c(out, entry)
    }
    if(!is.null(diss_check$empty_group)) {
      for(i in 1:nrow(diss_check$empty_group)) {
        entry <- sprintf("%s panel, %s group does not contain enough samples to measure pairwise sample dissimilarity", 
                         diss_check$empty_group[i,'panel'], 
                         diss_check$empty_group[i,'grouping'])
        out <- c(out, entry)
      }
    }
    
    if(!is.null(out)) {
      HTML(paste(out, collapse = '<br/>'))
    } else {
      " "
    }
  })
  
  output$diss_message_ui <- renderUI({
    diss_msg()
  })
  
  valid_stat <- eventReactive(bridge$diss_input$diss_calculate, {
    # tally number of dissimilarity measurements per panel-grouping
    curr <- table(diss_result()[,c('panel','grouping')])

    # need at least 2 dissimilarities per panel-grouping
    diss_obs <- apply(curr, 1:2, function(x) {x >= 2})

    # need to have two groups have have enough dissimilarity observations
    grp_obs <- apply(diss_obs, 1, function(x) sum(x) >=2)

    tibble::enframe(grp_obs, name='panel', value='contains.group')
  })
  
  # number of panels that are not valid for statistics calculations
  n_failed <- eventReactive(bridge$diss_input$diss_calculate, {

    # pass = TRUE, fail = FALSE
    nrow(valid_stat()) - sum(valid_stat()$contains.group)
  })
  
  # panels that work for statistics calculations
  keep_panel <- eventReactive(bridge$diss_input$diss_calculate, {
    valid_stat() %>%
      filter(contains.group == TRUE) %>%
      pull(panel)
  })
  
  # check number of groups to determine if should perform statistics 
  validation_stat <- eventReactive(bridge$diss_input$diss_calculate, {

    # perform statistic calculation unless
    out <- TRUE
    if((length(keep_panel()) > 0) & 
       (length(keep_panel()) < nrow(grp_tally()))) {
      out <- 'some'
    }
    if((n_failed() == length(iter()[['panel']])) | # all failed; only one panel
       (ngroup() == 'onegroup')) {
      out <- FALSE
    } 
    out
  })
  
  # perform statistical test
  stat_test <- reactive({
    if(length(grp_tally()) == 2) 'wilcox.test'
    else 'kruskal.test'
  })

  # statistical test on dissimilarity distances
  diss_stat <- eventReactive(bridge$diss_input$diss_calculate, {
 
    if(validation_stat() == 'some') {
      # only perform stat test on panels with groups
      in_data <- diss_result() %>% 
        filter(panel %in% keep_panel())
    } else {
      in_data <- diss_result()
    }
    
    validate(
      need(validation_stat() != FALSE, 
           "All observations are in the same group or not enough samples per group. Group-wise comparisons not performed."),
      need(nrow(in_data) > 0, "All Panels contain one group. Group-wise comparisons not performed.")
    )

    out <- ggpubr::compare_means(formula = bray~grouping,
                                 data = in_data,
                                 group.by = 'panel',
                                 method = stat_test(), p.adjust.method = 'BH')
    out
  })


  output$diss_stat <- DT::renderDataTable({

    DT::datatable(diss_stat() %>%
                    select(-.y., -p.format, ), extensions = 'Buttons',
                  options = list(scrollX = TRUE, dom = 'Blfrtip',
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = 'p', digits = 3)
  })


  output$check <- renderPrint({

  })

  # plot dissimilarity
  pdata_diss <- eventReactive(bridge$diss_input$diss_calculate, {

    out <- diss_result() %>% 
      group_by(panel, grouping) %>%
      mutate(diss_avg = mean(bray)) %>%
      ungroup()

    if(validation_stat() != FALSE) {
      # re-calculate comparison statistic
      if(validation_stat() == 'some') {
        # only perform stat test on panels with groups
        in_data <- diss_result() %>% filter(panel %in% keep_panel())
      } else {
        in_data <- diss_result()
      }
      
      compare_stat <- ggpubr::compare_means(formula = bray~grouping,
                                            data = in_data,
                                            group.by = 'panel',
                                            method = stat_test(),
                                            p.adjust.method = 'BH')

      out <- out %>%
        left_join(compare_stat %>% select(panel, p.adj), 'panel') %>%
        mutate(panel_text = sprintf("%s\np.adj=%0.3f", panel, p.adj))
    } else {
      out <- mutate(out, panel_text = panel)
    }
    out
  })
  
  p_diss <- eventReactive(bridge$diss_input$diss_calculate, {

    p <- ggplot(pdata_diss(), aes(x = grouping, y = bray)) +
      geom_boxplot(outlier.fill=NA) +
      geom_point(aes(text=paste0("sample pair: ", pairID)),
                 position = position_jitter(width = 0.25, seed = 1),
                 alpha = 0.6) +
      theme_bw() +
      xlab(bridge$diss_input$diss_grp) +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank())
    
    if(bridge$diss_input$diss_panel != 'none') {
      p <- p +
        facet_wrap(~panel_text, scales='free')
    }

    p
  })

  output$diss_plot <- renderPlotly({
    ggplotly(p_diss())
  })

  # download data
  for_download <- reactiveValues()
  observe({
    req(bridge$input_diss$calculate)
    for_download$figure <- p_diss()
    for_download$fig_data <- pdata_diss()
  })

  callModule(mod_download_server, "download_diss", bridge = for_download, 'diss')

  # initiate return list
  cross_module <- reactiveValues()
  observe({
    cross_module$diss <- list(
      validation_msg = validation_msg(),
      diss_result = diss_result(),
      p_diss = p_diss(),
      diss_msg = diss_msg()
    )
  })
  
  observe({
    if(validation_stat() != FALSE) {
      cross_module$diss$diss_stat <- diss_stat()
    } else {
      cross_module$diss$diss_stat <- "All observations are in the same group or not enough samples per group. Group-wise comparisons not performed."
    }
  })
  return(cross_module)
}
    
## To be copied in the UI
# mod_ov_diss_ui("ov_diss_ui_1")
    
## To be copied in the server
# callModule(mod_ov_diss_server, "ov_diss_ui_1")
 
