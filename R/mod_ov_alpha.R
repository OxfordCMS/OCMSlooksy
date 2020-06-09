# Module UI
  
#' @title   mod_ov_alpha_ui and mod_ov_alpha_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ov_alpha
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import htmlwidgets
#' @import shinyWidgets
#' @import readr
mod_ov_alpha_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    h1("\u03B1-Diversity"),
    tags$div("Alpha diversity assesses the diversity of sets of communities (or sets of samples). Species richness is the number of unique species. Species evenness is a measure of the consistency of species abundances (uneven data sets have community members that dominate in abundance). Entropy measures such as Shannon entropy and Simpson index are measures of uncertainty in the species identity of a sample [Jost 2006]. Diversity measures, such as Shannon's Diveristy and Inverse Simpson's Index, takes into account of the abundance of species in the community. In fact, when all species in a community are equally common, entropy and diveristy measures are equivalent. Entropy indeces can be converted to diversity by mathematical transformation."),
    column(
      width = 12,
      DT::dataTableOutput(ns('alpha_table'))  %>%
        shinycssloaders::withSpinner()
    ), br(),
    column(
      width = 12,
      DT::dataTableOutput(ns('alpha_test'))  %>%
        shinycssloaders::withSpinner()
    ), br(),
    column(
      width = 3, br(), br(),
      wellPanel(uiOutput(ns('alpha_grp_ui')))
    ),
    column(
      width = 9,
      dropdown(
        size = 'xs', icon = icon('save'), inline = TRUE, 
        style = 'material-circle',
        animate = animateOptions(
          enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
          exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
        
        downloadBttn(ns('dl_alpha_original'), 
                     list(icon('file-image'), "Original plot"),
                     size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_alpha_html'), 
                     list(icon('file-code'), "Interactive plot"),
                     size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_alpha_data'), 
                     list(icon('file-alt'), "Plot data"),
                     size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_alpha_rds'), 
                     list(icon('file-prescription'), "RDS"),
                     size = 'xs', style = 'minimal'), br(),
        downloadBttn(ns('dl_alpha_all'), 
                     list(icon('file-archive'), "All"),
                     size = 'xs', style = 'minimal')
      ),
      shinyjqui::jqui_resizable(
        plotlyOutput(ns('alpha_plot'), width = '100%', 
                     height= 'auto') %>% 
          shinycssloaders::withSpinner()
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ov_alpha
#' @export
#' @keywords internal
    
mod_ov_alpha_server <- function(input, output, session, param){
  ns <- session$ns
  
  # unpack data from parent module----------------------------------------------
  met <- reactive(param$met)
  asv <- reactive(param$asv)
  tax <- reactive(param$tax)
  asv_transform <- reactive(param$asv_transform)
  
  # render controls - alpha diversity-------------------------------------------
  output$alpha_grp_ui <- renderUI({
    radioButtons(ns('alpha_grp'), "Compare Sample Groups",
                 choices = colnames(met()), selected = 'sampleID')
  })
  
  # calculate alpha diversity---------------------------------------------------
  
  alpha_result <- reactive({
    req(input$alpha_grp)
    alpha_data <- asv() %>% select(-featureID)
    alpha_data <- as.data.frame(alpha_data)
    rownames(alpha_data) <- asv()$featureID
    
    shannon <- vegan::diversity(alpha_data,index = 'shannon',
                          base = 2, MARGIN = 2)
    shannon_d <- exp(shannon)
    richness <- vegan::specnumber(alpha_data, MARGIN = 2)
    evenness <- shannon / log(richness)
    invsimpson <- vegan::diversity(alpha_data,index = 'invsimpson',
                                   base = 2, MARGIN = 2)
    simpson <- vegan::diversity(alpha_data, index = 'simpson',
                                base = 2, MARGIN = 2)
    
    out <- data.frame(sampleID = names(shannon),
                      shannon = shannon,
                      simpson = simpson,
                      shannon_d = shannon_d,
                      richness = richness,
                      evenness = evenness,
                      invsimpson = invsimpson)
    out
  })
  
  output$check <- renderPrint({
    ymax = pdata_alpha() %>%
      group_by(alpha_metric) %>%
      summarise(ymax = max(alpha_value)) %>%
      mutate(ylimit = ymax * 1.1)
    ylimit <- ymax$ylimit
    names(ylimit) <- ymax$alpha_metric
    print(ylimit)
  })

  # determine valid stat test
  grp_tally <- reactive(table(met()[,input$alpha_grp]))
  stat_test <- reactive({
    if(length(grp_tally()) == 2) 'wilcox.test'
    else 'kruskal.test'
  })
  
  alpha_stat <- eventReactive(input$alpha_grp, {

    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )
    
    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      inner_join(met() %>% gather('meta_variable','grouping', -sampleID),
                 'sampleID') %>%
      filter(meta_variable == input$alpha_grp)
    
    out <- ggpubr::compare_means(formula = alpha_value~grouping, data = out,
                                 group.by = c('alpha_metric'),
                                 method = stat_test(), p.adjust.method = 'BH')
    out
  })
  
  # show tables
  output$alpha_table <- DT::renderDataTable({
    out <- met() %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID')
    DT::datatable(out, extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(alpha_result())[2:ncol(alpha_result())], digits = 3)
  })
  
  output$alpha_test <- DT::renderDataTable({
    validate(
      need(max(grp_tally()) != 1, "Only one observation per group. Group-wise comparisons not performed"),
      need(length(grp_tally()) > 1, "All observations are in the same group. Group-wise comparisons not performed")
    )
    
    DT::datatable(alpha_stat() %>%
                    select(-.y., -p.format, ), extensions = 'Buttons', 
                  options = list(scrollX = TRUE, dom = 'Blfrtip', 
                                 buttons = c('copy','csv'))) %>%
      DT::formatRound(column = 'p', digits = 3)
  })
  
  # plot alpha diversity
  pdata_alpha <- eventReactive(input$alpha_grp, {

    # set xorder based on shannon_d
    xorder <- met() %>%
      mutate_all(as.character) %>%
      arrange(sampleID) %>%
      inner_join(alpha_result(), 'sampleID') %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(shannon_d)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], 
                                      desc(alpha_avg)))
    
    out <- alpha_result() %>%
      gather('alpha_metric', 'alpha_value', -sampleID) %>%
      inner_join(met() %>% mutate_all(as.character), 'sampleID') %>%
      group_by(alpha_metric, .data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value),
             x = factor(.data[[input$alpha_grp]], 
                        levels = levels(xorder$x))) %>%
      distinct(x, alpha_metric, alpha_value, alpha_avg) %>%
      ungroup()
      out
  })
  p_alpha <- reactive({
    req(input$alpha_grp)
    
    p <- ggplot(pdata_alpha(), aes(x = x, y = alpha_value, group = x)) +
      facet_wrap(~alpha_metric, scales = 'free')
   
    if(max(grp_tally()) > 1) {
      # geom_GeomSignif not compatible with plotly
      # compare_pairs <- combn(names(grp_tally()), 2, simplify = FALSE)
      
      ymax = pdata_alpha() %>%
        group_by(alpha_metric) %>%
        mutate(ymax = max(alpha_value), yupper = ymax * 1.02,
                  ymin = min(alpha_value), ylower = ymin * 0.98)
      
      p <- p +
        ggpubr::stat_compare_means(method = stat_test(), label = 'p.format',
                                   size = 3) +
        geom_blank(data = ymax, aes(y = yupper)) +
        geom_blank(data = ymax, aes(y = ylower))
    }
    
    if(min(grp_tally()) > 5) {
      p <- p +
        geom_point(position = position_jitter(width = 0.25, seed = 1), 
                   alpha = 0.8)
    }
    else {
      p <- p +
        geom_point(alpha = 0.8)
    }
    
    p <- p +
      theme_bw() +
      xlab(input$alpha_grp) +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank())
    
    p
  })
  
  output$alpha_plot <- renderPlotly({
    ggplotly(p_alpha())
  })
  
  output$dl_alpha_original <- downloadHandler(
    fname <- function() {"ov_alpha.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_alpha())}
  )
  
  output$dl_alpha_html <- downloadHandler(
    fname <- function() {"ov_alpha.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_alpha())), file)
    }
  )
  
  output$dl_alpha_data <- downloadHandler(
    fname <- function() {"ov_alpha.csv"}, 
    content <- function(file) {
      readr::write_csv(pdata_alpha(), file)
    }
  )
  
  output$dl_alpha_rds <- downloadHandler(
    fname <- function() {"ov_alpha.rds"},
    content <- function(file) {
      saveRDS(p_alpha(), file)
    }
  )
  
  output$dl_alpha_all <- downloadHandler(
    fname <- function() {"ov_alpha.zip"},
    content <- function(file) {
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      to_zip <- c("ov_alpha.tiff", "ov_alpha.html","ov_alpha.csv", "ov_alpha.rds")
      ggsave(to_zip[1], plot=p_alpha())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_alpha())), to_zip[2])
      write.csv(pdata_alpha(), to_zip[3])
      saveRDS(p_alpha(), to_zip[4])
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
}
    
## To be copied in the UI
# mod_ov_alpha_ui("ov_alpha_ui_1")
    
## To be copied in the server
# callModule(mod_ov_alpha_server, "ov_alpha_ui_1")
 
