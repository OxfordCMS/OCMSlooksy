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
#' import readr
mod_ov_alpha_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("\u03B1-Diversity"),
    tags$div("Alpha diversity assesses the diversity of sets of communities (or sets of samples). Species richness is the number of unique species. Species evenness is a measure of the consistency of species abundances (uneven data sets have community members that dominate in abundance). Entropy measures such as Shannon entropy and Simpson index are measures of uncertainty in the species identity of a sample [Jost 2006]. Diversity measures, such as Shannon's Diveristy and Inverse Simpson's Index, takes into account of the abundance of species in the community. In fact, when all species in a community are equally common, entropy and diveristy measures are equivalent. Entropy indeces can be converted to diversity by mathematical transformation."),
    column(width = 12,
           DT::dataTableOutput(ns('alpha_table'))),
    hidden(div(id = ns('alpha_body_div'),
               column(width = 3, br(), br(),
                      wellPanel(
                        uiOutput(ns('alpha_grp_ui')))),
               column(width = 9,
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
                        plotlyOutput(ns('alpha_plot'), width = '100%')
                      )),
               column(width = 12,
                      DT::dataTableOutput(ns('alpha_test')))
    ))
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
  
  # unpack alpha inputs
  alpha_method <- reactive(param$alpha_input$alpha_method)
  alpha_calculate <- reactive(param$alpha_input$alpha_calculate)
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(alpha_calculate(), {
    show('alpha_body_div')
  })
  # render controls - alpha diversity-------------------------------------------
  output$alpha_grp_ui <- renderUI({
    radioButtons(ns('alpha_grp'), "Compare Sample Groups",
                 choices = colnames(met()), selected = 'sampleID')
  })
  
  # calculate alpha diversity---------------------------------------------------
  
  div_result <- eventReactive(alpha_calculate(), {
    
    alpha_data <- asv() %>% select(-featureID)
    alpha_data <- as.data.frame(alpha_data)
    rownames(alpha_data) <- asv()$featureID
    
    if(alpha_method() == 'shannon_d') {
      H <- vegan::diversity(alpha_data,index = 'shannon',
                            base = 2, MARGIN = 2)
      exp(H)
    }
    else if(alpha_method() == 'richness') {
      vegan::specnumber(alpha_data, MARGIN = 2)
    }
    else if(alpha_method() == 'evenness') {
      rich_result <- vegan::specnumber(alpha_data, MARGIN = 2)
      H <- vegan::diversity(alpha_data, index = 'shannon', base = 2, MARGIN = 2)
      
      H / log(rich_result)
    }
    else {
      vegan::diversity(alpha_data,index = alpha_method(),
                       base = 2, MARGIN = 2)
    }
  })
  
  # perform statistical tests on alpha values ********PICK UP HERE**********
  alpha_stat_validate <- function(input) {
    if(any(input == 'anova')) {
      
    }
  }
  alpha_stat <- eventReactive(alpha_calculate(), {
    
    # validate selected tests
    
  })
  
  
  # plot alpha diversity
  pdata_alpha <- eventReactive(alpha_calculate(), {
    met() %>%
      arrange(sampleID) %>%
      mutate_all(as.character) %>%
      mutate(alpha_value = div_result()[sort(names(div_result()))])
  })
  
  output$alpha_table <- DT::renderDataTable({
    DT::datatable(pdata_alpha() %>% rename(!!alpha_method() := alpha_value),
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  p_alpha <- reactive({
    req(input$alpha_grp)
    xorder <- pdata_alpha() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value)) %>%
      distinct(.data[[input$alpha_grp]], alpha_avg) %>%
      ungroup() %>%
      mutate(x = forcats::fct_reorder(.data[[input$alpha_grp]], desc(alpha_avg)))
    
    pdata_ordered <- pdata_alpha() %>%
      group_by(.data[[input$alpha_grp]]) %>%
      mutate(alpha_avg = mean(alpha_value),
             x = factor(.data[[input$alpha_grp]], levels = levels(xorder$x))) %>%
      distinct(x, sampleID, alpha_value) %>%
      ungroup()
    
    p <- ggplot(pdata_ordered, aes(x = x, y = alpha_value))
    
    n_grp <- table(met()[,input$alpha_grp])
    
    if(min(n_grp) > 5) {
      p <- p +
        geom_point(aes(group = .data[[input$alpha_grp]]), alpha = 0.8) +
        geom_violin(aes(group = .data[[input$alpha_grp]]), fill = NA)
    }
    else {
      p <- p +
        geom_point(alpha = 0.8)
    }
    
    ytitle <- c("Shannon-Weaver Index (H)"="shannon",
                "Simpson Index (D1)" = "simpson",
                "Shannon (H'), q = 1" = "shannon_d",
                "Inverse Simpson (D2), q = 2" = "invsimpson",
                "Species Richness (S), q = 0" = "richness",
                "Species Evenness (J)" = "evenness")
    p <- p +
      theme_bw() +
      xlab(input$alpha_grp) +
      ylab(names(ytitle[ytitle == alpha_method()])) +
      theme(axis.text.x = element_text(angle = 90))
    
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
 
