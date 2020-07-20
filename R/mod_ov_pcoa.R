# Module UI
  
#' @title   mod_ov_pcoa_ui and mod_ov_pcoa_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ov_pcoa
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import cluster
#' @import clusterSim
#' @import htmlwidgets
#' @import shinyWidgets
mod_ov_pcoa_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(width = 12, h3('check'), br(), verbatimTextOutput(ns('check'))),
    h1("Principal Coordinate Analysis"),
    tags$div("PCoA is a supervised multivariate analysis (a priori knowledge of clusters) that can be used for assessing statistical significance of cluster patterns under a multivariate model.", br()),
    hidden(div(
      id = ns('pcoa_body_div'),
      h2("Distance Matrix"),
      DT::dataTableOutput(ns('dist_table'))  %>%
        shinycssloaders::withSpinner(),
      h2("PCoA Summary"),
      DT::dataTableOutput(ns('pcoa_summary'))  %>%
        shinycssloaders::withSpinner(),
      h2('PCoA plot'),
      wellPanel(
        tags$div(style = 'text_align: center', h3("Plot Parameters")),
        fluidRow(
          # Plot controls
          column(
            width = 3,
            div(style = "display: inline-block;vertical-align: top",
                uiOutput(ns('xPCo_ui'))),
            div(style = "display: inline-block;vertical-align: top",
                uiOutput(ns('yPCo_ui'))),
            checkboxInput(ns('pcoa_ellipse'), "Show clusters", 
                          value = TRUE)),
          column(
            width = 3,
            # score point aesthetics
            h4("Score points aesthetics"),
            uiOutput(ns('pcoa_pt_colour_ui')),
            hidden(div(id = ns('pcoa_nclust_div'), 
                       uiOutput(ns('pcoa_nclust_ui')))),
            uiOutput(ns('pcoa_pt_shape_ui')),
            sliderInput(ns('pcoa_pt_size'), 'Point size:',
                        min = 0.1, max = 5, value = 3, step = 0.5,
                        ticks = FALSE),
            sliderInput(ns('pcoa_pt_alpha'), 'Point transparency:',
            min = 0.1, max = 1, value = 1, step = 0.1)),
         
          # score label aesthetics
          column(
            width = 3,
            h4("Score labels aesthetics"),
            uiOutput(ns('pcoa_label_ui')),
            uiOutput(ns('pcoa_lab_colour_ui')),
            sliderInput(ns('pcoa_lab_size'), 'Label size:',
                        min = 0.1, max = 5, value = 3, step = 0.5),
            sliderInput(ns('pcoa_lab_alpha'), 'Label transparency:',
                        min = 0.1, max = 1, value = 1, step = 0.1)),
          
          # cluster aethetics
          hidden(div(
            id = ns('pcoa_ell_div'),
            column(
              width = 3,
              h4("Cluster aesthetics"),
              checkboxInput(ns('pcoa_ell_colour'),"Colour cluster ellipses",
                            value = TRUE),
              selectInput(ns('pcoa_ell_type'), "Type of ellipse",
                          choices = c('t-distribution' = 't',
                                      'normal distribution' = 'norm',
                                      'Euclidean distance' = 'euclid'),
                          selected = 'norm'),
              radioButtons(ns('pcoa_ell_line'), "Linetype",
                           choices = c('solid','dashed','longdash',
                                       'dotdash'),
                           selected = 'solid'),
              numericInput(ns('pcoa_ell_ci'), "Confidence Interval",
                           min = 0.1, max = 0.99, value = 0.95, 
                           step = 0.05))
            )),
          # column(width = 6, plotlyOutput(ns('CH_plot'))),
          # column(width = 6, verbatimTextOutput(ns('CH_index'))),
          column(
            width = 12, 
            dropdown(
              size = 'xs', icon = icon('save'), inline = TRUE, 
              style = 'material-circle',
              animate = animateOptions(
                enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                exit = shinyWidgets::animations$fading_exits$fadeOutLeft),
              
              downloadBttn(ns('dl_pcoa_original'), 
                           list(icon('file-image'), "Original plot"),
                           size = 'xs', style = 'minimal'), br(),
              downloadBttn(ns('dl_pcoa_html'), 
                           list(icon('file-code'), "Interactive plot"),
                           size = 'xs', style = 'minimal'), br(),
              downloadBttn(ns('dl_pcoa_data'), 
                          list(icon('file-alt'), "Plot data"),
                          size = 'xs', style = 'minimal'), br(),
              downloadBttn(ns('dl_pcoa_rds'), 
                          list(icon('file-prescription'), "RDS"),
                          size = 'xs', style = 'minimal'), br(),
              downloadBttn(ns('dl_pcoa_all'), 
                          list(icon('file-archive'), "All"),
                          size = 'xs', style = 'minimal')),
            shinyjqui::jqui_resizable(
              plotlyOutput(ns('pcoa_plot'), width = '100%') %>% 
                shinycssloaders::withSpinner()
            ))
          ))
    ))
  )
}
    
# Module Server
    
#' @rdname mod_ov_pcoa
#' @export
#' @keywords internal
    
mod_ov_pcoa_server <- function(input, output, session, param){
  ns <- session$ns
  
  # unpack data from parent module----------------------------------------------
  # unpack pca inputs
  pcoa_dist <- reactive(param$pcoa_input$pcoa_dist)
  pcoa_calculate <- reactive(param$pcoa_input$pcoa_calculate)
  
  # toggle div for input controls-----------------------------------------------
  observeEvent(pcoa_calculate(), {
    show('pcoa_body_div')
  })
  
  observeEvent(input$pcoa_pt_colour, {
    toggle('pcoa_nclust_div', condition = input$pcoa_pt_colour == 'k-means')
  })
  observeEvent(input$pcoa_ellipse, {
    toggle('pcoa_ell_div')
  })
  
  ## render controls - PCoA-----------------------------------------------------
  output$pcoa_nclust_ui <- renderUI({
    numericInput(ns('pcoa_nclust'), "Number of clusters, k", 
                 value = 2, min = 2, max = nrow(param$work_db$met)-1, step = 1)
  })
  output$xPCo_ui <- renderUI({
    numericInput(ns('xPCo'), "Principal Coordinate, x-axis", min = 1, max = length(param$work_db$met$sampleID), step = 1,
                 value = 1)
  })
  output$yPCo_ui <- renderUI({
    numericInput(ns('yPCo'), "Principal Coordinate, y-axis", min = 1, max = length(param$work_db$met$sampleID), step = 1,
                 value = 2)
  })
  
  ### pcoa point aesthetics
  output$pcoa_pt_colour_ui <- renderUI({
    selectInput(ns('pcoa_pt_colour'), 'Point colour:', 
                choices = c('none', 'k-means', colnames(param$work_db$met)), selected = 'none')
  })
  output$pcoa_pt_shape_ui <- renderUI({
    selectInput(ns('pcoa_pt_shape'), 'Point shape:', 
                choices = c('none', colnames(param$work_db$met)), selected = 'none')
  })
  
  ### pcoa label aethetics
  output$pcoa_label_ui <- renderUI({
    selectInput(ns('pcoa_label'), 'Label by:', 
                choices = c('none', colnames(param$work_db$met)), selected = 'none')
  })
  output$pcoa_lab_colour_ui <- renderUI({
    selectInput(ns('pcoa_lab_colour'), 'Label colour:', 
                choices = c('none', 'k-means', colnames(param$work_db$met)), 
                selected = 'none')
  })
  
  # ### pca loaing points aesthetics
  # output$pcoa_pt_colour_ui <- renderUI({
  #   selectInput(ns('pcoa_pt_colour'), 'Point colour:', 
  #               choices = c('none', colnames(param$work_db$tax)), selected = 'none')
  # })
  # output$pcoa_pt_shape_ui <- renderUI({
  #   selectInput(ns('pcoa_pt_shape'), 'Point shape:', 
  #               choices = c('none', colnames(param$work_db$tax)), selected = 'none')
  # })
  
  # calculate pcoa--------------------------------------------------------------
  # sample clustering
  
  ## samples as rows
  dist_data <- eventReactive(pcoa_calculate(), {
    req(pcoa_dist())
    vegan::vegdist(t(param$work_db$asv_transform), method = pcoa_dist())
  })
  
  output$dist_table <- DT::renderDataTable({
    DT::datatable(as.data.frame(as.matrix(dist_data())), 
                  extensions = 'Buttons', 
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv')))
  })
  
  # identify clusters based on k-means
  cluster_result <- reactive({
    req(input$pcoa_pt_colour)
    if(input$pcoa_pt_colour == 'k-means'){
      req(input$pcoa_nclust)
      out <- data.frame(sampleID = rownames(as.matrix(dist_data())),
                        pam_cluster = as.vector(cluster::pam(dist_data(), 
                                                             input$pcoa_nclust)$cluster))    
    }
    else {
      out <- data.frame(sampleID = rownames(as.matrix(dist_data())),
                        pam_cluster = 1)
    }
    out
  })
  
  # ## determine the optimal number of clusters for the dataset using the mediod
  # ## as a midpoint
  # pcoa_optk <- reactive({
  #   out <- 0
  #   
  #   # cluster of 1 returns NaN
  #   for (k in 2:(nrow(param$work_db$met)-1)) {
  #     # find mediod clusters and return a vector of clusters
  #     
  #     # calculate Calisnki-Harabasz index to determine the fit to the cluster
  #     out[k] <- clusterSim::index.G1(t(param$work_db$asv_transform), cluster_result()$pam_cluster, 
  #                                    d = dist_data(), centrotypes = "medoids")
  #   }
  #   
  #   out
  # })
  
  # # plot CH index
  # output$CH_plot <- renderPlotly({
  #   
  #   pdata <- data.frame(x=1:length(pcoa_optk()), y=0, yend=pcoa_optk())
  #   pdata$xend <- pdata$x
  #   
  #   k <- nrow(param$work_db$met)-1
  #   # plot number of clusters and respective CH index
  #   p <- ggplot(pdata) +
  #     geom_segment(ggplot2::aes(x=x, y=y, xend=xend, yend=yend)) +
  #     scale_x_continuous(breaks=2:k, limits=c(2,k)) +
  #     xlab('k clusters') +
  #     ylab('Calinski-Harabasz Index') +
  #     theme_bw() +
  #     theme(panel.grid.minor = element_blank(),
  #           panel.grid.major.x=element_blank())
  #   
  #   ggplotly(p)
  # })
  # 
  # calculate principal coordinates
  pcoa_data <- eventReactive(pcoa_calculate(), {
    ape::pcoa(dist_data(), correction = 'cailliez')
  })
  
  # extract correction note
  pcoa_note <- eventReactive(pcoa_calculate(), {
    pcoa_data()$note
  })

  # summary of pcoa
  pcoa_summary <- reactive({
    
    if(pcoa_note() == 'There were no negative eigenvalues. No correction was applied') {
      col_keep <- c('Eigenvalues', 'Relative_eig','Cumul_eig')  
      col_name <- c('Eigenvalues','Variance Explained', 'Cumulative Variance Explained')
    }
    else {
      col_keep <- c('Corr_eig', 'Rel_corr_eig', 'Cum_corr_eig')
      col_name <- c('Corrected Eigenvalues','Corrected Variance Explained', 'Corrected Cumulative Variance Explained')
    }
    
    out <- as.matrix(pcoa_data()$values)
    out <- out[, col_keep]
    rownames(out) <- paste0('PC', 1:nrow(out))
    colnames(out) <- col_name
    t(out)
  })
  
  output$pcoa_summary <- DT::renderDataTable({
    DT::datatable(pcoa_summary(), 
                  extensions = 'Buttons',
                  options = list(scrollX = TRUE, 
                                 dom = 'Blfrtip', buttons = c('copy','csv'))) %>%
      DT::formatRound(column = colnames(pcoa_summary()), digits = 3)
    
  })
  
  # setting pcoa plot parameters
  pcoa_pt_colour <- reactive({
    if(input$pcoa_pt_colour == 'none')  'black'
    else if(input$pcoa_pt_colour == 'k-means') 'pam_cluster'
    else input$pcoa_pt_colour
  })
  
  pcoa_pt_shape <- reactive({
    if(input$pcoa_pt_shape == 'none') 1
    else pcoa_pt_shape <- input$pcoa_pt_shape
  })
  
  pcoa_pt_size <- reactive(input$pcoa_pt_size)
  pcoa_pt_alpha <- reactive(input$pcoa_pt_alpha)
  
  pcoa_label <- reactive({
    if(input$pcoa_label == 'none') FALSE
    else TRUE
  })
  
  pcoa_label_by <- reactive({
    if(input$pcoa_label == 'none') NULL
    else input$pcoa_label
  })
  
  pcoa_lab_colour <- reactive({
    if(input$pcoa_lab_colour == 'none') NULL
    else if(input$pcoa_lab_colour == 'k-means') 'pam_cluster'
    else input$pcoa_lab_colour
  })
  
  pcoa_lab_size <- reactive(input$pcoa_lab_size)
  pcoa_lab_alpha <- reactive(input$pcoa_lab_alpha)
  
  pcoa_ell_colour <- reactive({
    if(input$pcoa_ell_colour) pcoa_pt_colour()
    else 'black'
  })

  
  # plot pcoa plot
  pdata_pcoa <- reactive({
    pdata <- data.frame(pcoa_data()$vectors)
    pdata$sampleID <- rownames(pcoa_data()$vectors)
    pdata <- pdata %>%
      inner_join(cluster_result() %>%
                   mutate(pam_cluster = as.character(pam_cluster)), 
                 'sampleID') %>%
      inner_join(param$work_db$met, 'sampleID')
    pdata
  })
  
  p_pcoa <- reactive({
    xPCo <- paste('Axis', input$xPCo, sep = ".")
    yPCo <- paste('Axis', input$yPCo, sep = ".")
    
    p <- ggplot(pdata_pcoa(), aes_string(x = xPCo, y = yPCo))
    
    p <- p +
      ggfortify:::geom_factory(ggplot2::geom_point, pdata_pcoa(), 
                               colour = pcoa_pt_colour(), size = pcoa_pt_size(), 
                               alpha = pcoa_pt_alpha(), shape = pcoa_pt_shape())
    
    if(input$pcoa_ellipse) {
      p <- p +
        ggfortify:::geom_factory(ggplot2::stat_ellipse, pdata_pcoa(),
                                 group = pcoa_pt_colour(),
                                 colour = pcoa_ell_colour(),
                                 linetype = input$pcoa_ell_line,
                                 type = input$pcoa_ell_type,
                                 level = input$pcoa_ell_ci)
    }
    
    p <- ggfortify:::plot_label(p = p, data = pdata_pcoa(), 
                                label = pcoa_label(), 
                                label.label = pcoa_label_by(), 
                                label.colour = pcoa_lab_colour(), 
                                label.alpha = pcoa_lab_alpha(), 
                                label.size = pcoa_lab_size())
    
    if(pcoa_note() == 'There were no negative eigenvalues. No correction was applied') {
      rel_var <- 'Relative_eig'  
    }
    else {
      rel_var <- 'Rel_corr_eig'
    }
    xvar <- round(pcoa_data()$values[,rel_var][input$xPCo]*100, 2)
    yvar <- round(pcoa_data()$values[,rel_var][input$yPCo]*100, 2)
    p <- p + 
      theme_bw(12) +
      xlab(sprintf('PCo %s (%s%%)', input$xPCo, xvar)) +
      ylab(sprintf("PCo %s (%s%%)", input$yPCo, yvar))
    
    p
  })
  
  output$pcoa_plot <- renderPlotly({
    ggplotly(p_pcoa())
  })
  
  output$dl_pcoa_original <- downloadHandler(
    fname <- function() {"ov_pcoa.tiff"}, 
    content <- function(file) {ggsave(file, plot=p_pcoa())}
  )
  
  output$dl_pcoa_html <- downloadHandler(
    fname <- function() {"ov_pcoa.html"},
    content <- function(file) {
      htmlwidgets::saveWidget(as_widget(ggplotly(p_pcoa())), file)
    }
  )
  
  output$dl_pcoa_data <- downloadHandler(
    fname <- function() {"ov_pcoadata.zip"}, 
    content <- function(file) {
      # put together pcoa data to write to file
      to_save <- pcoa_data()
      to_save[['pcoa_plotdata']] <- pdata_pcoa()
      
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      
      to_zip <- sprintf('ov_pcoa%s.csv',names(to_save))
      for(i in 1:length(to_zip)) {
        write.csv(to_save, to_zip[i])  
      }
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
  
  output$dl_pcoa_rds <- downloadHandler(
    fname <- function() {"ov_pcoa.rds"},
    content <- function(file) {
      saveRDS(p_pcoa(), file)
    }
  )
  
  output$dl_pcoa_all <- downloadHandler(
    fname <- function() {"ov_pcoa.zip"},
    content <- function(file) {
      # put together pcoa data to write to file
      to_save <- pcoa_data()
      to_save[['pcoa_plotdata']] <- pdata_pcoa()
      
      # save current directory
      mydir <- getwd()
      # create temporary directory
      tmpdir <- tempdir()
      setwd(tempdir())
      
      to_zip <- c("ov_pcoa.tiff", "ov_pcoa.html","ov_pcoa.rds",
                  sprintf('ov_pcoa%s.csv',names(to_save)))
      
      # writing temp files
      ggsave(to_zip[1], plot=p_pcoa())
      htmlwidgets::saveWidget(as_widget(ggplotly(p_pcoa())), to_zip[2])
      saveRDS(p_pcoa(), to_zip[3])
      for(i in 1:length(to_save)) {
        write.csv(to_save, to_zip[i+3])  
      }
      
      #create the zip file
      zip(file, to_zip)
      setwd(mydir)
    }
  )
}
    
## To be copied in the UI
# mod_ov_pcoa_ui("ov_pcoa_ui_1")
    
## To be copied in the server
# callModule(mod_ov_pcoa_server, "ov_pcoa_ui_1")
 
