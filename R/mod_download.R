#' download UI Function
#'
#' @description UI function to make a dropdown menu from icon to save plot in various formats
#'
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
mod_download_ui <- function(id){
  ns <- NS(id)
  tagList(
    dropdown(
      size = 'xs', icon = icon('save'), inline = TRUE,
      style = 'material-circle', width = 160,
      animate = animateOptions(
        enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
        exit = shinyWidgets::animations$fading_exits$fadeOutLeft),

      hidden(div(
        id =ns('dl_fig_div'),
        myDownloadBttn(ns('dl_fig'), icon_name = 'file-image',
                       label = "Image (png)",
                       size = 'xs', style = 'minimal')
      )),
      hidden(div(
        id = ns('dl_pdf_div'),
        myDownloadBttn(ns('dl_pdf'), icon_name = 'file-image',
                       label = "Image (pdf)",
                       size = 'xs', style = 'minimal')
      )),
      hidden(div(
        id = ns('dl_html_div'),
        myDownloadBttn(ns('dl_html'), icon_name = 'file-code',
                       label = "Interactive plot",
                       size = 'xs', style = 'minimal')
      )),
      hidden(div(
        id = ns('dl_data_div'),
        myDownloadBttn(ns('dl_data'), icon_name = 'file-alt',
                       label = "Plot data",
                       size = 'xs', style = 'minimal')
      )),
      hidden(div(
        id = ns('dl_all_div'),
        myDownloadBttn(ns('dl_all'), icon_name = 'file-archive',
                       label = "All",
                       size = 'xs', style = 'minimal')
      ))
    )
  )
}

#' download Server Function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param bridge reactiveValues. contains objects to be passed from outer module
#' @param file_name string. default file name used when downloading
#' @param dl_options determins which download buttons/options to allow in
#'  dropdown menu. defaults to all: png, html, pdf, csv, zip.
#'
#' @noRd
mod_download_server <- function(input, output, session, bridge, file_name,
                                dl_options = c('png','html','csv','pdf','zip')){
  ns <- session$ns

  # generate file names based on options and file_name supplied
  file_ext <- sprintf("%s.%s", file_name, dl_options)

  observe({
    # download plot png
    if('png' %in% dl_options) {
      show('dl_fig_div')

      output$dl_fig <- downloadHandler(

        filename = function() {
          file_ext[grepl('png', file_ext)]
        },
        content = function(file) {
          ggsave(file, bridge$figure, height=5.21, width= 7.21, units='in')
        },
        contentType = 'image/png'
      )

    }

    # download interactiv eplot
    if('html' %in% dl_options) {
      show('dl_html_div')

      output$dl_html <- downloadHandler(
        filename = function() {
          file_ext[grepl('html', file_ext)]
        },
        content = function(file) {
          htmlwidgets::saveWidget(
            as_widget(ggplotly(bridge$figure)), file)
        },
        contentType = 'text/html'
      )
    }

    # download plot data
    if('csv' %in% dl_options) {
      show('dl_data_div')

      output$dl_data <- downloadHandler(
        filename = function() {
          file_ext[grepl('csv', file_ext)]
        },
        content = function(file) {
          write.csv(bridge$fig_data, file, row.names = FALSE)
        },
        contentType = 'text/csv'
      )
    }

    # download plot as rds
    if('pdf' %in% dl_options) {
      show('dl_pdf_div')

      output$dl_pdf <- downloadHandler(
        filename = function() {file_ext[grepl('pdf', file_ext)]},
        content = function(file) {
          ggsave(file, bridge$figure, height=5.21, width = 7.21, units='in')
        },
        contentType = 'application/pdf'
      )
    }

    # download all in zip file
    if('zip' %in% dl_options) {
      show('dl_all_div')

      output$dl_all <- downloadHandler(
        filename = function() {file_ext[grepl('zip', file_ext)]},
        content = function(file) {
          # save current directory
          mydir <- getwd()
          # create temporary directory
          tmpdir <- tempdir()
          setwd(tempdir())

          if('png' %in% dl_options) {
            ggsave(file_ext[grepl('png', file_ext)], bridge$figure)
          }
          if('html' %in% dl_options) {
            htmlwidgets::saveWidget(as_widget(ggplotly(bridge$figure)),
                                    file_ext[grepl('html', file_ext)])
          }
          if('pdf' %in% dl_options) {
            ggsave(file_ext[grepl('pdf', file_ext)], bridge$figure)
          }
          if('csv' %in% dl_options) {
            for(i in file_ext[grepl('csv', file_ext)]) {
              write.csv(bridge$fig_data, i, row.names = FALSE)
            }
          }

          #create the zip file
          zip(file, file_ext[!grepl('zip', file_ext)])
          setwd(mydir)
        },
        contentType = 'application/zip'
      )
    }
  })
}

## To be copied in the UI
# mod_download_ui("download_ui_1")

## To be copied in the server
# callModule(mod_download_server, "download_ui_1")

