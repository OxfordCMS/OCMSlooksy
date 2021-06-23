#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import rmarkdown
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h1("Generate Report"),
        h4("Report options"),
        radioButtons(ns("report_type"), "Report type",
                     c("html", "pdf")),
        withBusyIndicatorUI(
          myDownloadBttn(ns("report"), "Export Report",
                         style = "bordered", size = "sm")
        )
      )
    )
  )
}

#' report Server Function
#'
#' @noRd
mod_report_server <- function(input, output, session, bridge, template, file_name){
  ns <- session$ns

  withBusyIndicatorServer("report", 'report_ui_1',{
    output$report <- downloadHandler(
  
      # generate file names based on options and file_name supplied
      filename = function() {
        paste(file_name, input$report_type, sep = ".")
      },
  
      content = function(file) {
        
        id <- showNotification(
          "Rendering report...", 
          duration = NULL, 
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        
        # point to template report
        template_file <- system.file("rmarkdown", "templates", template,
                                     "skeleton", "skeleton.Rmd",
                                     package = "OCMSlooksy")
  
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        # save current directory
        mydir <- getwd()
        # create temporary directory
        tmpdir <- tempdir()
        setwd(tmpdir)
  
  
        # copy template into temporary directory before rendering it
        # in case of limited write permissions
        temp_report <- file.path(tmpdir, paste(file_name, "Rmd", sep='.'))
        file.copy(template_file, temp_report, overwrite = TRUE)
  
        # params contain parameters to pass to Rmd document
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- render(temp_report, output_file = file,
                      output_format = switch(input$report_type,
                                             pdf = pdf_document(),
                                             html = html_document()),
                      params = bridge$params,
                      envir = new.env(parent = globalenv()))
        setwd(mydir)
      },
      contentType = 'text/html'
    )
  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# callModule(mod_report_server, "report_ui_1")
