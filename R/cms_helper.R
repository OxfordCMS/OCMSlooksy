#' withBusyIndicatorUI
#' 
#' UI function to show busy/loading/error indicator after button is pressed.
#' Corresponds with \code{withBusyIndicatorServer} function.
#' copied from \href{https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R}
#' 
#' @param button Action button for indicator to act on

withBusyIndicatorUI <- function(button) {
  
  
  withBusyIndicatorCSS <- "
    .btn-loading-container {
    margin-left: 10px;
    font-size: 1.2em;
    }
    .btn-done-indicator {
    color: green;
    }
    .btn-err {
    margin-top: 10px;
    color: red;
    }
    "
  
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

#' withBusyIndicatorServer
#' 
#' Call this function from the server with the button id that is clicked and the
#' expression to run when the button is clicked.
#' Corresponds with \code{withBustyIndicatorUI} function.
#' Copied and modified from \href{https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R}
#' 
#' @param buttonID button id
#' @param mod_name name of shiny module where \code{withBusyIndicatorServer} is called

#' @param expr code executed when button clicked

withBusyIndicatorServer <- function(buttonId, mod_name, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s-%s] .btn-loading-indicator", mod_name, buttonId)
  doneEl <- sprintf("[data-for-btn=%s-%s] .btn-done-indicator", mod_name, buttonId)
  errEl <- sprintf("[data-for-btn=%s-%s] .btn-err", mod_name, buttonId)

  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # When an error happens after a button click, show the error
  errorFunc <- function(err, buttonId) {
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
    errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
  }
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

#' myDownloadBttn
#' 
#' Modifying downloadBttn from shinyWidgets so it doesn't have download ison as it's default
#' https://github.com/dreamRs/shinyWidgets
#' @param outputId The name of the output slot that the \code{downloadHandler} is assigned to.
#' @param label The label that should appear on the button.
#' @param style Style of the button, to choose between \code{simple}, \code{bordered},
#' \code{minimal}, \code{stretch}, \code{jelly}, \code{gradient}, \code{fill},
#' \code{material-circle}, \code{material-flat}, \code{pill}, \code{float}, \code{unite}.
#' @param color Color of the button : \code{default}, \code{primary}, \code{warning},
#'  \code{danger}, \code{success}, \code{royal}.
#' @param size Size of the button : \code{xs},\code{sm}, \code{md}, \code{lg}.
#' @param block Logical, full width button.
#' @param no_outline Logical, don't show outline when navigating with
#'  keyboard/interact using mouse or touch.
#'  
myDownloadBttn <- function(outputId,
                         label = "Download",
                         style = "unite",
                         color = "primary",
                         size = "md",
                         block = FALSE,
                         icon_name = "download",
                         no_outline = TRUE) {

  bttn <- actionBttn(
    inputId = paste0(outputId, "_bttn"),
    label = tagList(
      tags$a(
        id = outputId,
        class = "shiny-download-link",
        href = "",
        target = "_blank",
        download = NA
      ),
      label
    ),
    color = color, style = style,
    size = size, block = block,
    no_outline = no_outline,
    icon = icon(icon_name)
  )
  htmltools::tagAppendAttributes(
    bttn,
    onclick = sprintf("getElementById('%s').click()", outputId)
  )
}