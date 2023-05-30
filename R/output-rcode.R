#' Render R code with syntax highlighting
#'
#' Renders R code with syntax highlighting using the highlight.js library.
#'
#' @inheritParams shiny::renderText
#' @param outputArgs List of additional arguments to pass to the
#'   output function.
#' @param delay Delay in milliseconds before syntax highlighting starts.
#'
#' @return A render function similar to [shiny::renderText()].
#'
#' @importFrom shiny exprToFunction markRenderFunction
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library("shiny")
#'
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(
#'         4,
#'         textAreaInput(
#'           "rcode_in",
#'           label = NULL,
#'           width = "100%", height = "300px",
#'           value = "library('shiny')\n\nset.seed(42)\nx <- runif(10)"
#'         )
#'       ),
#'       column(
#'         8,
#'         rcodeOutput("rcode_out")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$rcode_out <- renderRcode({
#'       htmltools::htmlEscape(input$rcode_in)
#'     })
#'   }
#'
#'   shinyApp(ui = ui, server = server)
#' }
renderRcode <- function(
    expr,
    env = parent.frame(),
    quoted = FALSE,
    outputArgs = list(),
    delay = 100) {
  func <- exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    value <- func()
    for (d in delay) {
      shinysession$sendCustomMessage("highlight-rcode", list(id = name, delay = d))
    }
    return(paste(utils::capture.output(cat(value)), collapse = "\n"))
  }
  markRenderFunction(rcodeOutput, renderFunc, outputArgs = outputArgs)
}

#' @rdname renderRcode
#'
#' @param outputId Output variable to read the R code from.
#'
#' @importFrom htmltools tagList
#' @importFrom shiny uiOutput
#'
#' @export
rcodeOutput <- function(outputId) {
  tagList(
    rcodeHighlightDeps(),
    uiOutput(outputId, container = rcodeContainer)
  )
}

#' @importFrom htmltools tagList singleton includeCSS includeScript
rcodeHighlightDeps <- function() {
  src <- system.file("www/shared/shiny-highlight-rmarkdown", package = "gMCPShiny")
  tagList(
    singleton(list(
      includeCSS(file.path(src, "highlight-theme.css")),
      includeScript(file.path(src, "highlight.min.js")),
      includeScript(file.path(src, "r.js"))
    )),
    singleton(list(
      tags$script(
        "Shiny.addCustomMessageHandler(
           'highlight-rcode',
           function (message) {
               var id = message['id'];
               var delay = message['delay'];
               setTimeout(
                   function () {
                       var el = document.getElementById(id);
                       hljs.highlightElement(el);
                   },
                   delay
               );
           }
       );"
      )
    ))
  )
}

#' @importFrom htmltools tags div HTML
rcodeContainer <- function(...) {
  rcode <- HTML(as.character(tags$code(class = "language-r", ...)))
  div(tags$pre(rcode, style = "background-color: #FFFFFF;"))
}
