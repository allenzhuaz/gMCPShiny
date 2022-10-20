#' Render R code with syntax highlighting
#'
#' Description TBA
#'
#' @inheritParams shiny::renderText
#' @param outputArgs TBA
#' @param delay TBA
#'
#' @return TBA
#'
#' @importFrom shiny exprToFunction markRenderFunction
#'
#' @export renderRcode
#'
#' @examples
#' NULL
renderRcode <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), delay = 100) {
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
#' @export rcodeOutput
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
