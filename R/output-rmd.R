#' Render R Markdown with syntax highlighting
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
#' @export renderRmd
#'
#' @examples
#' NULL
renderRmd <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), delay = 100) {
  func <- exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    value <- func()
    for (d in delay) {
      shinysession$sendCustomMessage("highlight-rmd", list(id = name, delay = d))
    }
    return(paste(utils::capture.output(cat(value)), collapse = "\n"))
  }
  markRenderFunction(rmdOutput, renderFunc, outputArgs = outputArgs)
}

#' @rdname renderRmd
#'
#' @param outputId Output variable to read the R Markdown from.
#'
#' @importFrom htmltools tagList
#' @importFrom shiny uiOutput
#'
#' @export rmdOutput
rmdOutput <- function(outputId) {
  tagList(
    rmdHighlightDeps(),
    uiOutput(outputId, container = rmdContainer)
  )
}

#' @importFrom htmltools tagList singleton includeCSS includeScript
rmdHighlightDeps <- function() {
  src <- system.file("www/shared/shiny-highlight-rmarkdown", package = "gMCPShiny")
  tagList(
    singleton(list(
      includeCSS(file.path(src, "highlight-theme.css")),
      includeScript(file.path(src, "highlight.min.js")),
      includeScript(file.path(src, "r.js"))
    )),
    singleton(list(
      includeScript(file.path(src, "markdown.js")),
      includeScript(file.path(src, "yaml.min.js")),
      includeScript(file.path(src, "latex.min.js"))
    )),
    singleton(list(
      tags$script(
        "Shiny.addCustomMessageHandler(
           'highlight-rmd',
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
rmdContainer <- function(...) {
  rmd <- HTML(as.character(tags$code(class = "language-md", ...)))
  div(tags$pre(rmd, style = "background-color: #FFFFFF;"))
}
