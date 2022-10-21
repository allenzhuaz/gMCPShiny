#' Run Shiny app
#'
#' @inheritParams shiny::runApp
#'
#' @return TBA
#'
#' @importFrom shiny shinyAppFile
#'
#' @export run_app
#'
#' @examples
#' \dontrun{
#' gMCPShiny::run_app()
#' }
run_app <- function(port = getOption("shiny.port")) {
  shiny::shinyAppFile(
    system.file("app", "app.R", package = "gMCPShiny"),
    options = list(port = port)
  )
}
