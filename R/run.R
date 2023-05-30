#' Run Shiny app
#'
#' @inheritParams shiny::runApp
#'
#' @return An object that represents the app.
#'   Printing the object or passing it to [shiny::runApp()] will run the app.
#'
#' @importFrom shiny shinyAppFile
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   gMCPShiny::run_app()
#' }
run_app <- function(port = getOption("shiny.port")) {
  shiny::shinyAppFile(
    system.file("app", "app.R", package = "gMCPShiny"),
    options = list(port = port)
  )
}
