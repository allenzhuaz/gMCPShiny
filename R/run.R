#' Run Shiny app
#'
#' @inheritParams shiny::runApp
#'
#' @return TBA
#'
#' @importFrom shiny shinyAppFile
#'
#' @export run
#'
#' @examples
#' \dontrun{
#' gMCPShiny::run()
#' }
run <- function(port = getOption("shiny.port")) {
  shiny::shinyAppFile(
    system.file("app", "app.R", package = "gMCPShiny"),
    options = list(port = port)
  )
}
