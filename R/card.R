#' Card with header
#'
#' Card with header
#'
#' @param title TBA
#' @param ... TBA
#'
#' @return TBA
#'
#' @export headerCard
#'
#' @examples
#' NULL
headerCard <- function(title, ...) {
  tags$div(
    class = "card",
    tags$div(
      class = "card-header",
      tags$h5(
        class = "card-title",
        title
      )
    ),
    tags$div(
      class = "card-body",
      ...
    )
  )
}
