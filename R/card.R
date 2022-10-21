#' Card with header
#'
#' Card with header
#'
#' @param title TBA
#' @param ... TBA
#'
#' @return TBA
#'
#' @importFrom htmltools tags div
#'
#' @export headerCard
#'
#' @examples
#' NULL
headerCard <- function(title, ...) {
  div(
    class = "card",
    div(
      class = "card-header",
      tags$h5(
        class = "card-title",
        title
      )
    ),
    div(
      class = "card-body",
      ...
    )
  )
}
