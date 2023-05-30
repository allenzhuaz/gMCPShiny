#' Card with header
#'
#' Card with header (Bootstrap 5).
#'
#' @param title Card title.
#' @param ... List of elements to include in the body of the card.
#'
#' @return Card element with header and body.
#'
#' @importFrom htmltools tags div
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   headerCard("Card title", "Card body")
#' }
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
