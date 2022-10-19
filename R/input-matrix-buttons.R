#' Matrix buttons
#'
#' Buttons to add/delete rows and reset a matrix input
#'
#' @param inputId TBA
#'
#' @return TBA
#'
#' @importFrom shiny fluidRow column actionButton icon
#'
#' @export matrixButtonGroup
#'
#' @examples
#' NULL
matrixButtonGroup <- function(inputId) {
  fluidRow(
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_addrow"),
        label = "",
        icon = icon("plus"),
        width = "100%",
        class = "btn btn-block btn-outline-primary"
      )
    ),
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_delrow"),
        label = "",
        icon = icon("minus"),
        width = "100%",
        class = "btn btn-block btn-outline-primary"
      )
    ),
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_reset"),
        label = "",
        icon = icon("arrow-rotate-left"),
        width = "100%",
        class = "btn btn-block btn-outline-primary"
      )
    )
  )
}

#' @rdname matrixButtonGroup
#'
#' @param x TBA
#'
#' @export addMatrixRow
addMatrixRow <- function(x) {
  rbind(x, rep(NA, ncol(x)))
}

#' @rdname matrixButtonGroup
#'
#' @param x TBA
#'
#' @export delMatrixRow
delMatrixRow <- function(x) {
  if (nrow(x) == 1L) {
    return(x)
  }
  x[-nrow(x), , drop = FALSE]
}
