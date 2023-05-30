#' Matrix buttons
#'
#' Buttons to add/delete rows and reset a matrix input.
#'
#' @param inputId ID of the matrix input.
#'
#' @return A matrix button group.
#'
#' @importFrom shiny fluidRow column actionButton icon
#'
#' @export
#'
#' @examples
#' matrixButtonGroup("matrix")
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
#' @param x Name of the matrix input to add a row to or delete a row from.
#'
#' @export
addMatrixRow <- function(x) {
  rbind(x, rep(NA, ncol(x)))
}

#' @rdname matrixButtonGroup
#'
#' @export
delMatrixRow <- function(x) {
  if (nrow(x) == 1L) {
    return(x)
  }
  x[-nrow(x), , drop = FALSE]
}
