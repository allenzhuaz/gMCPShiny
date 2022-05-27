# Buttons to add/delete rows and reset a matrix input
matrixButtonGroup <- function(inputId) {
  fluidRow(
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_addrow"),
        label = "",
        icon = icon("plus"),
        width = "100%",
        class = "btn btn-block btn-outline"
      )
    ),
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_delrow"),
        label = "",
        icon = icon("minus"),
        width = "100%",
        class = "btn btn-block btn-outline"
      )
    ),
    column(
      4,
      actionButton(
        paste0("btn_", inputId, "_reset"),
        label = "",
        icon = icon("undo"),
        width = "100%",
        class = "btn btn-block btn-outline"
      )
    )
  )
}



addMatrixRow <- function(x) {
  rbind(x, rep(NA, ncol(x)))
}

delMatrixRow <- function(x) {
  if (nrow(x) == 1L) {
    return(x)
  }
  x[-nrow(x), , drop = FALSE]
}
