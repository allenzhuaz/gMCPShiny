#' Text input addon
#'
#' Text input with addon on the right side.
#' From <https://getbootstrap.com/docs/5.2/forms/input-group/>.
#'
#' @inheritParams shiny::textInput
#' @param addon Addon text
#'
#' @return TBA
#'
#' @importFrom htmltools tags span
#' @importFrom shiny validateCssUnit restoreInput
#'
#' @export textInputAddonRight
#'
#' @examples
#' NULL
textInputAddonRight <- function(inputId, label, value = "", width = NULL, placeholder = NULL, addon = NULL) {
  shinyInputLabel <- function(inputId, label = NULL) {
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      # `id` attribute is required for `aria-labelledby` used by screen readers:
      id = paste0(inputId, "-label"),
      `for` = inputId
    )
  }

  value <- restoreInput(id = inputId, default = value)
  div(
    class = "form-group shiny-input-container",
    style = htmltools::css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    div(
      class = "input-group",
      tags$input(
        id = inputId,
        type = "text",
        class = "form-control",
        value = value,
        placeholder = placeholder
      ),
      span(addon, class = "input-group-text")
    )
  )
}
