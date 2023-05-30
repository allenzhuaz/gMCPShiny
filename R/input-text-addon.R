#' Text input addon
#'
#' Text input with addon on the right side.
#'
#' @inheritParams shiny::textInput
#' @param addon Addon text.
#'
#' @return Text input with addon on the right side.
#'
#' @references
#' <https://getbootstrap.com/docs/5.3/forms/input-group/>
#'
#' @importFrom htmltools tags span
#' @importFrom shiny validateCssUnit restoreInput
#'
#' @export
#'
#' @examples
#' textInputAddonRight(
#'   "filename",
#'   label = "Name of the report:",
#'   value = "gMCPShiny",
#'   addon = ".Rmd",
#'   width = "100%"
#' )
textInputAddonRight <- function(inputId, label, value = "", width = NULL, placeholder = NULL, addon = NULL) {
  value <- restoreInput(id = inputId, default = value)
  div(
    class = "form-group shiny-input-container",
    style = htmltools::css(width = validateCssUnit(width)),
    shinyInputLabel_(inputId, label),
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

# Copy of shiny:::shinyInputLabel()
shinyInputLabel_ <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    # `id` attribute is required for `aria-labelledby` used by screen readers:
    id = paste0(inputId, "-label"),
    `for` = inputId
  )
}
