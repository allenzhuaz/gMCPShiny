# File input but with only the button
fileButtonInput <- function(inputId, label, multiple = FALSE, accept = NULL, width = NULL,
                            buttonLabel = "Browse...", placeholder = "No file selected") {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(
    id = inputId, name = inputId, type = "file",
    style = "display: none;", `data-restore` = restoredValue
  )
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  div(
    style = "display: inline-block; width: 10rem; margin-bottom: -2rem;", # not super elegant but works
    div(
      class = "form-group shiny-input-container", style = if (!is.null(width)) {
        paste0("width: ", validateCssUnit(width), ";")
      },
      shiny:::shinyInputLabel(inputId, label),
      div(
        class = "input-group",
        style = "width: 10rem;",
        tags$label(
          class = "input-group-prepend",
          span(
            class = "btn btn-outline-primary",
            HTML('<i class="fa fa-upload"></i>'),
            buttonLabel,
            inputTag
          )
        )
      )
    )
  )
}