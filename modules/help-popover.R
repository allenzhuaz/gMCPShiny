# Creates ? Help Popovers
helpPopover <- function(title, content) {
  tags$a(
    `data-bs-toggle` = "popover",
    title = title,
    `data-bs-trigger` = "hover focus",
    `data-bs-content` = content,
    `data-bs-placement` = "right",
    class = "help-popover",
    shiny::icon("question-circle")
  )
}

helpLink <- function(href) {
  tags$a(
    href = href,
    target = "_blank",
    class = "help-popover",
    shiny::icon("external-link-alt")
  )
}
