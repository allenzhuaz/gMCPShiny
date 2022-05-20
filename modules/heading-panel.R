# Panel with heading
headingPanel <- function(title, ...) {
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
