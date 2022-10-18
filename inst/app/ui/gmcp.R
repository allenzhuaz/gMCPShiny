tabPanel(
  title = "Iterative Graphs Update",
  icon = icon("hourglass"),
  fluidRow(
    column(
      width = 10, offset = 1,
      fluidRow(
        column(
          width = 12,
          span("Update Iterative Graphs", style = "font-size:1.5rem;")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          source("ui/update/tabset-sidebar.R", local = TRUE)$value
        ),
        column(
          width = 8,
          source("ui/update/tabset-main.R", local = TRUE)$value
        )
      )
    )
  )
)
