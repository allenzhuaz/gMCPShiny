tabPanel(
  title = "hGraph",
  icon = icon("drafting-compass"),
  fluidRow(
    column(
      width = 10, offset = 1,
      fluidRow(
        column(
          width = 4,
          source("ui/tabset-sidebar.R", local = TRUE)$value
        ),
        column(
          width = 8,
          source("ui/tabset-main.R", local = TRUE)$value
        )
      )
    )
  )
)
