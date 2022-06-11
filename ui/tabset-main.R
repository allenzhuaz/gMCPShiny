headingPanel(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Graph",
      plotOutput("thePlot")
    ),
    tabPanel(
      "Code",
      downloadButton("downloadCode", label = "Download R Code", class = "btn btn-outline-primary", style = "margin-bottom: 1rem;"),
      rcodeOutput("changingCode")
    )
  )
)
