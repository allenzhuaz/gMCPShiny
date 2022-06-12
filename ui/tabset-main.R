headingPanel(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Graph",
      plotOutput("thePlot"),
      hr(),
      p("Right click the above graph, then select ", em("Copy image "), "or ", em("Save image as... "), "to copy or download it."),
    ),
    tabPanel(
      "Code",
      downloadButton("downloadCode", label = "Download R Code", class = "btn btn-outline-primary", style = "margin-bottom: 1rem;"),
      rcodeOutput("changingCode")
    )
  )
)
