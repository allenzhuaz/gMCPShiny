headingPanel(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Graph",
      plotOutput("thePlot"),
      br(),
      hr(),
      p("To copy or download plot:", "right click the plot, select ", em("Copy image "), "or ", em("Save image as... ")),
    ),
    tabPanel(
      "Code",
      downloadButton("downloadCode", label = "Download R Code", class = "btn btn-outline-primary", style = "margin-bottom: 1rem;"),
      rcodeOutput("changingCode")
    )
  )
)
