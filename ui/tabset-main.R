headingPanel(
  "Outputs",

  # Custom HTML to trigger JS which downloads image from DOM
  # HTML('<a href="#" role="button" class="btn btn-default" onclick="prepHref(this)" download><i class="fa fa-download"></i>Download Image</a>'),

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
