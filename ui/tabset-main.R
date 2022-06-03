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
      p("Code from both the below tabs is needed in order to replicate this hGraph."),
      downloadButton("downloadCode", label = "Download Code to an R File", class = "btn btn-outline-primary"),
      br(),
      br(),
      tabsetPanel(
        tabPanel(
          "Function call for creating the hGraph",
          verbatimTextOutput("changingCode")
        )
      )
    )
  )
)
