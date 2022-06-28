headingPanel(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Iterative Graph",
      plotOutput("theSeqPlot"),
      br(),
      hr(),
      p("To copy or download plot:", "right click the plot, select ", em("Copy image "), "or ", em("Save image as... ")),
    ),
    tabPanel(
      "Report",
    )
  )
)
