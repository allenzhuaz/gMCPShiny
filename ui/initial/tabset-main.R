headingPanel(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Graph",
      plotOutput("thePlot"),
      br(),
      hr(),
      actionButton("btn_modal_save_png", label = "Download Plot", class = "btn btn-outline-primary", icon = icon("download"))
    ),
    tabPanel(
      "Code",
      rcodeOutput("changingCode"),
      downloadButton("downloadCode", label = "Download R Code", class = "btn btn-outline-primary")
    )
  )
)
