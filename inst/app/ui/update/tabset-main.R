headerCard(
  "Outputs",
  tabsetPanel(
    tabPanel(
      "Graph",
      plotOutput("theSeqPlot"),
      br(),
      hr(),
      actionButton("btn_modal_save_seq_png", label = "Save as PNG", class = "btn btn-outline-primary", icon = icon("download")),
      actionButton("btn_modal_save_seq_pdf", label = "Save as PDF", class = "btn btn-outline-primary", icon = icon("download"))
    ),
    tabPanel(
      "Design",
      "WIP"
    ),
    tabPanel(
      "Report",
      "WIP"
    )
  )
)
