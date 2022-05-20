tags$footer(
  class = "my-5",
  fluidRow(
    column(
      width = 10, offset = 1,
      hr(),
      # Use YYYY.0M.MICRO as defined by https://calver.org/
      p(paste("hgraph app", as.vector(read.dcf("DESCRIPTION", fields = "Version"))), class = "text-muted"),
      # p(paste("gMCPmini", packageVersion("gMCPmini")), class = "text-muted")
    )
  )
)
