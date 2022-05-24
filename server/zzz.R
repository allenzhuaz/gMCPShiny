
# Unused code ------------------------------------------------------------------

observeEvent(input$submit, {
  output$thePlot <- renderPlot({
    input$update

    print(plotInput())
  })
})

### DEBUG
output$debug <- renderUI({
  DF <- dataHypotheses()

  str1 <- paste("Length: ", nrow(DF))

  str2 <- paste("dat()[2,2]: ", DF[2, 2])

  # DFGroups <- dataGroups()
  # str3 <- paste("DFGroups[,2]: ", as.character(DFGroups[,2]))

  HTML(paste(str1, str2, sep = "<br/>"))

  # str(input$hot_select$select)
})


# output$table <- renderTable({dataHypotheses()})

# Download the plot
output$downloadPlot <- downloadHandler(
  filename = "myhGraph.png",
  content = function(file) {
    # device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = plotInput())
  }
)
