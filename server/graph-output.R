
# Initial node position uioutput
output$initnodepos <- renderUI({
  radianStart <- if((nrow(input$hypothesesMatrix))%%2 != 0) {
    pi * (1/2 + 1/nrow(input$hypothesesMatrix)) } else {
      pi * (1 + 2/nrow(input$hypothesesMatrix))/2 }

  matrixInput("nodeposMatrix",
              label = tagList(
                "Node position matrix",
                helpPopover(
                  "nodeposMatrix",
                  "The text input supports Unicode escape sequence like \\uABCD. Use \\n to add a line break.
                            The x, y coordinates are for the relative position of the hypothesis ellipses."
                )
              ),
              value = as.matrix(data.frame(cbind(Hypothesis = input$hypothesesMatrix[,"Name"],
                                                 x = round(2 * cos((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi)), 6),
                                                 y = round(2 * sin((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi)), 6)
              ))),
              class = "character",
              rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
              cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
  )
})


# Create plot ------------------------------------------------------------------


plotInput <- reactive({

  Trans <- data.frame(input$trwtMatrix)
  keepTransRows <-  (Trans[,1] %in% input$hypothesesMatrix[,1]) & (Trans[,2] %in% input$hypothesesMatrix[,1])
  transitions <- Trans[keepTransRows,]

  m <- df2graph(namesH = unique(input$hypothesesMatrix[,1]), df = transitions)


  gMCPmini::hGraph(
    nHypotheses = nrow(input$hypothesesMatrix),
    nameHypotheses = stringi::stri_unescape_unicode(input$hypothesesMatrix[,"Name"]),
    alphaHypotheses = as.numeric(input$hypothesesMatrix[,"Alpha"]),
    m = m,
    fill = input$hypothesesMatrix[,"Group"],
    palette = hgraph_palette(pal_name = input$pal_name, n = length(unique(input$hypothesesMatrix[,"Group"])), alpha = input$pal_alpha),
    labels = stringi::stri_unescape_unicode(input$hypothesesMatrix[,"Group"]),
    legend.name = input$legend.name,
    legend.position = input$legendPosition,
    halfWid = input$width,
    halfHgt = input$height,
    trhw = input$trhw,
    trhh = input$trhh,
    trprop = input$trprop,
    digits = input$digits,
    trdigits = input$trdigits,
    size = input$size,
    boxtextsize = input$boxtextsize,
    legend.textsize = input$legend.textsize,
    arrowsize = input$arrowsize,
    offset = input$offset,
    x = if(is.null(input$nodeposMatrix[,"x"]) | !setequal(input$nodeposMatrix[,"Hypothesis"], input$hypothesesMatrix[,"Name"])){NULL} else{as.numeric(input$nodeposMatrix[,"x"])},
    y = if(is.null(input$nodeposMatrix[,"y"]) | !setequal(input$nodeposMatrix[,"Hypothesis"], input$hypothesesMatrix[,"Name"])){NULL} else{as.numeric(input$nodeposMatrix[,"y"])},
    wchar = stringi::stri_unescape_unicode(input$wchar)
  )


})

output$thePlot <- renderPlot({
  print(parseQueryString(session$clientData$url_search))
  print(plotInput())
})
outputOptions(output, "thePlot", suspendWhenHidden = FALSE) # thePlot runs even when not visible

# Output for graph code that changes based on user inputs
getChangingCode <- reactive({
  report <- tempfile(fileext = ".R")
  brew::brew("templates/call-hgraph.R", output = report)
  paste0(readLines(report), collapse = "\n")
})

output$changingCode <- renderRcode({
  htmltools::htmlEscape(getChangingCode())
})

# Download graph code
output$downloadCode <- downloadHandler(
  filename = "hgraph.R",
  content = function(file) {
    write(getChangingCode(), file)
  }
)
