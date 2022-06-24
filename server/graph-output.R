options(scipen = 999) # Enforce disabling scientific notation


# Create plot ------------------------------------------------------------------


plotInput <- reactive({
  Trans <- data.frame(input$trwtMatrix)
  keepTransRows <- (Trans[, 1] %in% input$hypothesesMatrix[, 1]) & (Trans[, 2] %in% input$hypothesesMatrix[, 1])
  transitions <- Trans[keepTransRows, ]

  m <- df2graph(namesH = input$hypothesesMatrix[, 1], df = transitions)


  h <- gMCPmini::hGraph(
    nHypotheses = nrow(input$hypothesesMatrix),
    nameHypotheses = stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Name"]),
    alphaHypotheses = sapply(input$hypothesesMatrix[, "Alpha"], arithmetic_to_numeric),
    m = m,
    fill = factor(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"]),
      levels = unique(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"]))
    ),
    palette = hgraph_palette(pal_name = rv_nodes$pal_name, n = length(unique(input$hypothesesMatrix[, "Group"])), alpha = rv_nodes$pal_alpha),
    labels = unique(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"])),
    legend.name = stringi::stri_unescape_unicode(input$legend.name),
    legend.position = input$legendPosition,
    halfWid = rv_nodes$width,
    halfHgt = rv_nodes$height,
    trhw = rv_edges$trhw,
    trhh = rv_edges$trhh,
    trprop = rv_edges$trprop,
    digits = rv_nodes$digits,
    trdigits = rv_edges$trdigits,
    size = rv_nodes$size,
    boxtextsize = rv_edges$boxtextsize,
    legend.textsize = input$legend.textsize,
    arrowsize = rv_edges$arrowsize,
    offset = rv_edges$offset,
    x = if (is.null(input$nodeposMatrix[, "x"]) | !setequal(input$nodeposMatrix[, "Hypothesis"], input$hypothesesMatrix[, "Name"])) {
      NULL
    } else {
      as.numeric(input$nodeposMatrix[, "x"])
    },
    y = if (is.null(input$nodeposMatrix[, "y"]) | !setequal(input$nodeposMatrix[, "Hypothesis"], input$hypothesesMatrix[, "Name"])) {
      NULL
    } else {
      as.numeric(input$nodeposMatrix[, "y"])
    },
    wchar = stringi::stri_unescape_unicode(rv_nodes$wchar)
  )

  if (input$plotTitle != "") {
    hjust <- switch(input$title.position,
      "top left" = 0,
      "top center" = 0.5,
      "top right" = 1,
      "bottom left" = 0,
      "bottom center" = 0.5,
      "bottom right" = 1
    )
    if (grepl(x = input$title.position, pattern = "top")) {
      h <- h +
        ggplot2::labs(title = stringi::stri_unescape_unicode(input$plotTitle)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = input$title.textsize, hjust = hjust))
    } else if (grepl(x = input$title.position, pattern = "bottom")) {
      h <- h +
        ggplot2::labs(caption = stringi::stri_unescape_unicode(input$plotTitle)) +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = input$title.textsize, hjust = hjust))
    }
  }

  h
})

output$thePlot <- renderPlot({
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
