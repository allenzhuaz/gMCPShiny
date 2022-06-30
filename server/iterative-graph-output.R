output$pval_update_ui <- renderUI({
  lapply(seq_len(nrow(input$hypothesesMatrix)), function(i) {
    tagList(
      numericInput(
        inputId = paste0("pval_", i),
        label = tagList(
          paste0("Observed p-values for hypothesis ", input$hypothesesMatrix[, "Name"][i])
        ),
        min = 0, max = 1, step = .00001, value = 1
      )
    )
  })
})
outputOptions(output, name = "pval_update_ui", suspendWhenHidden = FALSE)

output$reject_update_ui <- renderUI({
  lapply(seq_len(nrow(input$hypothesesMatrix)), function(i) {
    tagList(
      checkboxInput(
        inputId = paste0("reject_", i),
        label = paste0("Reject hypothesis ", input$hypothesesMatrix[, "Name"][i])
      )
    )
  })
})
outputOptions(output, name = "reject_update_ui", suspendWhenHidden = FALSE)

GetPval <- reactive({
  n_hypo <- nrow(input$hypothesesMatrix)
  sapply(seq_len(n_hypo), function(i) input[[paste0("pval_", i)]])
})

GetReject <- reactive({
  n_hypo <- nrow(input$hypothesesMatrix)
  sapply(
    seq_len(n_hypo), function(i) {
      1 - as.numeric(input[[paste0("reject_", i)]]) # rejection decision is reversed
    }
  )
})

SeqPlotInput <- reactive({
  Trans <- data.frame(input$trwtMatrix)
  keepTransRows <- (Trans[, 1] %in% input$hypothesesMatrix[, 1]) & (Trans[, 2] %in% input$hypothesesMatrix[, 1])
  transitions <- Trans[keepTransRows, ]

  ## Ensure m and alphaHypotheses converted from different types to numeric
  m <- df2graph(namesH = input$hypothesesMatrix[, 1], df = transitions)
  alphaHypotheses <- sapply(input$hypothesesMatrix[, "Alpha"], arithmetic_to_numeric)

  FWER <- sum(alphaHypotheses)
  SeqGraph <- gMCPmini::setWeights(object = gMCPmini::matrix2graph(m), weights = alphaHypotheses / FWER)
  pval <- if (input$knowpval == "yes") GetPval() else GetReject()
  SeqResult <- gMCPmini::gMCP(graph = SeqGraph, pvalues = pval, alpha = FWER)

  ngraphs <- length(SeqResult@graphs)

  gMCPmini::hGraph(
    nHypotheses = nrow(input$hypothesesMatrix),
    nameHypotheses = stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Name"]),
    alphaHypotheses = SeqResult@graphs[[ngraphs]]@weights * FWER,
    m = SeqResult@graphs[[ngraphs]]@m,
    fill = factor(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"]),
      levels = unique(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"]))
    ),
    palette = hgraph_palette(pal_name = rv_nodes$pal_name, n = length(unique(input$hypothesesMatrix[, "Group"])), alpha = rv_nodes$pal_alpha),
    labels = unique(stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Group"])),
    legend.name = input$legend.name,
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
  ) +
    ggplot2::labs(
      title = stringi::stri_unescape_unicode(input$plotTitle),
      caption = stringi::stri_unescape_unicode(input$plotCaption)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = input$title.textsize, hjust = input$title.position),
      plot.caption = ggplot2::element_text(size = input$caption.textsize, hjust = input$caption.position)
    )
})

output$theSeqPlot <- renderPlot({
  SeqPlotInput()
})
