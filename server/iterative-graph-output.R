output$pval_update_ui <- renderUI({
  lapply(seq_len(nrow(input$hypothesesMatrix)), function(i) {
    tagList(
      hr(),
      h5(paste0("Hypothesis ", input$hypothesesMatrix[, "Name"][i])),

      selectInput(
        inputId = paste0("design_type_", i),
        label = "Type of Design:", choices = list("Fixed Design" = c("Input observed p-value" = "fix"),
                                                  "Group Sequential Design" = c("Input analysis, p-value boundary and observed p-value" = "gs",
                                                                                "Upload design data and input observed p-value" = "gs_upload",
                                                                                "Input sequential p-value" = "gs_seqp", ## directly input sequential p-value for gsDesign is similar to input observed p-value for fixed design
                                                                                "Upload design data and input observed p-value to calculate sequential p-value" = "gs_upload_seqp")),
        selectize = FALSE
      ),
      conditionalPanel(
        condition = paste0("input.design_type_", i, " == 'gs_upload'"),
        fileButtonInput(inputId = paste0("btn_update_restore_", i), label = NULL,
                        buttonLabel = "Upload Design",
                        multiple = FALSE, accept = ".rds", width = "100%")

      ),
      conditionalPanel(
        condition = paste0("input.design_type_", i, " == 'gs'"),
        matrixInput(paste0("pvalMatrix_", i),
                    label = tagList(
                      "p-value boundary and observed p-value:",
                      helpPopover(
                        "p-values matrix",
                        "\"Analysis\" requires text input, shoud match hypotheses names.
            \"pvalueBoundary\" and \"pvalueObserved\" supports numeric input and arithmetic expressions, should range between 0 and 1."
                      )
                    ),
                    value = as.matrix(data.frame(cbind(
                      Analysis = c("IA 1", "IA 2", "Final"),
                      pvalueBoundary = c(0.0031250, 0.0046875, 0.0059375),
                      pvalueObserved = rep(1, 3)
                    ))),
                    class = "character",
                    rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
                    cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
        ),
        matrixButtonGroup(paste0("pvalMatrix_", i)),
      )
      ,
      conditionalPanel(
        condition = paste0("input.design_type_", i, " == 'gs_seqp'"),
        numericInput(
          inputId = paste0("pval_", i),
          label = "Sequential p-value:",
          min = 0, max = 1, step = .00001, value = 1
        )),

      conditionalPanel(
        condition = paste0("input.design_type_", i, " == 'fix'"),
        numericInput(
        inputId = paste0("pval_", i),
        label = "Observed p-value:",
        min = 0, max = 1, step = .00001, value = 1
      )),

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
  sapply(seq_len(n_hypo), function(i){
    if (input[[paste0("design_type_", i)]] %in% c("fix", "gs_seqp")){
    input[[paste0("pval_", i)]]
    } else if(input[[paste0("design_type_", i)]] == "gs"){
      # Bonferroni type testing
      1 - any(sapply(input[[paste0("pvalMatrix_", i)]][,"pvalueObserved"], arithmetic_to_numeric, USE.NAMES = FALSE) < sapply(input[[paste0("pvalMatrix_", i)]][,"pvalueBoundary"], arithmetic_to_numeric, USE.NAMES = FALSE))
    } else if(input[[paste0("design_type_", i)]] == "gs_upload"){
      ## ToDo with uploaded gsDesign p-value boundary ##
    } else if(input[[paste0("design_type_", i)]] == "gs_upload_seqp"){
      ## ToDo with uploaded gsDesign p-value boundary ##
    }
  })
})

GetGSRej <- reactive({
  n_hypo <- nrow(input$hypothesesMatrix)
  sapply(seq_len(n_hypo), function(i){
  })
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
  SeqGraph <- gMCPLite::setWeights(object = gMCPLite::matrix2graph(m), weights = alphaHypotheses / FWER)
  pval <- if (input$knowpval == "yes") GetPval() else GetReject()
  SeqResult <- gMCPLite::gMCP(graph = SeqGraph, pvalues = pval, alpha = FWER)

  ngraphs <- length(SeqResult@graphs)

  gMCPLite::hGraph(
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
