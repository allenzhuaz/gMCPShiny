output$pval_update_ui <- renderUI({
  lapply(seq_len(nrow(input$hypothesesMatrix)), function(i) {
    tagList(
      hr(),
      h5(paste0("Hypothesis ", input$hypothesesMatrix[, "Name"][i])),

      selectInput(
        inputId = paste0("design_type_", i),
        label = "Type of Design:", choices =  c("Fixed Design" = "fix",
                                                "Group Sequential Design" = "gs_upload"),
        selectize = FALSE
      ),

      conditionalPanel(
        condition = paste0("input.design_type_", i, " == 'gs_upload'"),
        #checkboxInput("minialpha", label = "With minimum alpha spending", value = FALSE, width = "100%"),
        selectInput(
          "spending_interim",
          label = tagList(
            "Interim analysis alpha spending strategy",
            helpPopover(
              "spending_interim",
              "Spend alpha at interim analyses based on?"
            )
          ),
          choices = c(
            "Actual information fraction" = "info",
            "Minimum of planned and actual information fraction" = "pmin"
          )
        ),
        matrixInput(paste0("pvalMatrix_", i),
                    label = tagList(
                      "observed events and p-values:",
                      helpPopover(
                        "p-values matrix",
                        "\"Analysis\" requires text input, shoud match hypotheses names.
            \"pvalueBoundary\" and \"pvalueObserved\" supports numeric input and arithmetic expressions, should range between 0 and 1."
                      )
                    ),
                    value = as.matrix(data.frame(cbind(
                      Analysis = c("1", "2", "3"),
                      ObsEvents = c(120, 240, 360),
                      ObsPval = rep(1, 3)
                    ))),
                    class = "character",
                    rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
                    cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
        ),
        matrixButtonGroup(paste0("pvalMatrix_", i)),
        fileButtonInput(inputId = paste0("btn_gsdesign_", i), label = NULL,
                        buttonLabel = "Upload Design",
                        multiple = FALSE, accept = ".rds", width = "100%")

      ),

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
    if (input[[paste0("design_type_", i)]] %in% c("fix")){
      input[[paste0("pval_", i)]]
    } else if(input[[paste0("design_type_", i)]] == "gs_upload"){
      ##Read in uploaded design
      rds <- input[[paste0("btn_gsdesign_",i)]]
      req(rds)
      design <- readRDS(rds$datapath)
      ##get observed events from input
      obsEvents <- sapply(input[[paste0("pvalMatrix_", i)]][,"ObsEvents"], arithmetic_to_numeric, USE.NAMES = FALSE)
      ##spending time
      if (input$spending_interim == "info"){
        spendingTime <- obsEvents/max(design$gs_object$n.I)
      } else if (input$spending_interim == "pmin") {
        spendingTime <- apply(cbind(obsEvents,design$gs_object$n.I[1:length(obsEvents)]),1,min)/max(design$gs_object$n.I)
      }
      #spendingTime <- ifelse(spendingTime>1,1,spendingTime)

      gsDesign::sequentialPValue(
        gsD = design$gs_object, interval = c(.0001, .9999),
        n.I = obsEvents,
        Z = -qnorm(sapply(input[[paste0("pvalMatrix_", i)]][,"ObsPval"], arithmetic_to_numeric, USE.NAMES = FALSE)),
        usTime = spendingTime
      )
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
  return(gMCPLite::gMCP(graph = SeqGraph, pvalues = pval, alpha = FWER))
})

fwerInput <- reactive({
  alphaHypotheses <- sapply(input$hypothesesMatrix[, "Alpha"], arithmetic_to_numeric)
  sum(alphaHypotheses)
})

# Create tabs
output$theSeqPlot <- renderUI({

  ngraphs <- length(SeqPlotInput()@graphs)

  plot_tabs <- lapply(seq_len(ngraphs), function(i){
    tabPanel(paste("Graph", i),
             plotOutput(paste("graph", i)))
  })

  do.call(tabsetPanel, plot_tabs)
})

# Create plots
observe(
  lapply(seq_len(nrow(input$hypothesesMatrix)+1), function(i) {
    output[[paste("graph", i)]] <- renderPlot({
      if(i==nrow(input$hypothesesMatrix)+1){
        m <- SeqPlotInput()@graphs[[nrow(input$hypothesesMatrix)]]@m
      } else{
        m <- SeqPlotInput()@graphs[[i]]@m
      }
      rownames(m) <- NULL
      colnames(m) <- NULL

      gMCPLite::hGraph(
        nHypotheses = nrow(input$hypothesesMatrix),
        nameHypotheses = stringi::stri_unescape_unicode(input$hypothesesMatrix[, "Name"]),
        alphaHypotheses = SeqPlotInput()@graphs[[i]]@weights * fwerInput(),
        m = m,
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
  })
)

# Initial Design output ---------------------------------------------------------------
output$gsDesign <- renderUI({
  if (input$knowpval == "yes"){
    myDesign<-list()
    for (i in 1:nrow(input$hypothesesMatrix)){
      if (input[[paste0("design_type_", i)]] == "fix"){
        myDesign[[i]]<-tabPanel(title = input$hypothesesMatrix[i,"Name"],
                 h3(paste0(input$hypothesesMatrix[i,"Name"], " is fixed sample size design.")))
    }
      if (input[[paste0("design_type_", i)]] != "fix"){
        rds <- input[[paste0("btn_gsdesign_",i)]]
        req(rds)
        design <- readRDS(rds$datapath)
        output[[paste0("tmp",i)]]<-renderTable({
          gsDesign::gsBoundSummary(design$gs_object)
        },include.rownames=FALSE)
        myDesign[[i]]<-tabPanel(title = input$hypothesesMatrix[i,"Name"],
                                htmlOutput(paste0("tmp",i)))}
    }
    do.call(tabsetPanel,myDesign)
  }
})

# Tabular output ---------------------------------------------------------------
output$TestResultsHTML <- renderUI(
  {
    ##sequntial pvalues to graphs comparison
    EOCtab <- data.frame(input$hypothesesMatrix[,c(1,3)])
    EOCtab$seqp <- GetPval()
    EOCtab$Rejected <- SeqPlotInput()@rejected
    EOCtab$adjPValues <- SeqPlotInput()@adjPValues
    FWER <- sum(sapply(input$hypothesesMatrix[, "Alpha"], arithmetic_to_numeric))
    ngraphs <- length(SeqPlotInput()@graphs)
    rejected <- NULL
    for (i in 1:ngraphs){
      rejected <- rbind(
        rejected,
        data.frame(
          Name = 1:nrow(EOCtab), Stage = rep(i,nrow(EOCtab)),
          Rejected = as.logical(SeqPlotInput()@graphs[[i]]@nodeAttr$rejected)
        )
      )
    }
    rejected <- rejected %>%
      filter(Rejected) %>%
      group_by(Name) %>%
      summarize(graphRejecting = min(Stage) - 1, .groups = "drop") %>%
      arrange(graphRejecting)

    lastWeights <- as.numeric(SeqPlotInput()@graphs[[ngraphs]]@weights)
    lastGraph <- rep(ngraphs, nrow(EOCtab))

    # We will update for rejected hypotheses with last positive weight for each
    if (ngraphs > 1) {
      for (i in 1:(ngraphs - 1)) {
        lastWeights[rejected$Name[i]] <- as.numeric(SeqPlotInput()@graphs[[i]]@weights[rejected$Name[i]])
        lastGraph[rejected$Name[i]] <- i
      }
    }
    EOCtab$lastAlpha <- FWER * lastWeights
    EOCtab$lastGraph <- lastGraph
    EOCtabx <- EOCtab
    names(EOCtabx) <- c(
      "Name", "Group", "Sequential p",
      "Rejected", "Adjusted p", "Max alpha allocated", "Last Graph")
    output$tmp_seqp <- renderTable({
      EOCtabx %>% select(c(1:3, 6, 4:5, 7))
    })
    seqp <- htmlOutput('tmp_seqp')

    #Bounds at allocated alpha
    bounds<-list()
    for (i in 1:nrow(input$hypothesesMatrix)) {
      ##Get input results
      if (input[[paste0("design_type_", i)]] %in% c("fix")){
        # If not group sequential for this hypothesis, print the max alpha allocated
        # and the nominal p-value
        bounds[[i]]<-tabPanel(title = input$hypothesesMatrix[i,"Name"],
                                                             h3(paste0(input$hypothesesMatrix[i,"Name"],": Maximum alpha allocated: ",EOCtab$lastAlpha[i],", Nominal p-value for hypothesis test: ",input[[paste0("pval_", i)]])))
      } else {
        ##Read in uploaded design
        rds <- input[[paste0("btn_gsdesign_",i)]]
        req(rds)
        design <- readRDS(rds$datapath)
        ##get observed events from input
        obsEvents <- sapply(input[[paste0("pvalMatrix_", i)]][,"ObsEvents"], arithmetic_to_numeric, USE.NAMES = FALSE)
        ##spending time
        if (input$spending_interim == "info"){
          spendingTime <- obsEvents/max(design$gs_object$n.I)
        } else if (input$spending_interim == "pmin"){
          spendingTime <- apply(cbind(obsEvents,design$gs_object$n.I[1:length(obsEvents)]),1,min)/max(design$gs_object$n.I)
        }
        #spendingTime <- ifelse(spendingTime>1,1,spendingTime)

        ##get observed events and pvals from input
        nominalP <- sapply(input[[paste0("pvalMatrix_", i)]][,"ObsPval"], arithmetic_to_numeric, USE.NAMES = FALSE)
        events <- obsEvents
        Analysis <- sapply(input[[paste0("pvalMatrix_", i)]][,"Analysis"], arithmetic_to_numeric, USE.NAMES = FALSE)
        spendingTime <- spendingTime

        d <- design$gs_object

        # Get other info for current hypothesis
        length(events)<-d$k
        length(spendingTime)<-d$k
        ###To work on special situation: if observed event is larger than the future planned events
        n.I <- ifelse(is.na(events),d$n.I,events)
        usTime <- ifelse(is.na(spendingTime),d$timing,spendingTime)
        n.Iplan <- max(d$n.I)
        # If no alpha allocated, just print text line to note this along with the 0 alpha allocated
        if (EOCtab$lastAlpha[i] == 0) {
          bounds[[i]]<-paste0("Maximum alpha allocated: 0. No testing required.")
        }
        if (EOCtab$lastAlpha[i] > 0) {
          dupdate <- gsDesign::gsDesign(
            alpha = EOCtab$lastAlpha[i],
            k = d$k,
            n.I = n.I,
            usTime = usTime,
            maxn.IPlan = n.Iplan,
            n.fix = d$n.fix,
            test.type = d$test.type,
            sfu = d$upper$sf,
            sfupar = d$upper$param
          )
          output[[paste0("tmp_update",i)]]<-renderTable({
            gsDesign::gsBoundSummary(dupdate,#Nname = "Events",
                                     exclude = c(
                                       "B-value", "CP", "CP H1", "Spending",
                                       "~delta at bound", "P(Cross) if delta=0",
                                       "PP", "P(Cross) if delta=1")
            )
          },include.rownames=FALSE)
          bounds[[i]] <- tabPanel(title = input$hypothesesMatrix[i,"Name"],
                                  htmlOutput(paste0("tmp_update",i)))
        }
      }
    }

    switch(input$TestResults,
           "Comparison of sequential p-values to graphs" = seqp,
           "Bounds at final allocated alpha" =do.call(tabsetPanel,bounds)
    )
  }
)

