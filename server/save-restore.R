
# Output for graph code that doesn't change based on user inputs ---------------

output$fixedCode <- renderPrint({
  librariesCode <- "library(dplyr)\nlibrary(reshape2)\nlibrary(ggplot2)\nlibrary(stringr)\n"
  plainToPlotCode <- getCode(deparse(substitute(plainToPlot)), plainToPlot)
  theme_nothingCode <- getCode(deparse(substitute(theme_nothing)), theme_nothing)
  radiansCode <- getCode(deparse(substitute(radians)), radians)
  hGraphCode <- getCode(deparse(substitute(hGraph)), hGraph)
  fixedCode <- c(librariesCode, plainToPlotCode, theme_nothingCode, radiansCode, hGraphCode)
  fixedCode <- paste0(fixedCode, collapse = "\n")
  session$userData$fixedCode <- fixedCode # save in the session to use for code file download
  cat(fixedCode)
})

getCode <- function(name, closure) {
  code <- capture.output(closure)
  if (startsWith(code[length(code)], "<bytecode:")) {
    code[length(code)] <- "\n" # replace last line with a newline if the lastline is <bytecode: something> (not part of the function code)
  }
  code[1] <- paste0(name, " <- ", code[1], "\n")
  return(code)
}

# Output for graph code that changes based on user inputs ----------------------

changingCode <- reactive({
  report <- tempfile(fileext = ".R")
  brew::brew("templates/call-hgraph.R", output = report)
  paste0(readLines(report), collapse = "\n")
})

output$changingCode <- renderPrint({
  cat(changingCode(),sep="\n")
})

# Get the latest input args to the hGraph() function as a string for the R code to reproduce the graph output
getHGraphArgs <- reactive({
  input$updateCode
  envList <- latestCall
  argStr <- "\t"
  nameVector <- names(envList)
  len <- length(nameVector)
  for (i in 1:len) {
    comma <- ",\n\t"
    if (i == len) {
      comma <- "\n"
    }
    x <- nameVector[i]
    value <- paste0(capture.output(dput(envList[[x]])), collapse = "")
    argStr <- paste0(argStr, x, " = ", value, comma, collapse = "")
  }

  return(argStr)
})

# Download graph code ----------------------------------------------------------

output$downloadCode <- downloadHandler(
  filename = "hGraphCode.R",
  content = function(file) {
    write(session$userData$fixedCode, file)
    write(session$userData$changingCode, file, append = TRUE)
  }
)

# Save and load graph inputs ---------------------------------------------------

savedFields <- c("hotHypotheses", "hotGroups", "hotTransitions", "hotPositions")
output$save_inputs <- downloadHandler(
  filename = "table_inputs.rda",
  content = function(file) {
    # inputList <- reactiveValuesToList(input)
    # print(inputList)
    # saveRDS(reactiveValuesToList(input) , file = file)

    data <- sapply(savedFields, function(x) hot_to_r(input[[x]]))
    save(data, file = file)
  }
)

loadFlag <- reactiveVal(value = FALSE) # value of flag (T/F) doesn't matter, just need it to change

observeEvent(input$load_inputs, {
  inFile <- input$load_inputs
  if (is.null(inFile)) {
    return(NULL)
  }
  # savedInputs <- readRDS(inFile$datapath)
  # print(savedInputs)
  # inputIDs <- names(savedInputs)
  # inputValues <- unlist(savedInputs)
  # for(i in 1:length(savedInputs)){
  #   session$sendInputMessage(inputIDs[i], list(value=inputValues[[i]]))
  # }

  # load hot table data into temporary environment and list
  e1 <- new.env()
  invisible(load(inFile$datapath, envir = e1))
  tmp <- as.list(e1)$data

  # update rhandsontables
  lapply(savedFields, function(x) {
    output[[x]] <- renderRHandsontable({
      DF <- tmp[[x]]
      rhandsontable(DF, stretchH = "all", useTypes = FALSE, selectCallback = TRUE)
    })
  })

  loadFlag(!loadFlag()) # flag intended to trigger changes in the graph by bypassing user button clicks to "update" the tables. Does not work yet, sorry!
})

