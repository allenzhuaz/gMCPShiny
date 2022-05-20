# rhandsontable outputs --------------------------------------------------------

# <https://github.com/jrowen/rhandsontable/issues/13>

# Render user inputs for hypotheses
output$hotHypotheses <- renderRHandsontable({
  DF <- dataHypotheses()
  rhandsontable(DF, stretchH = "all", useTypes = FALSE, selectCallback = TRUE)
})
outputOptions(output, "hotHypotheses", suspendWhenHidden = FALSE)

# Render user inputs for hypotheses groups
output$hotGroups <- renderRHandsontable({
  DF <- dataGroups()
  rhandsontable(DF, stretchH = "all", useTypes = FALSE, selectCallback = TRUE)
})
outputOptions(output, "hotGroups", suspendWhenHidden = FALSE)

# Render user inputs for transitions
output$hotTransitions <- renderRHandsontable({
  DF <- dataTrans()
  rhandsontable(DF, stretchH = "all", useTypes = FALSE, selectCallback = TRUE)
})
outputOptions(output, "hotTransitions", suspendWhenHidden = FALSE)

output$hotPositions <- renderRHandsontable({ # render user inputs for node positions
  if (is.null(dataPositions$data)) {
    DF <- getPositions()
  } else {
    DF <- dataPositions$data
  }
  rhandsontable(DF, stretchH = "all", useTypes = FALSE, selectCallback = TRUE)
})
outputOptions(output, "hotPositions", suspendWhenHidden = FALSE)

# Color Picker Inputs; based on selected color palette -------------------------

output$colorSet <- renderUI({
  cbPalette <- c(
    "#e5e5e5", # Grey
    "#E69F00", # Orange
    "#56B4E9", # Light Blue
    "#009E73", # Green
    "#F0E442", # Yellow
    "#0072B2", # Blue
    "#D55E00", # Burnt Orange
    "#CC79A7" # Purple
  )

  greyScalePalette <- c(
    "#eeeeee",
    "#dddddd",
    "#cccccc",
    "#bbbbbb",
    "#a3a3a3"
  )

  dark2 <- c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  )

  if (is.null(input$palette)) {
    print("No Value Selected for Color Palette")
  } else {
    groupNames <- dataGroups()$GroupName
    if (input$palette == "All Colors") { # if palette selected is "All Colors"
      list(
        lapply(1:nrow(dataGroups()), function(x) {
          list(
            colourInput(
              # Create colorpicker inputs for all groups. Names of inputs are input$col1, input$col2, etc.
              paste0("col", x),
              paste0("Select color for ", groupNames[x], ":"),
              "#0F0303",
              showColour = "text"
            )
          )
        })
      )
    } # end if

    else { # else palette selected is not "All Colors"
      palette <- switch(input$palette,
        "Color Blind" = cbPalette,
        "Greyscale" = greyScalePalette,
        "Dark2" = dark2
      )

      list(
        lapply(1:nrow(dataGroups()), function(x) {
          list(
            colourInput(
              # Create colorpicker inputs for all groups with options limited by palette. Names of inputs are input$col1, input$col2, etc.
              paste0("col", x),
              paste0("Select color for ", groupNames[x], ":"),
              palette[x],
              palette = "limited",
              allowedCols = palette,
              showColour = "text"
            )
          )
        })
      )
    } # end else (not all colors)
  } # end else (palette value exists)
})
outputOptions(output, "colorSet", suspendWhenHidden = FALSE) # colorSet runs even when not visible so that colors update when nodes are added

# rhandsontable Dependencies ---------------------------------------------------
dataHypotheses <- reactive({
  input$update
  loadFlag()
  isolate({
    if (is.null(input$hotHypotheses)) {
      DF <- data.frame(
        Name = c("H1", "H2", "H3", "H4"),
        Alpha = c(0.00625, 0.00625, 0.00625, 0.00625),
        Group = c("A", "B", "C", "D")
      )
    } else {
      DF <- hot_to_r(input$hotHypotheses)
    }
    DF
  })
})

dataGroups <- reactive({
  input$update
  loadFlag()

  isolate({
    if (is.null(input$hotGroups)) {
      DF <- data.frame(
        Group = c("A", "B", "C", "D"),
        GroupName = c("Group A", "Group B", "Group C", "Group D")
      )
    } else {
      DF <- hot_to_r(input$hotGroups)
    }
    DF
  })
})

dataColorsDF <- reactive({
  DFColors <- dataGroups()
  colorsExist <- eval(parse(text = paste0("input$col", nrow(dataGroups()))))
  if (input$chkAddColors == FALSE | is.null(colorsExist)) { # if either Add Colors checkbox is false or color inputs don't exist, use light grey for all hypotheses
    DFColors$cols <- rep("#DCDCDC", nrow(dataGroups()))
  } else { # otherwise, add a column for the desired color per group
    DFColors$cols <- sapply(1:nrow(dataGroups()), function(x) eval(parse(text = paste0("input$col", x))))
  }
  DFColors
})

dataTrans <- reactive({
  input$updateEdges
  loadFlag()

  isolate({
    if (is.null(input$hotTransitions)) {
      DF <- data.frame(
        From = c(1, 2, 3, 4),
        To = c(2, 3, 4, 1),
        Weight = rep(1, 4)
      )
    } else {
      DF <- hot_to_r(input$hotTransitions)
    }
    DF
  })
})

# rhandsontable dependencies for node positioning ------------------------------

dataPositions <- reactiveValues()

observeEvent(input$update, {
  dataPositions$data <- getPositions()
})

observeEvent(input$rotation, {
  dataPositions$data <- getRotatedPositions()
})

observeEvent(c(input$updatePositions, loadFlag()), {
  if (is.null(input$hotPositions)) {
    DF <- getPositions()
  } else {
    DF <- hot_to_r(input$hotPositions)
  }
  dataPositions$data <- DF
})

xInputs <- reactive({
  dataPositions$data$X
})

yInputs <- reactive({
  dataPositions$data$Y
})

# Get new positions after updating the hypotheses or if the hotPositions input doesn't exist yet.
getPositions <- function() {
  # Keep custom hotPositions inputs
  if (input$chkCustomPositions & !is.null(input$hotPositions)) {
    positions <- getCustomPositions()
  } else {
    positions <- getDefaultPositions()
  }
  return(positions)
}

# Get new positions after updating the rotation
getRotatedPositions <- function() {
  if (input$chkCustomPositions & !is.null(input$hotPositions)) { # keep custom hotPositions inputs (rotate so current spacing is preserved) - this math needs work (sorry, folks!)
    positions <- getCustomPositions()
    currentAngles <- atan2(0, 0) - atan2(positions$X, positions$Y)
    newAngles <- (currentAngles + (pi * (input$rotation + 1))) %% (2 * pi)
    radius <- 2
    positions$X <- radius * cos(newAngles)
    positions$Y <- radius * sin(newAngles)
  } else {
    positions <- getDefaultPositions()
  }
  return(positions)
}

# Get the user input hotPositions, and add positons for new hypotheses at (0,0)
getCustomPositions <- function() {
  positions <- hot_to_r(input$hotPositions)

  # Keep only the rows that contain info for existing hypotheses
  names <- dataHypotheses()$Name
  keepPositionRows <- (rownames(positions) %in% names)
  positions <- positions[keepPositionRows, ]

  # Add rows for new hypotheses, position at (0,0)
  newNames <- subset(names, !names %in% rownames(positions))
  for (name in newNames) {
    positions <- rbind(positions, "temp" = c(0, 0))
    rownames(positions)[rownames(positions) == "temp"] <- name
  }
  return(positions)
}

# Get default positions that are evenly spaced in a circle
getDefaultPositions <- function() {
  numRows <- nrow(dataHypotheses())
  radianStart <- if ((numRows) %% 2 != 0) {
    pi * (input$rotation + 1) * (1 / 2 + 1 / numRows)
  } else {
    pi * (input$rotation + 1) * (1 + 2 / numRows) / 2
  }
  radian <- (radianStart - (0:(numRows - 1)) / numRows * 2 * pi) %% (2 * pi)
  radius <- 2
  radius2 <- radius

  xPosition <- radius * cos(radian)
  yPosition <- radius * sin(radian)
  positions <- data.frame("X" = xPosition, "Y" = yPosition)
  rownames(positions) <- dataHypotheses()$Name
  return(positions)
}

# Get legend title -------------------------------------------------------------

getLegendTitle <- function() {
  if (input$chkLegend) {
    input$txtLegendName
  } else {
    NULL
  }
}

# Create plot ------------------------------------------------------------------

plotInput <- reactive({
  DFHypotheses <- dataHypotheses()

  DFColors <- dataColorsDF()

  DFAll <- left_join(DFHypotheses, DFColors, by = "Group")

  # Remove invalid transitions from input
  keepTransRows <- (dataTrans()$From %in% c(1:nrow(DFAll))) & (dataTrans()$To %in% c(1:nrow(DFAll)))
  transitions <- dataTrans()[keepTransRows, ]

  m <- df2graph(transitions)

  # Create a named vector for group colors
  groupColors <- DFColors$cols
  names(groupColors) <- DFColors$GroupName

  # Call hGraph()
  hGraph(
    nHypotheses = nrow(DFAll), # Number of Hypotheses, determines the number of circles to draw

    # ELLIPSES FORMATTING AND TEXT
    nodeTextTop = DFAll$Name,
    nodeTextBottom = DFAll$Alpha,
    halfHgt = input$height,
    halfWid = input$width,
    size = input$size,
    digits = input$digits, # Number of decimal digits to show for alpha - not used right now?

    # TRANSITION FORMATTING AND TEXT
    m = m, # Transition df
    boxtextsize = input$boxtextsize, # Transition Text Size
    trhw = input$trhw, # Transition Box Width
    trhh = input$trhh, # Transition Box Height
    trdigits = input$trdigits, # Number of decimal digits to show for transition digit arrow boxes
    trprop = input$trprop, # Transition positioning
    arrowsize = input$arrowsize, # Arrow Size
    offset = input$offset,
    # ELLIPSES PLACEMENT
    x = xInputs(),
    y = yInputs(),

    # COLOR AND LEGEND
    groupNames = as.character(DFAll$GroupName), # groupNames used to determine how the ellipses will be filled
    ellipsesColors = groupColors, # colors for each groupName
    legend = input$chkLegend,
    legendTitle = getLegendTitle(),
    legendtextsize = input$legendtextsize,
    legend.position = input$legend.position
  )
})

output$thePlot <- renderPlot({
  print(parseQueryString(session$clientData$url_search))
  print(plotInput())
})
outputOptions(output, "thePlot", suspendWhenHidden = FALSE) # thePlot runs even when not visible

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

output$changingCode <- renderPrint({
  hGraphCall <- paste0("h <- hGraph(\n", getHGraphArgs(), ")\n\nplot(h)\n", collapse = "")
  session$userData$changingCode <- hGraphCall # save in the session to use for code file download
  cat(hGraphCall)
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
