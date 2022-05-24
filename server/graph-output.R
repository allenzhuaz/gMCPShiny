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
