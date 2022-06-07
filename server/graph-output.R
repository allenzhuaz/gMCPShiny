# rhandsontable outputs --------------------------------------------------------

# <https://github.com/jrowen/rhandsontable/issues/13>

# Render user inputs for hypotheses


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
    groupNames <- input$groupMatrix[,"GroupName"]
    if (input$palette == "All Colors") { # if palette selected is "All Colors"
      list(
        lapply(1:nrow(input$groupMatrix), function(x) {
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
        lapply(1:nrow(input$groupMatrix), function(x) {
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
                  "The hypothesis input uses the plotmath syntax. See ?grDevices::plotmath for details. Use \\n to add a line break.
                            The x, y coordinates are for the relative position of the hypothesis ellipses."
                )
              ),
              value = as.matrix(data.frame(cbind(Hypothesis = input$hypothesesMatrix[,"Name"],
                                                 x = 2 * cos((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi))
                                                 ,
                                                 y = 2 * sin((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi))
              ))),
              class = "character",
              rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
              cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
  )
})

dataColorsDF <- reactive({
  DFColors <- input$groupMatrix
  colorsExist <- eval(parse(text = paste0("input$col", nrow(input$groupMatrix))))
  if (input$chkAddColors == FALSE | is.null(colorsExist)) { # if either Add Colors checkbox is false or color inputs don't exist, use light grey for all hypotheses
    DFColors$cols <- rep("#DCDCDC", nrow(input$groupMatrix))
  } else { # otherwise, add a column for the desired color per group
    DFColors$cols <- sapply(1:nrow(input$groupMatrix), function(x) eval(parse(text = paste0("input$col", x))))
  }
  DFColors
})




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
#  DFHypotheses <- dataHypotheses()

#  DFColors <- dataColorsDF()

#  DFAll <- left_join(DFHypotheses, DFColors, by = "Group")

  # Remove invalid transitions from input
#  keepTransRows <- (dataTrans()$From %in% c(1:nrow(DFAll))) & (dataTrans()$To %in% c(1:nrow(DFAll)))
#  transitions <- dataTrans()[keepTransRows, ]
  Trans <- data.frame(input$trwtMatrix)
  keepTransRows <-  (Trans[,1] %in% input$hypothesesMatrix[,1]) & (Trans[,2] %in% input$hypothesesMatrix[,1])
  transitions <- Trans[keepTransRows,]

  m <- df2graph(namesH = unique(input$hypothesesMatrix[,1]), df = transitions)

  # Create a named vector for group colors
#  groupColors <- DFColors$cols
#  names(groupColors) <- DFColors$GroupName

  # Call hGraph()
  # hGraph(
  #   nHypotheses = nrow(input$hypothesesMatrix), # Number of Hypotheses, determines the number of circles to draw
  #
  #   # ELLIPSES FORMATTING AND TEXT
  #   nodeTextTop = input$hypothesesMatrix[,"Name"],
  #   nodeTextBottom = input$hypothesesMatrix[,"Alpha"],
  #   halfHgt = input$height,
  #   halfWid = input$width,
  #   size = input$size,
  #   digits = input$digits, # Number of decimal digits to show for alpha - not used right now?
  #
  #   # TRANSITION FORMATTING AND TEXT
  #   m = m, # Transition df
  #   boxtextsize = input$boxtextsize, # Transition Text Size
  #   trhw = input$trhw, # Transition Box Width
  #   trhh = input$trhh, # Transition Box Height
  #   trdigits = input$trdigits, # Number of decimal digits to show for transition digit arrow boxes
  #   trprop = input$trprop, # Transition positioning
  #   arrowsize = input$arrowsize, # Arrow Size
  #   offset = input$offset,
  #   # ELLIPSES PLACEMENT
  #   x = xInputs(),
  #   y = yInputs(),
  #
  #   # COLOR AND LEGEND
  #   groupNames = input$groupMatrix[,"GroupName"], # groupNames used to determine how the ellipses will be filled
  #   ellipsesColors = groupColors, # colors for each groupName
  #   legend = input$chkLegend,
  #   legendTitle = getLegendTitle(),
  #   legendtextsize = input$legendtextsize,
  #   legend.position = input$legend.position
  # )

  gsDesign::hGraph(
    nHypotheses = nrow(input$hypothesesMatrix),
    nameHypotheses = input$hypothesesMatrix[,"Name"],
    alphaHypotheses = as.numeric(input$hypothesesMatrix[,"Alpha"]),
    m = m,
    fill = input$hypothesesMatrix[,"Group"],
    palette = unname(palette.colors(n = length(unique(input$hypothesesMatrix[,"Group"])), palette = input$palette, recycle = TRUE)),
    labels = input$hypothesesMatrix[,"Group"],
    legend.name = getLegendTitle(),
    legend.position = "bottom",
    halfWid = input$height,
    halfHgt = input$width,
    trhw = input$trhw,
    trhh = input$trhh,
    trprop = input$trprop,
    digits = input$digits,
    trdigits = input$trdigits,
    size = input$size,
    boxtextsize = input$boxtextsize,
    arrowsize = input$arrowsize,
    offset = input$offset,
    x = if(is.null(input$nodeposMatrix[,"x"])){NULL} else{as.numeric(input$nodeposMatrix[,"x"])},
    y = if(is.null(input$nodeposMatrix[,"y"])){NULL} else{as.numeric(input$nodeposMatrix[,"y"])},
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
