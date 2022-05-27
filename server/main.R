observeEvent(input$btn_hotHypotheses_addrow, {
  updateMatrixInput(session, inputId = "hotHypotheses", value = addMatrixRow(input$hotHypotheses))
})

observeEvent(input$btn_hotHypotheses_delrow, {
  updateMatrixInput(session, inputId = "hotHypotheses", value = delMatrixRow(input$hotHypotheses))
})

observeEvent(input$btn_hotHypotheses_reset, {
  updateMatrixInput(session, inputId = "hotHypotheses", value = as.matrix(data.frame(cbind(Name = c("H1","H2","H3","H4"), Alpha = rep(0.025/4,4), Group = c("A","B","C","D")))))
})


observeEvent(input$btn_hotTransitions_addrow, {
  updateMatrixInput(session, inputId = "hotTransitions", value = addMatrixRow(input$hotTransitions))
})

observeEvent(input$btn_hotTransitions_delrow, {
  updateMatrixInput(session, inputId = "hotTransitions", value = delMatrixRow(input$hotTransitions))
})

observeEvent(input$btn_hotTransitions_reset, {
  updateMatrixInput(session, inputId = "hotTransitions", value = as.matrix(data.frame(cbind(From = c("H1","H2","H3","H4"), To = c("H2","H3","H4","H1"), Weight = c(1,1,1,1)))))
})


observeEvent(input$btn_hotPositions_addrow, {
  updateMatrixInput(session, inputId = "hotPositions", value = addMatrixRow(input$hotPositions))
})

observeEvent(input$btn_hotPositions_delrow, {
  updateMatrixInput(session, inputId = "hotPositions", value = delMatrixRow(input$hotPositions))
})

observeEvent(input$btn_hotPositions_reset, {
  updateMatrixInput(session, inputId = "hotPositions", value = as.matrix(data.frame(cbind(Name = c("H1","H2","H3","H4"), X = c(-1.414214,1.414214,1.414214,-1.414214), Y = c(1.414214,1.414214,-1.414214,-1.414214)))))
})

#-----Color Picker Inputs; based on selected color palette-----

output$colorSet <- renderUI({

  cbPalette <- c("#e5e5e5", # Grey
                 "#E69F00", # Orange
                 "#56B4E9", # Light Blue
                 "#009E73", # Green
                 "#F0E442", # Yellow
                 "#0072B2", # Blue
                 "#D55E00", # Burnt Orange
                 "#CC79A7") # Purple

  greyScalePalette <- c("#eeeeee",
                        "#dddddd",
                        "#cccccc",
                        "#bbbbbb",
                        "#a3a3a3")

  dark2 <- c("#1B9E77",
             "#D95F02",
             "#7570B3",
             "#E7298A",
             "#66A61E",
             "#E6AB02",
             "#A6761D",
             "#666666")

  if (is.null(input$palette)){
    print("No Value Selected for Color Palette")
  }
  else{
    groupNames = input$hotHypotheses[,1]
    nrows = nrow(input$hotHypotheses[complete.cases(input$hotHypotheses),])
    if (input$palette == "All Colors") { # if palette selected is "All Colors"
      list(
        lapply(1:nrows, function(x) {
          list(
            colourInput(paste0("col", x), # create colorpicker inputs for all groups. Names of inputs are input$col1, input$col2, etc.
                        paste0("Select color for ", groupNames[x], ":"),
                        "#0F0303",
                        showColour = "text")
          )
        }
        )
      )
    } # end if

    else{ # else palette selected is not "All Colors"
      palette <- switch(input$palette,
                        "Color Blind" = cbPalette,
                        "Greyscale" = greyScalePalette,
                        "Dark2" = dark2
      )

      list(
        lapply(1:nrows, function(x) {
          list(
            colourInput(paste0("col", x), # create colorpicker inputs for all groups with options limited by palette. Names of inputs are input$col1, input$col2, etc.
                        paste0("Select color for ", groupNames[x], ":"),
                        palette[x],
                        palette = "limited",
                        allowedCols = palette,
                        showColour = "text")
          )
        })
      )

    } # end else (not all colors)
  } # end else (palette value exists)

})
outputOptions(output, "colorSet", suspendWhenHidden = FALSE) # colorSet runs even when not visible so that colors update when nodes are added


dataColorsDF <- reactive({
  DFColors = data.frame(input$hotHypotheses)
  nrows = nrow(DFColors[complete.cases(DFColors),])
  colorsExist <- eval(parse(text = paste0("input$col", nrows)))
  if(input$chkAddColors == FALSE | is.null(colorsExist)){ # if either Add Colors checkbox is false or color inputs don't exist, use light grey for all hypotheses
    DFColors$cols <- rep("#DCDCDC", nrows)
  }
  else{ # otherwise, add a column for the desired color per group
    DFColors$cols <- sapply(1:nrows, function(x) eval(parse(text = paste0("input$col", x))))
  }
  DFColors
})


#-----Node Positioning-----

dataPositions <- reactiveValues()

observeEvent(input$update,{
  dataPositions$data <- getPositions()
})

observeEvent(input$rotation,{
  dataPositions$data <- getRotatedPositions()
})

observeEvent(c(input$updatePositions, loadFlag()), {
  if (is.null(input$hotPositions)){
    DF = getPositions()
  }
  else{
    DF = data.frame(input$hotPositions)
  }
  dataPositions$data <- DF
})

xInputs <- reactive({
  as.numeric(dataPositions$data$X)
})

yInputs <- reactive({
  as.numeric(dataPositions$data$Y)
})


# Get new positions after updating the hypotheses or if the hotPositions input doesn't exist yet.
getPositions <- function(){
  if(input$chkCustomPositions & !is.null(input$hotPositions)){ # keep custom hotPositions inputs
    positions <- getCustomPositions()
  }
  else{
    positions <- getDefaultPositions()
  }
  return(positions)
}

# Get new positions after updating the rotation
getRotatedPositions <- function(){
  if(input$chkCustomPositions & !is.null(input$hotPositions)){ # keep custom hotPositions inputs (rotate so current spacing is preserved) - this math needs work (sorry, folks!)
    positions <- getCustomPositions()
    currentAngles <- atan2(0,0) - atan2(positions$X, positions$Y)
    newAngles <- (currentAngles + (pi*(input$rotation + 1)))%% (2* pi)
    radius <- 2
    positions$X<-radius*cos(newAngles)
    positions$Y<-radius*sin(newAngles)
  }
  else{
    positions <- getDefaultPositions()
  }
  return(positions)
}

# Get the user input hotPositions, and add positions for new hypotheses at (0,0)
getCustomPositions <- function(){
  positions <- input$hotPositions

  # keep only the rows that contain info for existing hypotheses
  names <-input$hotHypotheses[,1]
  keepPositionRows <- positions[,1] %in% names
  positions <- positions[keepPositionRows,]

  # add rows for new hypotheses, position at (0,0)
  newNames <- subset(names, !names %in% positions[,1])
  for(name in newNames){
    positions <- rbind(positions, "temp" = c(0, 0))
    positions[,1][positions[,1] == "temp"] <- name
  }
  return(positions)
}

# Get default positions that are evenly spaced in a circle
getDefaultPositions <- function(){
  numRows <- nrow(input$hotHypotheses[complete.cases(input$hotHypotheses),])
  radianStart <- if((numRows)%%2!=0){pi*(input$rotation +1)*(1/2+1/numRows)}else{
    pi * (input$rotation +1) * (1 + 2 / numRows) / 2}
  radian <- (radianStart - (0:(numRows-1))/numRows*2*pi) %% (2*pi)
  radius = 2
  radius2 = radius

  xPosition=radius*cos(radian)
  yPosition=radius*sin(radian)
  positions <- data.frame("X" = xPosition, "Y" = yPosition)
  rownames(positions) <- input$hotHypotheses[,1]
  return(positions)
}

#-----Get legend title-----

getLegendTitle <- function() {
  if (input$chkLegend) {
    input$txtLegendName
  } else {
    NULL
  }
}


#-----Create plot-----

plotInput <- reactive({

  DFHypotheses <- data.frame(input$hotHypotheses)

  DFColors <- dataColorsDF()

  DFAll <- left_join(DFHypotheses, DFColors, by = "Name")

  # remove invalid transitions from input
  Trans <- data.frame(input$hotTransitions)
  keepTransRows <-  (Trans[,1] %in% DFAll[,1]) & (Trans[,2] %in% DFAll[,1])
  #transitions <- data.frame(From=as.integer(Trans[keepTransRows,1]),To=as.integer(Trans[keepTransRows,2]),Weight=as.integer(Trans[keepTransRows,3]))
  transitions <- Trans[keepTransRows,]

  m <- df2graph(namesH=DFAll[,1], df=transitions)

  # create a named vector for group colors
  groupColors <- DFColors$cols
  names(groupColors) <- DFColors$Group

  # call hGraph()
  hGraph(
    nHypotheses     = nrow(DFAll), # Number of Hypotheses, determines the number of circles to draw

    # ELLIPSES FORMATTING AND TEXT
    nodeTextTop     = as.character(DFAll[,1]),
    nodeTextBottom  = as.numeric(DFAll[,2]),
    halfHgt         = input$height,
    halfWid         = input$width,
    size            = input$size,
    digits          = input$digits,      # Number of decimal digits to show for alpha - not used right now?

    # TRANSITION FORMATTING AND TEXT
    m               = m,                 # Transition df
    boxtextsize     = input$boxtextsize, # Transition Text Size
    trhw            = input$trhw,        # Transition Box Width
    trhh            = input$trhh,        # Transition Box Height
    trdigits        = input$trdigits,    # Number of decimal digits to show for transition digit arrow boxes
    trprop          = input$trprop,      # Transition positioning
    arrowsize       = input$arrowsize,   # Arrow Size
    offset = input$offset,

    # ELLIPSES PLACEMENT
    x               = xInputs(),
    y               = yInputs(),

    # COLOR AND LEGEND
    groupNames          = as.character(DFAll[,3]), # groupNames used to determine how the ellipses will be filled
    ellipsesColors  = groupColors,                   # colors for each groupName
    legend          = input$chkLegend,
    legendTitle     = getLegendTitle(),
    legendtextsize  = input$legendtextsize
  )

})


output$thePlot <- renderPlot({

  print(parseQueryString(session$clientData$url_search))
  print(plotInput())

})
outputOptions(output, "thePlot", suspendWhenHidden = FALSE) # thePlot runs even when not visible



#-----Output for graph code that doesn't change based on user inputs-----

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

getCode <- function(name, closure){
  code <- capture.output(closure)
  if(startsWith(code[length(code)], "<bytecode:")){
    code[length(code)] <- "\n" # replace last line with a newline if the lastline is <bytecode: something> (not part of the function code)
  }
  code[1] <- paste0(name, " <- ", code[1], "\n")
  return(code)
}

#-----Output for graph code that changes based on user inputs-----
changingCode <- reactive({
  report <- tempfile(fileext = ".R")
  brew::brew("templates/call-hgraph.R", output = report)
  paste0(readLines(report), collapse = "\n")
})

output$changingCode <- renderPrint({
  cat(changingCode(),sep="\n")
})


#-----Download graph code-----

output$downloadCode <- downloadHandler(
  filename = "hGraphCode.R",
  content = function(file) {
    write(session$userData$fixedCode, file)
    write(session$userData$changingCode, file, append=TRUE)
  }
)

#-----Save and load graph inputs-----
savedFields <- c("hotHypotheses", "hotTransitions", "hotPositions")
output$save_inputs <- downloadHandler(
  filename = "table_inputs.rda",
  content = function(file) {

    data <- sapply(savedFields, function(x) data.frame(eval(parse(text=paste0("input$",savedFields[x])))
    ))
    save(data, file = file)

  })

loadFlag <- reactiveVal(value = FALSE) # value of flag (T/F) doesn't matter, just need it to change

observeEvent(input$load_inputs,{
  inFile <- input$load_inputs
  if(is.null(inFile)){
    return(NULL)
  }

  # load hot table data into temporary environment and list
  e1 = new.env()
  invisible(load(inFile$datapath, envir = e1))
  tmp = as.list(e1)$data

})


#-----Unused code-----

observeEvent(input$submit, {
  output$thePlot <- renderPlot({

    input$update

    print(plotInput())

  })
})


### DEBUG
output$debug <- renderUI({

  DF <- data.frame(input$hotHypotheses)

  nrows = nrow(DF[complete.cases(DF),])

  str1 <- paste("Length: ", nrows)

  str2 <- paste("dat()[2,2]: ", DF[2,2])

  #DFGroups <- dataGroups()
  #str3 <- paste("DFGroups[,2]: ", as.character(DFGroups[,2]))

  HTML(paste(str1, str2, sep = '<br/>'))

  #str(input$hot_select$select)

})


# output$table <- renderTable({dataHypotheses()})

# Download the plot
output$downloadPlot <- downloadHandler(
  filename = 'myhGraph.png',
  content = function(file) {
    # device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = plotInput())
  }
)