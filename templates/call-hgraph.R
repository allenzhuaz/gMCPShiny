library(gMCPmini)

h <- hGraph(
  nHypotheses     = <%=nrow(input$hypothesesMatrix)%>,
  nameHypotheses  = c(<%=paste0("\"", paste0(input$hypothesesMatrix[,"Name"], collapse = "\", \""), "\"")%>),
  alphaHypotheses = c(<%=paste0(as.numeric(input$hypothesesMatrix[,"Alpha"]), collapse = ", ")%>),
  m               = matrix(c(<%=paste0(c(df2graph(namesH = unique(input$hypothesesMatrix[,1]), df = data.frame(input$trwtMatrix)[(data.frame(input$trwtMatrix)[,1] %in% input$hypothesesMatrix[,1]) & (data.frame(input$trwtMatrix)[,2] %in% input$hypothesesMatrix[,1]),])), collapse = ", ")%>), nrow = <%=nrow(input$hypothesesMatrix)%>),
  fill            = c(<%=paste0("\"", paste0(input$hypothesesMatrix[,"Group"], collapse = "\", \""), "\"")%>),
  palette         = c(<%=paste0("\"", paste0(hgraph_palette(pal_name = input$pal_name, n = length(unique(input$hypothesesMatrix[,"Group"])), alpha = input$pal_alpha), collapse = "\", \""), "\"")%>),
  labels          = c(<%=paste0("\"", paste0(input$hypothesesMatrix[,"Group"], collapse = "\", \""), "\"")%>),
  legend.name     = <%=paste0("\"", input$legend.name, "\"")%>,
  legend.position = <%=paste0("\"", input$legendPosition, "\"")%>,
  halfHgt         = <%=input$height%>,
  halfWid         = <%=input$width%>,
  trhw            = <%=input$trhw%>,
  trhh            = <%=input$trhh%>,
  trprop          = <%=input$trprop%>,
  digits          = <%=input$digits%>,
  trdigits        = <%=input$trdigits%>,
  size            = <%=input$size%>,
  boxtextsize     = <%=input$boxtextsize%>,
  legend.textsize = <%=input$legend.textsize%>,
  arrowsize       = <%=input$arrowsize%>,
  offset          = <%=input$offset%>,
  x               = <%=if(is.null(input$nodeposMatrix[,"x"]) | !setequal(input$nodeposMatrix[,"Hypothesis"], input$hypothesesMatrix[,"Name"])){"NULL"} else{paste0("c(", paste0(as.numeric(input$nodeposMatrix[,"x"]), collapse = ", "), ")")}%>,
  y               = <%=if(is.null(input$nodeposMatrix[,"y"]) | !setequal(input$nodeposMatrix[,"Hypothesis"], input$hypothesesMatrix[,"Name"])){"NULL"} else{paste0("c(", paste0(as.numeric(input$nodeposMatrix[,"y"]), collapse = ", "), ")")}%>,
  wchar           = <%=paste0("\"", input$wchar, "\"")%>
)

plot(h)
