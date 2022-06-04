library(gMCPmini)

h <- hGraph(
  nHypotheses     = <%=nrow(input$hypothesesMatrix)%>,
  nameHypotheses  = c(<%=paste0("\"", paste0(input$hypothesesMatrix[,"Name"], collapse = "\", \""), "\"")%>),
  alphaHypotheses = c(<%=paste0(as.numeric(input$hypothesesMatrix[,"Alpha"]), collapse = ", ")%>),
  halfHgt         = <%=input$height%>,
  halfWid         = <%=input$width%>,
  size            = <%=input$size%>,
  digits          = <%=input$digits%>,
  m               = matrix(c(<%=paste0(c(df2graph(namesH = unique(input$hypothesesMatrix[,1]), df = data.frame(input$trwtMatrix)[(data.frame(input$trwtMatrix)[,1] %in% input$hypothesesMatrix[,1]) & (data.frame(input$trwtMatrix)[,2] %in% input$hypothesesMatrix[,1]),])), collapse = ", ")%>), nrow = <%=nrow(input$hypothesesMatrix)%>),
  boxtextsize     = <%=input$boxtextsize%>,
  trhw            = <%=input$trhw%>,
  trhh            = <%=input$trhh%>,
  trdigits        = <%=input$trdigits%>,
  trprop          = <%=input$trprop%>,
  arrowsize       = <%=input$arrowsize%>,
  offset          = <%=input$offset%>,
  labels          = c(<%=paste0(paste0('"', input$hypothesesMatrix[,"Group"], '"'), collapse = ", ")%>),
  legend.name     = <%=paste0('"',getLegendTitle(), '"')%>,
  legend.textsize = <%=input$legendtextsize%>,
  legend.position = <%=paste0('"',input$legend.position,'"')%>
)

plot(h)
