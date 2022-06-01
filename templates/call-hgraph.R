library(gMCPmini)

h <- hGraph(
  nHypotheses     = <%=nrow(input$hypothesesMatrix)%>,
  nameHypotheses  = c(<%=paste0("'", paste0(input$hypothesesMatrix[,"Name"], collapse = "', '"), "'")%>),
  alphaHypotheses = c(<%=paste0(as.numeric(input$hypothesesMatrix[,"Alpha"]), collapse = ", ")%>),
  halfHgt         = <%=input$height%>,
  halfWid         = <%=input$width%>,
  size            = <%=input$size%>,
  digits          = <%=input$digits%>,
  m               = matrix(c(<%=paste0(c(df2graph(namesH = unique(c(input$trwtMatrix[,1:2])), df = data.frame(input$trwtMatrix))), collapse = ", ")%>), nrow = <%=length(unique(c(input$trwtMatrix[,1:2])))%>)
  boxtextsize     = <%=input$boxtextsize%>,
  trhw            = <%=input$trhw%>,
  trhh            = <%=input$trhh%>,
  trdigits        = <%=input$trdigits%>,
  trprop          = <%=input$trprop%>,
  arrowsize       = <%=input$arrowsize%>,
  offset          = <%=input$offset%>,
  groupNames      = c(<%=paste0(input$hypothesesMatrix[,"Group"], collapse = ", ")%>),
  legend          = <%=input$chkLegend%>,
  legendTitle     = <%=getLegendTitle()%>,
  legendtextsize  = <%=input$legendtextsize%>,
  legend.position = <%=paste0('"',input$legend.position,'"')%>
)

plot(h)
