h <- hGraph(
  nHypotheses     = <%=nrow(DFAll)%>,
  nodeTextTop     = c(<%=paste0(paste0('"', DFAll[,1], '"'), collapse = ", ")%>),
  nodeTextBottom  = c(<%=paste0(as.numeric(DFAll[,2]),collapse = ", ")%>),
  halfHgt         = <%=input$height%>,
  halfWid         = <%=input$width%>,
  size            = <%=input$size%>,
  digits          = <%=input$digits%>,
  m               = matrix(c(<%=paste0(m,collapse = ",")%>),nrow=<%=nrow(DFAll)%>,ncol=<%=nrow(DFAll)%>),
  boxtextsize     = <%=input$boxtextsize%>,
  trhw            = <%=input$trhw%>,
  trhh            = <%=input$trhh%>,
  trdigits        = <%=input$trdigits%>,
  trprop          = <%=input$trprop%>,
  arrowsize       = <%=input$arrowsize%>,
  offset = <%=input$offset%>,
  x               = c(<%=paste0(xInputs(), collapse = ", ")%>),
  y               = c(<%=paste0(yInputs(), collapse = ", ")%>),
  groupNames      = c(<%=paste0(paste0('"', DFAll[,3], '"'), collapse = ", ")%>),
  ellipsesColors  = c(<%=paste0(paste0('"', groupColors, '"'), collapse = ", ")%>),
  legend          = <%=input$chkLegend%>,
  legendTitle     = <%=getLegendTitle()%>,
  legendtextsize  = <%=input$legendtextsize%>,
  legend.position = "bottom"
)

plot(h)
