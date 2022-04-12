library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)


plainToPlot <- function(myPlainText) {
  # Convert NAs to blank spaces
  myPlainText[is.na(myPlainText)] <- ""
  
  # Change the object type
  myPlainText <- as.character(myPlainText)
  
  # Remove unnecessary spaces
  myPlainText <- gsub(" \\\\n ", "\\\\n", myPlainText)
  myPlainText <- gsub(" \\\\n", "\\\\n", myPlainText)
  myPlainText <- gsub("\\\\n ", "\\\\n", myPlainText)
  
  temp <- NULL
  
  for (j in 1:length(myPlainText)) {
    myPlainTextSub <- myPlainText[j]
    # Get Number of line breaks
    lineBreaks <- str_count(myPlainTextSub, pattern = "\\\\n")
    if (lineBreaks == 0) {
      finalExpression <- myPlainTextSub
    } else if (lineBreaks == 1) {
      separateWords <- strsplit(myPlainTextSub, "\\\\n")[[1]]
      finalExpression <- paste0("atop(textstyle(", separateWords[1], "),textstyle(", separateWords[2], "))")
    } else if (lineBreaks > 1) {
      separateWords <- strsplit(myPlainTextSub, "\\\\n")[[1]]
      finalExpression <- paste0("atop(textstyle(", separateWords[1], "),textstyle(", separateWords[2], "))")
      # Initiate loop
      for (i in 2:lineBreaks) {
        finalExpression <- paste0("atop(textstyle(", finalExpression, "),textstyle(", separateWords[i+1], "))")
      }
    }
    temp <- c(temp, finalExpression)
  }
  temp <- gsub(" ", "~", temp)
  return(temp)
}


options(scipen=999)

theme_nothing <- function(base_size = 12, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect             = element_blank(),
      line             = element_blank(),
      text             = element_blank(),
      legend.position  = "none"
    )
}

radians <- function(x,y,xend,yend,halfWid,halfHgt){
  theta <- sign(yend-y)*(abs(xend - x)<.0001) * pi / 2
  indx <- (xend-x>0.00001)
  theta[indx] <- atan((yend[indx]-y[indx])*halfWid/(xend[indx]-x[indx])/halfHgt)
  indx <- ((xend - x)< -0.00001 & (yend-y)>.00001)
  theta[indx] <- pi/2 + atan(-(xend[indx]-x[indx])*halfHgt/(yend[indx]-y[indx])/halfWid)
  indx <- ((xend - x) < -.00001 & (yend - y) < -.00001)
  theta[indx] <- atan((yend[indx]-y[indx]) / halfHgt / (xend[indx] - x[indx]) * halfWid) + pi
  indx <- ((xend -x) < -.00001 & abs(yend - y) < .00001)
  theta[indx] <- pi
  return(theta)
}

latestCall <- "" # global variable needed to store input args for a call to hGraph function


hGraph <- function(nHypotheses = 4, # number of hypotheses
                   
                   # ELLIPSES FORMATTING AND TEXT
                   nodeTextTop = NULL, 
                   nodeTextBottom = NULL,                   
                   halfWid = .5,
                   halfHgt = .5,
                   size = 4,
                   digits = 4,        # Number of decimal digits to show for alpha - not used right now?
                   textcol = "black", # not used right now?
                   textalpha = 1,
                   
                   # TRANSITION FORMATTING AND TEXT
                   m = matrix(array(1/(nHypotheses-1),nHypotheses^2),nrow=nHypotheses) - # Transition df
                     diag(1/(nHypotheses-1),nHypotheses),
                   boxtextsize = 3,   # Transition Text Size
                   trhw = .1,         # Transition Box Width
                   trhh = .075,       # Transition Box Height
                   trdigits = 2,      # Number of decimal digits to show for transition digit arrow boxes
                   trprop = 1/3,      # Transition positioning
                   arrowsize = 0.035, # Arrow Size
                   offset = pi / 4 / nHypotheses,
                   
                   # ELLIPSES PLACEMENT
                   x = NULL,
                   y = NULL,
                   # don't need the below inputs if passing in x and y coordinates
                   radius = 2,
                   radius2 = radius,
                   rotation = 1,
                   radianStart = if((nHypotheses)%%2!=0){pi*rotation*(1/2+1/nHypotheses)}else{
                     pi * rotation * (1 + 2 / nHypotheses) / 2},
                   
                   # COLOR AND LEGEND
                   groupNames = array(1,nHypotheses),                 # list of groupNames for every hypothesis used to determine how the ellipses will be colored
                   ellipsesColors = rep("#DCDCDC", nrow(groupNames)), # colors for each group - prefer named vector with elements, [groupName: colorCode]
                   fillalpha = 1,
                   legendTitle    = NULL,
                   legend = FALSE,
                   legendtextsize = 20
                   
){
  latestCall <<- as.list(environment())
  # compute middle of each rectangle
  radian <- (radianStart - (0:(nHypotheses-1))/nHypotheses*2*pi) %% (2*pi)
  # create data frame with middle (x and y), left and right (l, r: x-coordinates), 
  # top and bottom (t, b: y-coordinates)
  rr <- data.frame(x=radius*cos(radian),y=radius2*sin(radian),
                   # txt=plainToPlot(nodeText), 
                   txt=ifelse(is.na(nodeTextBottom),
                              plainToPlot(nodeTextTop),
                              paste("atop(", plainToPlot(nodeTextTop), ",~alpha ==", plainToPlot(nodeTextBottom), ")", sep="")),
                   #txt=paste("atop(", nameHypotheses, ",~alpha ==", round(alphaHypotheses,digits), ")", sep=""),
                   stringsAsFactors = FALSE)
  
  if (!is.null(x)&& length(x)==nHypotheses) rr$x=x
  if (!is.null(y)&& length(y)==nHypotheses) rr$y=y
  # hack to get ellipses around x,y with radii halfWid and halfHgt
  w <- halfWid/3.1
  h <- halfHgt/3.1
  
  indices <- 1:nHypotheses # make sure each ellipses's points are in its own "group" - different from the hypotheses groups used to color the ellipses (fill)
  ellipses <- rbind(data.frame(x=rr$x,y=rr$y+h,fill=groupNames,fillalpha=fillalpha, group=indices),
                    data.frame(x=rr$x,y=rr$y-h,fill=groupNames,fillalpha=fillalpha, group=indices),
                    data.frame(x=rr$x+w,y=rr$y,fill=groupNames,fillalpha=fillalpha, group=indices),
                    data.frame(x=rr$x-w,y=rr$y,fill=groupNames,fillalpha=fillalpha, group=indices))
  ellipses$txt=""
  
  # create data frame for connecting hypotheses with transition proportions
  gr <- filter(melt(m),value>0)
  rra <- select(rr,x,y)
  rra$Var1 <- 1:dim(rra)[1]
  rrb <- rra
  names(rrb) <- c("xend","yend","Var2")
  transitions <- left_join(gr,rra) %>% left_join(rrb)
  transitions$theta <- with(transitions, radians(x,y,xend,yend,halfWid,halfHgt))
  transitions$x <- transitions$x+halfWid*cos(transitions$theta+offset)
  transitions$xend <- transitions$xend+halfWid*cos(transitions$theta+pi-offset)
  transitions$y <- transitions$y+halfHgt*sin(transitions$theta+offset)
  transitions$yend <- transitions$yend+halfHgt*sin(transitions$theta+pi-offset)
  transitions$txt=""
  # now make a data frame for placement of transition weights
  transitionText <- transitions
  transitionText$x <- (1-trprop)*transitions$x+trprop*transitions$xend
  transitionText$y <- (1-trprop)*transitions$y+trprop*transitions$yend
  transitionText$txt <- as.character(round(transitions$value,trdigits))
  transitionText$xmin <- transitionText$x+trhw
  transitionText$ymin <- transitionText$y+trhh
  transitionText$xmax <- transitionText$x-trhw
  transitionText$ymax <- transitionText$y-trhh
  
  # return plot
  if(length(fillalpha)==1) fillalpha<-array(fillalpha,nHypotheses)
  if(length(textcol)==1) textcol<-array(textcol,nHypotheses)
  x<-ggplot()+
    stat_ellipse(data=ellipses,aes(x=x,y=y,fill=fill,alpha=fillalpha, group=group), 
                 geom="polygon")+
    geom_text(data=rr,aes(x=x,y=y,label=txt,alpha=textalpha), parse=TRUE,size=size)+
    geom_segment(data=transitions,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 arrow = grid::arrow(length = grid::unit(arrowsize, "npc")))+
    geom_rect(data=transitionText,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="white",color="black")+
    geom_text(data=transitionText,aes(x=x,y=y,label=txt),size=boxtextsize) 
  if (legend) {
    x <- x+theme_void() +
          theme(legend.title=element_text(size=legendtextsize),
                legend.text=element_text(size=legendtextsize))
  } else{ 
    x <- x+theme_nothing()
  }
  # ADD COLORS AND LEGEND
  x <- x+ 
    scale_fill_manual(values=ellipsesColors, guide = guide_legend(legendTitle)) + # color based on groups
    scale_alpha(guide="none") +
    scale_size(guide="none") 
  return(x)
}
