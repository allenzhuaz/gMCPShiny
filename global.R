library(shiny)
library(rhandsontable)
library(colourpicker)
source("hGraph.r")

enableBookmarking(store = "url")
#shinyApp(ui, server)
# Bring in the custom navbar
navbar <- readLines("www/navbar.html")

dfToGraph <- function(df) {
  
  # Check to make sure the column names are present
  if (!any(names(df) %in% c("From", "To", "Weight"))) {
    print("You need to have three columns in your input dataframe: 'From', 'To', and 'Weight'")
  }
  
  maxUnNodes <- max(unique(c(df$From, df$To)))
  m <- matrix(rep(0, maxUnNodes^2), nrow = maxUnNodes)
  m[df$From+maxUnNodes*(df$To-1)] <- df$Weight # From = rows, and To = columns
  
  return(m)
  
}

