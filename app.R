# Packages ---------------------------------------------------------------------

library(shiny)
library(rhandsontable)
library(colourpicker)
library(markdown)
library(shinyMatrix)
# Global modules and functions -------------------------------------------------

source("modules/navbar-page-custom.R")
source("modules/heading-panel.R")
source("modules/hgraph.R")
source("modules/df2graph.R")
source("modules/input-matrix-buttons.R")
source("modules/help-popover.R")

# Global declarations ----------------------------------------------------------

enableBookmarking(store = "url")

# UI ---------------------------------------------------------------------------

ui <- function(request) {
  source("ui/main.R", local = TRUE)$value
}

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  source("server/main.R", local = TRUE)$value
}

shinyApp(ui, server)
