# Packages ---------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyMatrix)
library(colourpicker)
library(markdown)
library(gsDesign)
library(shinyMatrix)
library(rhandsontable)
# Global modules and functions -------------------------------------------------

source("modules/navbar-page-custom.R")
source("modules/heading-panel.R")
source("modules/hgraph.R")
source("modules/df2graph.R")
source("modules/input-matrix-buttons.R")
source("modules/help-popover.R")
source("modules/input-file-button.R")
source("modules/input-text-addon.R")
source("modules/sanitize-filename.R")

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
