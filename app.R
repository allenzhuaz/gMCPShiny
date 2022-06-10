# Packages ---------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(markdown)
library(gMCPmini)
library(shinyMatrix)

# Global modules and functions -------------------------------------------------

source("modules/navbar-page-custom.R")
source("modules/heading-panel.R")
source("modules/output-rcode.R")
source("modules/input-matrix-buttons.R")
source("modules/help-popover.R")
source("modules/input-file-button.R")
source("modules/input-text-addon.R")
source("modules/sanitize-filename.R")
source("modules/hgraph.R")
source("modules/df2graph.R")
source("modules/adaptive-palette.R")


# UI ---------------------------------------------------------------------------

ui <- function(request) {
  source("ui/main.R", local = TRUE)$value
}

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  source("server/main.R", local = TRUE)$value
}

shinyApp(ui, server)
