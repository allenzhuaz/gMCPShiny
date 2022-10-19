# Packages ---------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(markdown)
library(shinyMatrix)
library(ragg)
library(gsDesign)
library(gMCPLite)
library(gMCPShiny)

# Global modules and functions -------------------------------------------------

source("modules/navbar-page-custom.R")
source("modules/output-rcode.R")
source("modules/input-matrix-buttons.R")
source("modules/help-popover.R")
source("modules/input-text-addon.R")
source("modules/sanitize-filename.R")
source("modules/arith2numeric.R")
source("modules/df2graph.R")

# Use `ragg::agg_png()` for plot outputs to make them cross-platform consistent.
# Importantly, this makes ellipse edges look smooth with proper anti-aliasing.
# Note this is experimental in shiny and may be superseded in the future.
# See <https://github.com/rstudio/shiny/blob/main/R/imageutils.R>
options(shiny.useragg = TRUE)

# UI ---------------------------------------------------------------------------

ui <- function(request) {
  source("ui/main.R", local = TRUE)$value
}

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  source("server/main.R", local = TRUE)$value
}

shinyApp(ui, server)
