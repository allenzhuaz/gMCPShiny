# Packages ---------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(markdown)
library(shinyMatrix)
library(ragg)
library(gsDesign)
library(gMCPLite)
library(gMCPShiny)
library(dplyr)

# Modules and global settings --------------------------------------------------

# Use `ragg::agg_png()` for plot outputs to make them cross-platform consistent.
# Importantly, this makes ellipse edges look smooth with proper anti-aliasing.
# Note this is experimental in shiny and may be superseded in the future.
# See <https://github.com/rstudio/shiny/blob/main/R/imageutils.R>
options(shiny.useragg = TRUE)

# user JS to activate the last tab
activate_last_tab_plots <- '
const activetab = document.querySelector("#theSeqPlot .nav-tabs li.active");
const lasttab = document.querySelector("#theSeqPlot .nav-tabs li:last-child");
activetab.classList.remove("active");
lasttab.classList.add("active");
'

# UI ---------------------------------------------------------------------------

ui <- function(request) {
  source("ui/main.R", local = TRUE)$value
}

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  source("server/main.R", local = TRUE)$value
}

shinyApp(ui, server)
