#' hGraph example data
#'
#' Illustrate the example data file's naming convention and string replacement
#' rule for displaying them in the Shiny UI.
#'
#' @section File names:
#' Use the naming convention "Example_##-##_hypotheses+##_groups_(method xyz)"
#' for the example data files.
#'
#' @section Display text:
#' Display the example names with regular expression replacement:
#' - "_" is replaced by " ",
#' - "-" is replaced by ": "
#' - "+" is replace by ", "

#' Convert file names to display text
#' @param file File names.
filename_to_text <- function(file) {
  gsub(".rds", "", gsub("_", " ", gsub("-", ": ", gsub("\\+", ", ", file))))
}

#' Convert display text to file names
#' @param x Character strings of display text.
text_to_filename <- function(x) {
  paste0(gsub(" ", "_", gsub(": ", "-", gsub(", ", "\\+", x))), ".rds")
}

# Display settings modal (from Hypothesis tab)
observeEvent(input$btn_hgraph_example_modal, {
  showModal(modalDialog(
    title = "Choose Example",
    selectInput(
      inputId = "example_hgraph",
      label = "",
      choices = filename_to_text(stringi::stri_sort(list.files("data/"), numeric = TRUE)),
      width = "100%"
    ),
    easyClose = TRUE,
    footer = tagList(
      actionButton(
        "btn_show_example",
        label = "Load Example",
        class = "btn-primary",
        icon = icon("magic")
      )
    )
  ))
})

observeEvent(input$btn_show_example, {
  # rds <- input$btn_design_restore
  # req(rds)
  lst <- readRDS(file = file.path("data/", text_to_filename(input$example_hgraph)))
  hgraph_inputs <- lst$hgraph_inputs

  # Restore regular inputs and matrix inputs separately
  is_matrix_input <- unname(sapply(
    lapply(lst$hgraph_inputs, class),
    FUN = function(x) "matrix" %in% x
  ))


  lapply(
    names(hgraph_inputs)[is_matrix_input],
    function(x) updateMatrixInput(session, inputId = x, value = hgraph_inputs[[x]])
  )

  lapply(
    names(hgraph_inputs)[!is_matrix_input],
    function(x) session$sendInputMessage(x, list(value = hgraph_inputs[[x]]))
  )

  # Restore global reactive values
  node_settings <- lst$node_settings

  rv_nodes$wchar <- node_settings$rv_nodes$wchar
  rv_nodes$digits <- node_settings$rv_nodes$digits
  rv_nodes$width <- node_settings$rv_nodes$width
  rv_nodes$height <- node_settings$rv_nodes$height
  rv_nodes$size <- node_settings$rv_nodes$size
  rv_nodes$pal_name <- node_settings$rv_nodes$pal_name
  rv_nodes$pal_alpha <- node_settings$rv_nodes$pal_alpha

  edge_settings <- lst$edge_settings

  rv_edges$trhw <- edge_settings$rv_edges$trhw
  rv_edges$trhh <- edge_settings$rv_edges$trhh
  rv_edges$trdigits <- edge_settings$rv_edges$trdigits
  rv_edges$boxtextsize <- edge_settings$rv_edges$boxtextsize
  rv_edges$trprop <- edge_settings$rv_edges$trprop
  rv_edges$arrowsize <- edge_settings$rv_edges$arrowsize
  rv_edges$offset <- edge_settings$rv_edges$offset

  removeModal()
})
