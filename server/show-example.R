#rv_example <- reactiveValues("wchar" = "\\u03b1", "digits" = 5, "width" = .75, "height" = .5, "size" = 8, "pal_name" = "gray", "pal_alpha" = 0.6,  "trdigits" = 4)

# Display settings modal (from Hypothesis tab)
observeEvent(input$btn_hgraph_example_modal, {
  showModal(modalDialog(
    title = "Choose Example",

    selectInput(
      inputId = "example_hgraph",
      label = "",
      choices = gsub(".rds", "",
                gsub("_", " ",
                gsub("-", ": ",
                gsub("\\+", ", ", stringi::stri_sort(list.files("data/"), numeric = TRUE))))),
      width = "100%"
    ),

    easyClose = TRUE,
    footer = tagList(
      actionButton("btn_show_example", label = "Load Example", class = "btn-primary", icon = icon("magic"))
    )
  ))
})

observeEvent(input$btn_show_example, {
  # rds <- input$btn_design_restore
  # req(rds)
  lst <- readRDS(file = file.path("data/", paste0(gsub(" ", "_",
                                                  gsub(": ", "-",
                                                  gsub(", ", "\\+", input$example_hgraph))), ".rds")))
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

