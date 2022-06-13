# Save design modal (from Design tab)
observeEvent(input$btn_design_save_modal, {
  showModal(modalDialog(
    title = "Save Graph Design",
    textInputAddonRight("filename_rds_design", label = "Name the hgraph design:", value = "hgraph_design", addon = ".rds", width = "100%"),
    easyClose = TRUE,
    footer = tagList(
      downloadButton("btn_design_save", label = "Save Graph Design", class = "btn-primary", icon = icon("download")),
      modalButton("Cancel")
    )
  ))
})


# Save hgraph design object, all input parameters, and reactive values
output$btn_design_save <- downloadHandler(
  filename = function() {
    x <- input$filename_rds_design
    # sanitize from input
    fn0 <- if (x == "") "design" else sanitize_filename(x)
    # sanitize again
    fn <- if (fn0 == "") "design" else fn0
    paste0(fn, ".rds")
  },
  content = function(con) {

    # Get inputs
    hgraph_inputs <- lapply(isolate(reactiveValuesToList(input)), unclass)
    # Remove all irrelevant input values
    hgraph_inputs[which(grepl(
      "btn_|hgraphnav",
      names(hgraph_inputs)
    ))] <- NULL

    # Save to file
    lst <- list("hgraph_inputs" = hgraph_inputs)
    saveRDS(lst, file = con)
  }
)



# Restore hgraph parameters
observeEvent(input$btn_design_restore, {
  rds <- input$btn_design_restore
  req(rds)
  lst <- readRDS(rds$datapath)
  hgraph_inputs <- lst$hgraph_inputs

  # Restore regular inputs and matrix inputs separately
  is_matrix_input <- unname(sapply(
    lapply(lst$hgraph_inputs, class),
    FUN = function(x) "matrix" %in% x
  ))

  lapply(
    names(hgraph_inputs)[!is_matrix_input],
    function(x) session$sendInputMessage(x, list(value = hgraph_inputs[[x]]))
  )

  lapply(
    names(hgraph_inputs)[is_matrix_input],
    function(x) updateMatrixInput(session, inputId = x, value = hgraph_inputs[[x]])
  )

  # Restore Node Position parameters with a progress delay
  # Otherwise the execution timing of `renderUI()` will
  # make it impossible to restore these values
  shinyjs::delay(500, {
      updateMatrixInput(session, inputId = "nodeposMatrix", value = hgraph_inputs[["nodeposMatrix"]])
  })
})




