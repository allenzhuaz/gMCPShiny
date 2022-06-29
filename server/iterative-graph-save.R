observeEvent(input$btn_modal_save_seq_png, {
  showModal(modalDialog(
    title = "Dowload Plot",
    textInputAddonRight(
      "filename_png",
      label = tagList(
        "File name:",
        helpPopover(
          "filename",
          "PNG file name."
        )
      ),
      value = "iterative-hgraph",
      addon = ".png",
      width = "100%"
    ),
    fluidRow(
      column(
        6,
        textInputAddonRight(
          "width_png",
          label = tagList(
            "Width:",
            helpPopover(
              "width",
              "Width of the PNG."
            )
          ),
          value = 1294, addon = "px", width = "100%"
        )
      ),
      column(
        6,
        textInputAddonRight(
          "height_png",
          label = tagList(
            "Height:",
            helpPopover(
              "height",
              "Height of the PNG."
            )
          ),
          value = 800, addon = "px", width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        6,
        numericInput(
          "dpi_png",
          label = tagList(
            "DPI:",
            helpPopover(
              "DPI",
              "Resolution of the PNG."
            )
          ),
          value = 72, min = 72, max = 600, width = "100%"
        )
      ),
      column(
        6,
        numericInput(
          "scale_png",
          label = tagList(
            "Scaling factor:",
            helpPopover(
              "scaling",
              "Multiplicative scaling factor for line width and text size.
              Useful for getting the right dimensions at the resolution you need.
              For example, if you need a plot at 3000x2000 px to fit into a layout,
              but it appears too small, you can increase this setting to
              make everything appear bigger at the same resolution."
            )
          ),
          value = 2, min = 1, max = 10, width = "100%"
        )
      )
    ),
    easyClose = TRUE,
    footer = tagList(
      downloadButton("btn_save_seq_png", label = "Download Plot", class = "btn-primary", icon = icon("download")),
      modalButton("Cancel")
    )
  ))
})

output$btn_save_seq_png <- downloadHandler(
  filename = function() {
    x <- input$filename_png
    # sanitize from input
    fn0 <- if (x == "") "iterative-hgraph" else sanitize_filename(x)
    # sanitize again
    fn <- if (fn0 == "") "iterative-hgraph" else fn0
    paste0(fn, ".png")
  },
  content = function(con) {
    ragg::agg_png(
      filename = con,
      width = min(8000, as.numeric(input$width_png)),
      height = min(8000, as.numeric(input$height_png)),
      units = "px",
      res = input$dpi_png,
      scaling = input$scale_png
    )
    print(SeqPlotInput())
    dev.off()
  }
)
