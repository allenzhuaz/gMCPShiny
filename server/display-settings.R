# Create a reactive value object to maintain the settings globally
rv_nodes <- reactiveValues("wchar" = "\\u03b1", "digits" = 5, "width" = .75, "height" = .5, "size" = 8, "pal_name" = "gray", "pal_alpha" = 0.6,  "trdigits" = 4)

# Display settings modal (from Hypothesis tab)
observeEvent(input$btn_node_setting_modal, {
  showModal(modalDialog(
    title = "Node Display Settings",

    h5("Node Shape and Text Format", style = "margin-top: 0;"),
    hr(),

    fluidRow(
      column(
        width = 3,
    numericInput("width", "Width: ", min = 0, max = 1, value = rv_nodes$width, step = .01)),

      column(
        width = 3,
    numericInput("height", "Height: ", min = 0, max = 1, value = rv_nodes$height, step = .01)),

    column(
      width = 3,
      numericInput("digits",
                   label = tagList(
                     "Digits: ",
                     helpPopover(
                       "digits",
                       "Number of digits past the decimal for siginificance level in the hypothesis nodes"
                     )
                   ), value = rv_nodes$digits, min = 1, max = 12, step = 1, width = "100%"
      )),

    column(
      width = 3,
    numericInput("size", "Text Size: ", value = rv_nodes$size, min = 1, max = 20, step = 1))),

    h5("Node Symbol and Color", style = "margin-top: 0;"),
    hr(),
    fluidRow(

      column(
        width = 6,
        textInput("wchar",
                  label = tagList(
                    "Symbol: ",
                    helpPopover(
                      "wchar",
                      "Symbol character to be shown for significance level in the hypothesis nodes.
                  Input \\unicode for Greek letters, for example \\u03b1 for alpha.
                  For full dictionary, please refer to http://kestrel.nmt.edu/~raymond/software/howtos/greekscape.xhtml"
                    )
                  ), value = rv_nodes$wchar, width = "100%"
        )),

      column(
        width = 6,
    selectInput(
      "pal_name",
      label = "Palette: ",
      choices = c(
        "gray",
        "Okabe-Ito",
        "d3.category10",
        "d3.category20",
        "d3.category20b",
        "d3.category20c",
        "Teal"
      ), selected = rv_nodes$pal_name
    ))),

  fluidRow(
    column(
      width = 12,
    sliderInput("pal_alpha",
                label = tagList(
                  "Transparency: ",
                  helpPopover(
                    "Color Transparency",
                    "Values range from 0 to 1 , with lower values corresponding to more transparent colors"
                  )
                ),
                min = 0.1, max = 1, value = rv_nodes$pal_alpha, step = 0.01, width = "100%"))),


    easyClose = FALSE,
    footer = tagList(
      actionButton("btn_node_settings_save", label = "Save Settings", class = "btn-primary", icon = icon("save")),
      modalButton("Cancel")
    )
  ))
})

observeEvent(input$btn_node_settings_save, {
  rv_nodes$wchar <- input$wchar
  rv_nodes$digits <- input$digits
  rv_nodes$width <- input$width
  rv_nodes$height <- input$height
  rv_nodes$size <- input$size
  rv_nodes$pal_name <- input$pal_name
  rv_nodes$pal_alpha <- input$pal_alpha
  removeModal()
})


# Create a reactive value object to maintain the settings globally
rv_edges <- reactiveValues("trhw" = .13, "trhh" = .1, "trdigits" = 4, "boxtextsize" = 6, "trprop" = .33, "arrowsize" = .035, "offset" = .15)

# Display settings modal (from Transition tab)
observeEvent(input$btn_edge_setting_modal, {
  showModal(modalDialog(
    title = "Edge Display Settings",
    h5("Transition Box Shape and Text Format", style = "margin-top: 0;"),
    hr(),
    fluidRow(
      column(
        width = 3,
    numericInput("trhw", "Width", rv_edges$trhw, 0, 1, .01)),

    column(
      width = 3,
    numericInput("trhh", "Height", rv_edges$trhh, 0, 1, .01)),


    column(
      width = 3,
    numericInput("trdigits",
             label = tagList(
               "Digits:",
               helpPopover(
                 "Digits for Transition Weights",
                 "Number of digits past the decimal for transition weight in the transition box"
               )
             ), value = rv_edges$trdigits, min = 1, max = 10, step = 1, width = "100%"
      )),

    column(
      width = 3,
    numericInput("boxtextsize", "Text Size", rv_edges$boxtextsize, 1, 20, 1))),

    h5("Edge Layout", style = "margin-top: 0;"),
    hr(),

    fluidRow(
     column(
      width = 6,
    numericInput("arrowsize", "Arrow Size: ", rv_edges$arrowsize, .005, .6, .005)),

    column(
      width = 6,
    numericInput("offset", label = tagList(
      "Offset:",
      helpPopover(
        "offset",
        "Rotational offset in radians for transition edges, can control space between neighbor edges"
      )
    ), rv_edges$offset, 0.01, 1, 0.01))),

    fluidRow(
      column(
        width = 12,
        sliderInput("trprop", label = tagList(
          "Box Position:",
          helpPopover(
            "trprop",
            "Transition box's position proportional to edge length (relative to the edge starting point), value can be set between 0 and 1"
          )
        ), min = .05, max = .95, value = rv_edges$trprop, step = .01, width = "100%"))
    ),

  easyClose = FALSE,
  footer = tagList(
    actionButton("btn_edge_settings_save", label = "Save Settings", class = "btn-primary", icon = icon("save")),
    modalButton("Cancel")
  )
    ))
  })

  observeEvent(input$btn_edge_settings_save, {
    rv_edges$trhw <- input$trhw
    rv_edges$trhh <- input$trhh
    rv_edges$trdigits <- input$trdigits
    rv_edges$boxtextsize <- input$boxtextsize
    rv_edges$trprop <- input$trprop
    rv_edges$arrowsize <- input$arrowsize
    rv_edges$offset <- input$offset
    removeModal()
  })




