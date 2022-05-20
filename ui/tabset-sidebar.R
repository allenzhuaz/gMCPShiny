headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",

    # Hypotheses Tab -----

    tabPanel(
      "Hypotheses",
      h4("Set the # of Hypotheses"),
      p("(Both name and group must be updated.)"),
      HTML('<p>The text input uses the plotmath syntax. See <a href="https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html", target = "_blank">plot math syntax</a> for details. Use <code>\\n</code> to add a line break.</p>'),
      br(),
      rHandsontableOutput("hotHypotheses"),
      br(),
      rHandsontableOutput("hotGroups"),
      br(),
      actionButton("update", label = "Update Nodes", class = "btn btn-outline-primary", icon = icon("sync")),
      # bookmarkButton(),
      br(),
      h4("Save or load table data"),
      p("When loading data, only the tables (hypotheses, groups, transitions, and positions) will be updated. User must press the each update button to change the graph."),
      downloadButton("save_inputs", label = "Save Tables", class = "btn btn-outline-primary"),
      br(),
      fileInput("load_inputs", "Load Table Inputs from .rda or .rdata file", accept = c(".rda", ".rdata"))
    ), # end Hypotheses Tab

    # Transitions Tab -----

    tabPanel(
      "Transitions",
      h4("Input Data Frame"),
      p("Transitions between non-existing hypotheses will not influence graph output."),
      rHandsontableOutput("hotTransitions"),
      br(),
      actionButton("updateEdges", label = "Update Edges", class = "btn btn-outline-primary", icon = icon("sync"))
    ), # end Transitions Tab

    # Format Tab -----

    tabPanel(
      "Format",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Ellipses",
          h4("Change the Shape and Text Format"),
          sliderInput("width", "Set the ellipsis width:", 0, 1, .75, .01),
          sliderInput("height", "Set the ellipsis height:", 0, 1, .5, .01),
          numericInput("size", "Hypothesis Text Size", 8, 1, 10, 1),
          numericInput("digits", "# Digits for weight/alpha", 3, 1, 6, 1),
          sliderInput("rotation", "Set the rotation", -1, 1, 0, .005),
          br(),
          h4("Set the Positions"),
          p("By default, any custom positioning data will be lost if you add hypotheses or set the rotation (hypotheses will become equally spaced again).
                  Check the box below to keep custom positions, place all new hypotheses at position (0,0), and maintain current spacing when rotating."),
          checkboxInput("chkCustomPositions", label = "Keep Custom Positions", value = FALSE),
          rHandsontableOutput("hotPositions"),
          actionButton("updatePositions", label = "Update Custom Positions", class = "btn btn-outline-primary", icon = icon("sync"))
        ), # end Ellipses subtab

        tabPanel(
          "Connections",
          br(),
          numericInput("boxtextsize", "Transition Text Size", 6, 1, 10, 1),
          numericInput("trhw", "Transition Box Width", .13, .05, .3, .01),
          numericInput("trhh", "Transition Box Height", .1, .05, .3, .01),
          numericInput("trdigits", "# Digits for transitions", 2, 1, 6, 1),
          sliderInput("trprop", "Transition positioning", .05, .95, .33, .01),
          numericInput("arrowsize", "Arrow Size", .035, .005, .6, .005),
          numericInput("offset", "Space between Neighbor Arrows", .01, pi, .6, pi/4)
        ), # end Connections subtab

        tabPanel(
          "Colors",
          br(),
          checkboxInput("chkAddColors", label = "Add Colors", value = FALSE),
          conditionalPanel(
            condition = "input.chkAddColors == true",
            h4("Set the Colors"),
            selectInput("palette", "Select the Color Palette:", c("Dark2", "Greyscale", "Color Blind", "All Colors")),
            uiOutput("colorSet")
          ), # end Add Colors conditional panel

          checkboxInput("chkLegend", label = "Show Legend", value = FALSE),
          conditionalPanel(
            condition = "input.chkLegend == true",
            textInput("txtLegendName", label = "Legend Name:", value = "Group Name"),
            selectInput("legend.position", label = "Legend Position", choices = c("none", "left", "right", "bottom", "top"), selected = "bottom", multiple = FALSE),
            numericInput("legendtextsize", "Legend text size", 20, 6, 14, 1)
          ) # end Show Legend conditional panel
        ) # end Colors subtab
      ) # end tabsetPanel (subtabs)
    ) # end Format tab
  ) # end tabsetPanel (maintabs)
)
