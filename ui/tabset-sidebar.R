headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",

    # Hypotheses Tab -----

    tabPanel(
      "Hypotheses",
      h4("Set Hypotheses"),
      br(),
      matrixInput("hypothesesMatrix",
        label = tagList(
          "Hypotheses matrix",
          helpPopover(
            "hypothesesMatrix",
            "Name and Group need text input, and Alpha needs numeric input. The text inputs support Unicode escape sequence, like \\uABCD for a special symbol and \\n for adding a line break. See ?Quotes for details."
          )
        ),
        value = as.matrix(data.frame(cbind(
          Name = paste0("H", 1:4),
          Alpha = rep(0.025 / 4, 4),
          Group = LETTERS[1:4]
        ))),
        class = "character",
        rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
        cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
      ),
      matrixButtonGroup("hypothesesMatrix")
    ), # end Hypotheses Tab

    # Transitions Tab -----

    tabPanel(
      "Transitions",
      h4("Set Transition Weights"),
      p("Transitions between non-existing hypotheses will not influence graph output."),
      matrixInput("trwtMatrix",
        value = as.matrix(data.frame(cbind(
          From = paste0("H", c(1, 2, 3, 4)),
          To = paste0("H", c(2, 3, 4, 1)),
          Weight = rep(1, 4)
        ))),
        class = "character",
        rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
        cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
      ),
      matrixButtonGroup("trwtMatrix"),
      br(),
    ), # end Transitions Tab

    # Format Tab -----

    tabPanel(
      "Format",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Ellipses",
          br(),
          sliderInput("width", "Ellipsis Width", 0, 1, .75, .01),
          sliderInput("height", "Ellipsis Height", 0, 1, .5, .01),
          numericInput("size", "Hypothesis Text Size", 8, 1, 10, 1),
          numericInput("digits", "# Digits for Significance Level", 3, 1, 6, 1),
          textInput("wchar", "Significance Level Symbol", "\\u03b1"),
          br(),
          h4("Set the Positions"),
          uiOutput("initnodepos"),
          matrixButtonGroup("nodeposMatrix")
        ), # end Ellipses subtab

        tabPanel(
          "Connections",
          br(),
          numericInput("boxtextsize", "Transition Text Size", 6, 1, 10, 1),
          numericInput("trhw", "Transition Box Width", .13, .05, .3, .01),
          numericInput("trhh", "Transition Box Height", .1, .05, .3, .01),
          numericInput("trdigits", "# Digits for Transition Weight", 2, 1, 6, 1),
          sliderInput("trprop", "Transition Positioning", .05, .95, .33, .01),
          numericInput("arrowsize", "Arrow Size", .035, .005, .6, .005),
          numericInput("offset", "Space between Neighbor Arrows", .15, 0.01, 1, 0.01)
        ), # end Connections subtab

        tabPanel(
          "Color and Legend",
          br(),
          selectInput(
            "pal_name",
            label = "Color Palette",
            choices = c(
              "gray",
              "Okabe-Ito",
              "d3.category10",
              "d3.category20",
              "d3.category20b",
              "d3.category20c",
              "Teal"
            ), selected = "gray"
          ),
          sliderInput("pal_alpha", label = "Color Transparency", min = 0.1, max = 1, value = 0.6, step = 0.01),
          selectInput("legendPosition", label = "Legend Position", choices = c("none", "left", "right", "bottom", "top"), selected = "bottom", multiple = FALSE),
          conditionalPanel(
            condition = "input.legendPosition != 'none'",
            textInput("legend.name", label = "Legend Name", value = "Group Name"),
            numericInput("legend.textsize", "Legend Text Size", 20, 6, 14, 1)
          ) # end Show Legend conditional panel
        ) # end Color and Legend subtab
      ) # end tabsetPanel (subtabs)
    ) # end Format tab
  ) # end tabsetPanel (maintabs)
)
