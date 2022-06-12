headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",

    # Hypotheses Tab -----

    tabPanel(
      "Hypotheses",
      matrixInput("hypothesesMatrix",
        label = tagList(
          "Set Hypotheses",
          helpPopover(
            "Hypotheses Matrix",
            "\"Name\" and \"Group\" need text input, \"Alpha\" needs numeric input.
            The text inputs support Unicode escape sequence, like `\\uABCD` for a special symbol and `\\n` for adding a line break. See `?Quotes` for details."
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
      matrixInput("trwtMatrix",
          label = tagList(
            "Set Transition Weights",
            helpPopover(
              "Transition Weights Matrix",
              "\"From\" and \"To\" need text input, shoud be corresponded to hypotheses names, \"Weight\" needs numeric input, should be between 0 and 1.
              Transitions between non-existing hypotheses will not influence graph output."
            )
          ),
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

          sliderInput("width", "Ellipsis Width", 0, 1, .75, .01),
          sliderInput("height", "Ellipsis Height", 0, 1, .5, .01),
          numericInput("size", "Hypothesis Text Size", 8, 1, 20, 1),
          numericInput("digits", "# Digits for Significance Level", 5, 1, 12, 1),
          textInput("wchar", "Significance Level Symbol", "\\u03b1"),

          uiOutput("initnodepos"),
          matrixButtonGroup("nodeposMatrix")
        ), # end Ellipses subtab

        tabPanel(
          "Connections",

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
          sliderInput("pal_alpha",
                      label = tagList(
                      "Color Transparency",
                      helpPopover(
                        "Color Transparency",
                        "Values range from 0 to 1 , with lower values corresponding to more transparent colors"
                      )
                    ),
                    min = 0.1, max = 1, value = 0.6, step = 0.01),
          selectInput("legendPosition",
                      label = tagList(
                        "Legend Position",
                        helpPopover(
                          "legend.position",
                          "choosing \"none\" is to turn off legend."
                        )
                      ),

                      choices = c("none", "left", "right", "bottom", "top"), selected = "bottom", multiple = FALSE),
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
