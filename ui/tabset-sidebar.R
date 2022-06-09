headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",

    # Hypotheses Tab -----

    tabPanel(
      "Hypotheses",
      h4("Set the # of Hypotheses"),
      p("(Both name and group must be updated.)"),
      matrixInput("hypothesesMatrix",
                  label = tagList(
                    "Hypotheses matrix",
                    helpPopover(
                      "hypothesesMatrix",
                      "The text input uses the plotmath syntax. See ?grDevices::plotmath for details. Use \\n to add a line break."
                    )
                  ),
                  value = as.matrix(data.frame(cbind(Name = paste0("H", 1:4),
                                                     Alpha = rep(0.025/4, 4),
                                                     Group = LETTERS[1:4]))),
                  class = "character",
                  rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
                  cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
      ),
      matrixButtonGroup("hypothesesMatrix")
    ), # end Hypotheses Tab

    # Transitions Tab -----

    tabPanel(
      "Transitions",
      h4("Input Data Frame"),
      p("Transitions between non-existing hypotheses will not influence graph output."),
      matrixInput("trwtMatrix",
                  value = as.matrix(data.frame(cbind(From = paste0("H", c(1, 2, 3, 4)),
                                                     To = paste0("H", c(2, 3, 4, 1)),
                                                     Weight = rep(1, 4)))),
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
          h4("Change the Shape and Text Format"),
          sliderInput("width", "Set the ellipsis width:", 0, 1, .75, .01),
          sliderInput("height", "Set the ellipsis height:", 0, 1, .5, .01),
          numericInput("size", "Hypothesis Text Size", 8, 1, 10, 1),
          numericInput("digits", "# Digits for weight/alpha", 3, 1, 6, 1),
          textInput("wchar", "Type 1 error symbol", "\u03b1"),
          br(),
          h4("Set the Positions"),
          p("By default, any custom positioning data will be lost if you add hypotheses or set the rotation (hypotheses will become equally spaced again).
                  Check the box below to keep custom positions, place all new hypotheses at position (0,0), and maintain current spacing when rotating."),
            uiOutput("initnodepos"),
            matrixButtonGroup("nodeposMatrix")
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
          "Color and Legend",
          br(),
            selectInput("pal_name", "Select the Color Palette", c("d3--category20", "d3--category20b", "d3--category20c", "Grey", "Merck", "Okabe-Ito"), selected = "Grey"),
            sliderInput("pal_alpha", "Color Transparency", 0.1, 1, 0.6, .01),

            selectInput("legendPosition", label = "Legend Position", choices = c("none", "left", "right", "bottom", "top"), selected = "bottom", multiple = FALSE),
             conditionalPanel(
               condition = "input.legendPosition != 'none'",
                textInput("legend.name", label = "Legend Name:", value = "Group Name"),
                numericInput("legend.textsize", "Legend text size", 20, 6, 14, 1)
              ) # end Show Legend conditional panel
        ) # end Color and Legend subtab
      ) # end tabsetPanel (subtabs)
    ) # end Format tab
  ) # end tabsetPanel (maintabs)
)