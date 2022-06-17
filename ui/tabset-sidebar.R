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
      matrixButtonGroup("hypothesesMatrix"),

      br(),

      matrixInput("nodeposMatrix",
                  label = tagList(
                    "Customize Node Position",
                    helpPopover(
                      "Node Position Matrix",
                      "The \"Hypotheses\" text inputs support Unicode escape sequence, like `\\uABCD` for a special symbol and `\\n` for adding a line break. See `?Quotes` for details.
                  The \"x\", \"y\" numeric inputs are coordinates for the relative position of the hypothesis ellipses."
                    )
                  ),
                  value = as.matrix(data.frame(cbind(Hypothesis = paste0("H", 1:4),
                                                     x = c(-1, 1, 1, -1),
                                                     y = c(1, 1, -1, -1)
                  ))),
                  class = "character",
                  rows = list(names = FALSE, editableNames = FALSE, extend = FALSE),
                  cols = list(names = TRUE, editableNames = FALSE, extend = FALSE)
      ),
      actionButton(
        "btn_nodeposMatrix_reset_init",
        label = "",
        icon = icon("sync"),
        width = "100%",
        class = "btn btn-block btn-outline-primary"
      ),

      br(),
      br(),

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
        numericInput("legend.textsize", "Legend Text Size", 20, 6, 50, 1)

    ),

    hr(),

    actionButton("btn_node_setting_modal", label = "More Node Settings", class = "btn btn-outline-primary", icon = icon("cog"), width =  "100%")
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
      hr(),
      actionButton("btn_edge_setting_modal", label = "More Edge Settings", class = "btn btn-outline-primary", icon = icon("cog"), width =  "100%")

    ) # end Transitions Tab

    # Format Tab -----

  ) # end tabsetPanel (maintabs)
)
