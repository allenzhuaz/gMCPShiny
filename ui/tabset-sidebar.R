headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Hypotheses",
      matrixInput(
        "hypothesesMatrix",
        label = tagList(
          "Set hypotheses:",
          helpPopover(
            "Hypotheses matrix",
            "\"Name\" and \"Group\" require text input, \"Alpha\" requires numeric input.
            The text inputs support Unicode escape sequence like `\\uABCD` for
            special characters. Click the second icon for a more comprehensive character list.
            Use `\\n` to add a line break. See `?Quotes` for details."
          ),
          HTML('&nbsp;'),
          helpLink("https://en.wikipedia.org/wiki/List_of_Unicode_characters")
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
      matrixInput(
        "nodeposMatrix",
        label = tagList(
          "Customize node position:",
          helpPopover(
            "Node position matrix",
            "The \"Hypotheses\" requires text input, shoud match hypotheses names.
            The \"x\", \"y\" numeric inputs are coordinates for the
            relative position of the hypothesis ellipses."
          )
        ),
        value = as.matrix(data.frame(cbind(
          Hypothesis = paste0("H", 1:4),
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
      selectInput(
        "legendPosition",
        label = tagList(
          "Legend position:",
          helpPopover(
            "legend.position",
            "Select \"none\" to turn off legend."
          )
        ),
        choices = c("none", "left", "right", "bottom", "top"),
        selected = "bottom",
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.legendPosition != 'none'",
        textInput(
          "legend.name",
          label = tagList(
            "Legend title:",
            helpPopover(
              "legend.name",
              "Text for legend title"
            )
          ),
          value = "Group Name"
        ),
        numericInput(
          "legend.textsize",
          label = tagList(
            "Legend text size:",
            helpPopover(
              "legend.textsize",
              "Legend text size"
            )
          ),
          value = 20, min = 6, max = 50, step = 1
        )
      ),

      br(),
      selectInput(
        "titlePosition",
        label = tagList(
          "Title position:",
          helpPopover(
            "title.position",
            "Select \"none\" to turn off title"
          )
        ),
        choices = c("none", "bottom", "top"),
        selected = "none",
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.titlePosition != 'none'",
        textInput(
          "title.name",
          label = tagList(
            "Title:",
            helpPopover(
              "title.name",
              "Text for title"
            )
          ),
          value = ""
        ),
        numericInput(
          "title.textsize",
          label = tagList(
            "Title text size:",
            helpPopover(
              "title.textsize",
              "Title text size"
            )
          ),
          value = 30, min = 6, max = 50, step = 1
        )
      ),


      hr(),
      actionButton(
        "btn_node_setting_modal",
        label = "More Node Settings",
        class = "btn btn-outline-primary",
        icon = icon("cog"),
        width = "100%"
      )
    ),
    tabPanel(
      "Transitions",
      matrixInput("trwtMatrix",
        label = tagList(
          "Set transition weights:",
          helpPopover(
            "Transition weights matrix",
            "\"From\" and \"To\" requires text input, shoud match hypotheses names.
            \"Weight\" supports numeric input and arithmetic expressions, should range between 0 and 1.
              Transitions between non-existing hypotheses will not affect graph output."
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
      actionButton(
        "btn_edge_setting_modal",
        label = "More Edge Settings",
        class = "btn btn-outline-primary",
        icon = icon("cog"),
        width = "100%"
      )
    )
  )
)
