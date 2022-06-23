headingPanel(
  "Inputs",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Testing",
      selectInput(inputId = "knowpval", label = "How to update graph", choices = c("Directly reject hypotheses and pass \u03b1" = "no", "Reject hypotheses based on observed p-values" = "yes")),
      conditionalPanel(
        condition = "input.knowpval == 'no'",
        uiOutput("reject_update_ui")
      ),
      conditionalPanel(
        condition = "input.knowpval == 'yes'",
        uiOutput("pval_update_ui")
      )

    )
  )
)
