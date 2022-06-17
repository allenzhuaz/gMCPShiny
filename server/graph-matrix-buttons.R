# hypotheses
observeEvent(input$btn_hypothesesMatrix_addrow, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = addMatrixRow(input$hypothesesMatrix))
})

observeEvent(input$btn_hypothesesMatrix_delrow, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = delMatrixRow(input$hypothesesMatrix))
})

observeEvent(input$btn_hypothesesMatrix_reset, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = as.matrix(data.frame(cbind(Name = paste0("H", 1:4),
                                                                                              Alpha = rep(0.025/4, 4),
                                                                                              Group = LETTERS[1:4]))))
})

# group
observeEvent(input$btn_groupMatrix_addrow, {
  updateMatrixInput(session, inputId = "groupMatrix", value = addMatrixRow(input$groupMatrix))
})

observeEvent(input$btn_groupMatrix_delrow, {
  updateMatrixInput(session, inputId = "groupMatrix", value = delMatrixRow(input$groupMatrix))
})

observeEvent(input$btn_groupMatrix_reset, {
  updateMatrixInput(session, inputId = "groupMatrix", value = as.matrix(data.frame(cbind(Group = LETTERS[1:4],
                                                                                         GroupName = paste0("Group ", LETTERS[1:4])))))
})

# Edge: Transition Matrix (original m*m Matrix or From-To weight)
observeEvent(input$btn_trwtMatrix_addrow, {
  updateMatrixInput(session, inputId = "trwtMatrix", value = addMatrixRow(input$trwtMatrix))
})

observeEvent(input$btn_trwtMatrix_delrow, {
  updateMatrixInput(session, inputId = "trwtMatrix", value = delMatrixRow(input$trwtMatrix))
})

observeEvent(input$btn_trwtMatrix_reset, {
  updateMatrixInput(session, inputId = "trwtMatrix", value = as.matrix(data.frame(cbind(From = paste0("H", c(1, 2, 3, 4)),
                                                                                          To = paste0("H", c(2, 3, 4, 1)),
                                                                                          Weight = rep(1, 4)))))
})

# Node position
observeEvent(input$btn_hypothesesMatrix_addrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = addMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_hypothesesMatrix_delrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = delMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_nodeposMatrix_reset_init, {
  showModal(modalDialog(
    title = p("Attention", icon("exclamation-triangle")),

    p("Press ", icon("sync"),  " will sync the hypothesis name in this table, but reset the graph layout to default circular layout, i.e. you will lose the current customized node position."),

    p("Do you still want to proceed?"),

    easyClose = TRUE,
    footer = tagList(
      actionButton("btn_nodeposMatrix_reset", label = "Yes", class = "btn-primary"),
      modalButton("No")
    )))
})


observeEvent(input$btn_nodeposMatrix_reset, {
  radianStart <- if((nrow(input$hypothesesMatrix))%%2 != 0) {
    pi * (1/2 + 1/nrow(input$hypothesesMatrix)) } else {
      pi * (1 + 2/nrow(input$hypothesesMatrix))/2 }

  updateMatrixInput(session, inputId = "nodeposMatrix", value = as.matrix(data.frame(cbind(Hypothesis = input$hypothesesMatrix[,"Name"],
                                                                                           x = round(2 * cos((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi)), 6),
                                                                                           y = round(2 * sin((radianStart - (0:(nrow(input$hypothesesMatrix)-1))/nrow(input$hypothesesMatrix)*2*pi) %% (2*pi)), 6)
))))
  removeModal()
})

