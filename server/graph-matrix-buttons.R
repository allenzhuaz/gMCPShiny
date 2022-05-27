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
  updateMatrixInput(session, inputId = "trwtMatrix", value = as.matrix(data.frame(cbind(From = c(1, 2, 3, 4),
                                                                                          To = c(2, 3, 4, 1),
                                                                                          Weight = rep(1, 4)))))
})

# Node position
observeEvent(input$btn_nodeposMatrix_addrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = addMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_nodeposMatrix_delrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = delMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_nodeposMatrix_reset, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = as.matrix(data.frame(cbind(X = c(-1, 1, -1, 1),
                                                                                           Y = c(1, 1, -1, -1)))))
})

