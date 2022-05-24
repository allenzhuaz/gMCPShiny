# hypotheses
observeEvent(input$btn_hypotheses_addrow, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = addMatrixRow(input$hypothesesMatrix))
})

observeEvent(input$btn_hypotheses_delrow, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = delMatrixRow(input$hypothesesMatrix))
})

observeEvent(input$btn_hypotheses_reset, {
  updateMatrixInput(session, inputId = "hypothesesMatrix", value = as.matrix(data.frame(cbind(Name = paste0("H", 1:4),
                                                                                              Alpha = rep(0.025/4, 4),
                                                                                              Group = LETTERS[1:4]))))
})

# group
observeEvent(input$btn_group_addrow, {
  updateMatrixInput(session, inputId = "groupMatrix", value = addMatrixRow(input$groupMatrix))
})

observeEvent(input$btn_group_delrow, {
  updateMatrixInput(session, inputId = "groupMatrix", value = delMatrixRow(input$groupMatrix))
})

observeEvent(input$btn_group_reset, {
  updateMatrixInput(session, inputId = "groupMatrix", value = as.matrix(data.frame(cbind(Group = LETTERS[1:4],
                                                                                         GroupName = paste0("Group ", LETTERS[1:4])))))
})

# Edge: Transition Matrix (original m*m Matrix or From-To weight)
observeEvent(input$btn_tranwt_addrow, {
  updateMatrixInput(session, inputId = "tranwtMatrix", value = addMatrixRow(input$tranwtMatrix))
})

observeEvent(input$btn_tranwt_delrow, {
  updateMatrixInput(session, inputId = "tranwtMatrix", value = delMatrixRow(input$tranwtMatrix))
})

observeEvent(input$btn_tranwt_reset, {
  updateMatrixInput(session, inputId = "tranwtMatrix", value = as.matrix(data.frame(cbind(From = c(1, 2, 3, 4),
                                                                                          To = c(2, 3, 4, 1),
                                                                                          Weight = rep(1, 4)))))
})

# Node position
observeEvent(input$btn_nodepos_addrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = addMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_nodepos_delrow, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = delMatrixRow(input$nodeposMatrix))
})

observeEvent(input$btn_nodepos_reset, {
  updateMatrixInput(session, inputId = "nodeposMatrix", value = as.matrix(data.frame(cbind(X = c(-1, 1, -1, 1),
                                                                                           Y = c(1, 1, -1, -1)))))
})

