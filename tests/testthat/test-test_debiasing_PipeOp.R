source(file.path(getwd(), "data_helper.R"))
fair_measure = msr("fairness", base_measure = msr("classif.pp"), operation = "groupwise_quotient")
learner_po = po("learner", learner = lrn("classif.rpart"))
task = biased_test_data()

test_that("reweighing PipeOp can be loaded and works with disparate impact score fairness measures", {
  reweighing = po("reweighing")
  graph = reweighing %>>% learner_po
  glrn = GraphLearner$new(graph)
  glrn$train(task)
  tem = glrn$predict(task)

  expect_true( round(fair_measure$score(tem, task),4) == 0.3625 )
})

test_that("The helper function `conditional_binary_target_pta_count` for reweighing PipeOp work as expected", {
  data = task$data()
  target = task$target_names
  pta = task$col_roles$pta
  privileged = data[1,pta]
  result = conditional_binary_target_pta_count(data, target, pta, privileged)
  expect_true( all(as.numeric(result) == c(36,12,44,8)) )
})

test_that("The helper function `get_reweighing_weights()` for reweighing PipeOp work as expected", {
  data = task$data()
  target = task$target_names
  positive = task$positive
  pta = task$col_roles$pta
  privileged = data[1,pta]
  result = get_reweighing_weights(data, target, pta, privileged)
  expect_true( all(round(result ,2) == c(1.07, 0.95, 1.20, 1.30)) )
})
