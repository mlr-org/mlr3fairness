test_that("reweighing PipeOp can be loaded and works with disparate impact score fairness measures", {
  task = tsk("adult_train")$filter(1:700)
  poed = po("EOd")
  graph = po("learner_cv", lrn("classif.rpart")) %>>% poed
  glrn = GraphLearner$new(graph)
  glrn$train(task)
  tem = glrn$predict(task)
  expect_true(!is.null(glrn$state))
  expect_r6(tem, "PredictionClassif")

  tem$score(msr("fairness.eod"), task = task)

  lrn("classif.rpart")$train(task)$predict(task)$score(msr("fairness.eod"), task = task)
})
