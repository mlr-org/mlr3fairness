source(file.path(getwd(), "data_helper.R"))
fair_measure = msr("fairness.tpr")
data = biased_test_data()

test_that("reweighing PipeOp can be loaded and works with true positive ratio fairness measures", {
  reweighing = po("reweighing")
  learner_po = po("learner", learner = lrn("classif.rpart"))
  graph = reweighing %>>% learner_po
  glrn = GraphLearner$new(graph)
  glrn$train(data)
  tem = glrn$predict(data)

  expect_true( round(fair_measure$score(tem, data),4) == 1.2857 )
})
