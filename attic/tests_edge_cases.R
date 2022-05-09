
test_that("Edge cases", {
  skip("Tested locally")
  skip_on_cran()
  skip_if_not_installed("mlr3oml")
  skip_if_not_installed("mlr3pipelines")
  library(mlr3)
  library(mlr3pipelines)
  library(mlr3fairness)
  task = tsk("oml", task_id = 317599)
  task$col_roles$pta = "x2"
  l = as_learner(po("explicit_pta") %>>% po("reweighing_os") %>>% lrn("classif.rpart"))
  l$train(task)
  expect_true(!is.null(l$model))
})

test_that("Edge cases", {
  skip("Tested locally")
  skip_on_cran()
  skip_if_not_installed("mlr3oml")
  skip_if_not_installed("mlr3pipelines")
  library(mlr3)
  library(mlr3pipelines)
  library(mlr3fairness)
  task = tsk("oml", task_id = 317599)
  task$col_roles$pta = "x2"
  l = lrn("classif.fairfgrrm")
  r = resample(task, l, rsmp("cv", folds = 3L))
  r$aggregate(msr("fairness.acc"))

  l = as_learner(po("explicit_pta") %>>% lrn("classif.fairfgrrm"))
  l$train(task)
  expect_true(!is.null(l$model))
})

