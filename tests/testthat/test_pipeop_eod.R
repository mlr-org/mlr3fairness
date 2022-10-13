test_that("PipeOpEOd works on a task", {
  skip_on_cran()
  skip_if_not_installed("linprog")
  task = tsk("adult_train")$filter(1:700)
  poed = po("EOd")
  graph = po("learner_cv", lrn("classif.rpart")) %>>% poed
  glrn = GraphLearner$new(graph)
  glrn$train(task)
  expect_true(names(glrn$state$model$EOd) == "flip_probs")
  tem = glrn$predict(task)
  expect_true(!is.null(glrn$state))
  expect_r6(tem, "PredictionClassif")
  expect_number(tem$score(msr("fairness.eod"), task = task))
})

test_that("PipeOpEOd technically works for trivial cases priv 0", {
  skip_on_cran()
  skip_if_not_installed("linprog")
  # Test data set / task
  dt = data.table(
    truth = rep(c(0, 1), 100),
    pta = rep(c(0, 1), each = 100)
  )
  dt[, prediction := truth]
  dt[pta == 1 & truth == 1, prediction := c(rep(0, 25), rep(1, 25))]
  dt[, truth := factor(truth)]
  dt[, prediction := factor(prediction)]
  t = TaskClassif$new("test_task", dt, target = "truth")
  t$set_col_roles("pta", "pta")

  # Fairness Tensor based
  dft = fairness_tensor(dt, task = t)[[1]] == fairness_tensor(dt, task = t)[[2]]
  expect_true(all(dft[, 1]))
  expect_true(all(!dft[, 2]))

  # Errors as epxected
  poed = po("EOd", privileged = "5")
  expect_error(poed$train(list(t)), "needs to be a valid value")
  # Trains as expected
  poed = po("EOd", privileged = "0")
  poed$train(list(t))
  expect_true(all(unlist(poed$state) == c(1, 0.5, 1, 0)))
  prd = poed$predict(list(t))[[1]]
  expect_true(prd$score(msr("fairness.eod"), task = t) < 1 / 50)
  # Matching fairness tensors afterwards
  expect_true(all(fairness_tensor(prd, task = t)[[1]] == fairness_tensor(prd, task = t)[[2]]))

  # No changes as expected
  poed = po("EOd", privileged = "1")
  poed$train(list(t))
  expect_true(all(unlist(poed$state) == c(1, 0, 1, 0.5)))
  prd = poed$predict(list(t))[[1]]
  expect_true(all(pmap_lgl(list(fairness_tensor(dt, task = t), fairness_tensor(prd, task = t)), function(x, y) all(x == y))))
})

test_that("PipeOpEOd technically works for trivial cases priv 1", {
  skip_on_cran()
  skip_if_not_installed("linprog")
  # Test data set
  dt = data.table(
    truth = rep(c(0, 1), 100),
    pta = rep(c(0, 1), each = 100)
  )
  dt[, prediction := truth]
  dt[pta == 0 & truth == 0, prediction := c(rep(0, 25), rep(1, 25))]
  dt[, truth := factor(truth)]
  dt[, prediction := factor(prediction)]
  # Debias
  t = TaskClassif$new("test_task", dt, target = "truth", positive = "0")
  t$set_col_roles("pta", "pta")
  poed = po("EOd", privileged = "1")
  poed$train(list(t))
  expect_true(all(unlist(poed$state) == c(0.5, 0, 1, 0)))
  prd = poed$predict(list(t))[[1]]
  expect_true(all(fairness_tensor(prd, task = t)[[1]] == fairness_tensor(prd, task = t)[[2]]))
})
