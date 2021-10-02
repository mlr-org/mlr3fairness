test_that("fairness measures work as expcted", {
  tsk = tsk("compas")
  prds = list(
    lrn("classif.rpart")$train(tsk)$predict(tsk),
    lrn("classif.rpart", predict_type = "prob")$train(tsk)$predict(tsk),
    lrn("classif.featureless", predict_type = "prob")$train(tsk)$predict(tsk)
  )
  metrics = c("fairness.acc", "fairness.EOd", "fairness.fn", "fairness.fnr", "fairness.fp",
    "fairness.fpr", "fairness.npv", "fairness.ppv", "fairness.tpr")
  map(prds, function(prd) {
    map(metrics, function (m) {
      out = prd$score(measures = msr(m), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
      out = prd$score(measures = msr(m, operation = groupdiff_tau), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
    })
  })
})

test_that("fairness measures work as expected - simulated data", {
  tsk = test_task_small()
  prds = list(pred_small())
  metrics = c("fairness.acc", "fairness.EOd", "fairness.fn", "fairness.fnr", "fairness.fp",
    "fairness.fpr", "fairness.npv", "fairness.ppv", "fairness.tpr")
  map(prds, function(prd) {
    map(metrics, function (m) {
      out = prd$score(measures = msr(m), task = tsk)
      expect_number(out, lower = 0, upper = Inf)
    })
  })
})

test_that("fairness errors on missing pta, works with", {
  df = data.frame(
    tgt = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), 'must have col_roles')
  task$col_roles$pta = "pta"
  expect_true(prd$score(msr("fairness.acc"), task = task) == 0.125)
  expect_true(prd$score(msr("fairness.fpr"), task = task) < 0.1)
  expect_true(prd$score(msr("fairness.tpr"), task = task) - 0.15 < 1e-8)
})

test_that("fairness works with non-binary pta", {
  df = data.frame(
    tgt = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,3,3,2,2,2,2,2,2,3,3))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), 'must have col_roles')
  task$col_roles$pta = "pta"
  expect_number(prd$score(msr("fairness.acc"), task = task), lower = 0, upper = 1)
  expect_number(prd$score(msr("fairness.tpr"), task = task), lower = 0, upper = 1)
})

test_that("fairness works on non-binary target", {
  df = data.frame(
    tgt = as.factor(c(1,1,2,3,1,1,2,3,3,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,3,3,2,2,2,2,2,2,3,3))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), 'must have col_roles')
  task$col_roles$pta = "pta"
  expect_number(prd$score(msr("fairness.acc"), task = task), lower = 0, upper = 1)
  expect_error(prd$score(msr("fairness.tpr"), task = task), 'needs task properties')
})

