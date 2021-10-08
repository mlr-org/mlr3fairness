test_that("fairness measures work as expcted", {
  tsk = tsk("compas")
  prds = list(
    lrn("classif.rpart")$train(tsk)$predict(tsk),
    lrn("classif.rpart", predict_type = "prob")$train(tsk)$predict(tsk),
    lrn("classif.featureless", predict_type = "prob")$train(tsk)$predict(tsk)
  )
  metrics = c("fairness.acc", "fairness.eod", "fairness.fn", "fairness.fnr", "fairness.fp",
    "fairness.fpr", "fairness.npv", "fairness.ppv", "fairness.tpr")
  map(prds, function(prd) {
    map(metrics, function (m) {
      out = prd$score(measures = msr(m), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
      out = prd$score(measures = msr(m, operation = groupdiff_tau), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
      out = prd$score(measures = msr(m, operation = groupdiff_absdiff), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
    })
  })
})

test_that("fairness measures work as expected - simulated data", {
  tsk = test_task_small()
  prds = list(pred_small())
  metrics = c("fairness.acc", "fairness.eod", "fairness.fn", "fairness.fnr", "fairness.fp",
    "fairness.fpr", "fairness.npv", "fairness.ppv", "fairness.tpr")
  map(prds, function(prd) {
    map(metrics, function (m) {
      out = prd$score(measures = msr(m), task = tsk)
      expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
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
  expect_warning(expect_warning(expect_warning(prd$score(msr("fairness.tpr"), task = task), 'is missing properties')))
})


delta = 1e-15
test_data = test_task_small()
predictions = pred_small()

test_that("fairness.fpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fpr"))
  expect_true( round(predictions$score(msr_obj, test_data),4) == 0.0833 )
})

test_that("fairness.acc can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.acc"))
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.125) < delta )
})

test_that("fairness.fnr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fnr"))
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.15) < delta )
})

test_that("fairness.tpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.tpr"))
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.15) < delta )
})

test_that("fairness.ppv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"))
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.25) < delta )
})

test_that("fairness.npv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.npv"))
  expect_true(abs(predictions$score(msr_obj, test_data) - 0) < delta )
})

test_that("fairness.fp can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fp"))
  expect_true( predictions$score(msr_obj, test_data) == 1 )
})

test_that("fairness.fn can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fn"))
  expect_true( predictions$score(msr_obj, test_data) == 0)
})

test_that("fairness.pp (disparate impact score) can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.pp"))
  expect_true(predictions$score(msr_obj, test_data) == 0)
})

test_that("fairness. composite no id", {
  msr_obj = msr("fairness.composite", measures = msrs(c("classif.fpr", "classif.fnr")))
  expect_true(msr_obj$id == "fairness.fpr_fnr")
})
