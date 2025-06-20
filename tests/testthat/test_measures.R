test_that("measure constructors work", {
  m = MeasureFairness$new(base_measure = msr("classif.acc"))
  expect_equal(m$id, "fairness.acc")
  expect_equal(m$base_measure, msr("classif.acc"))
  expect_equal(m$range, c(-Inf, Inf))
  expect_equal(m$task_type, "classif")

  m = MeasureFairness$new(base_measure = msr("classif.fpr"), range = c(0, 100), id = "test.it")
  expect_equal(m$id, "test.it")
  expect_equal(m$base_measure, msr("classif.fpr"))
  expect_equal(m$range, c(0, 100))
  expect_equal(m$task_type, "classif")

  m = MeasureFairness$new(base_measure = msr("classif.fpr"), range = c(0, 100), operation = function(x) 5)
  expect_true(m$operation(1) == 5)

  m = MeasureFairness$new(base_measure = msr("regr.mse"), range = c(0, 100))
  expect_equal(m$id, "fairness.mse")
  expect_equal(m$base_measure, msr("regr.mse"))
  expect_equal(m$range, c(0, 100))
  expect_equal(m$task_type, "regr")
})

test_that("dictionary constructors work", {
  # Construction in zzz.R
  m = msr("fairness.acc")
  expect_equal(m$base_measure, msr("classif.acc"))
  expect_equal(m$range, c(0, 1))
  expect_equal(m$task_type, "classif")

  # Construction from base measure
  m = msr("fairness", base_measure = msr("classif.acc"), range = c(0, 1))
  expect_equal(m$base_measure, msr("classif.acc"))
  expect_equal(m$range, c(0, 1))
  expect_equal(m$task_type, "classif")
})

test_that("fairness measures work as expcted", {
  skip_if_not_installed("rpart")
  tsk = tsk("compas")
  prds = list(
    lrn("classif.rpart")$train(tsk)$predict(tsk),
    lrn("classif.rpart", predict_type = "prob")$train(tsk)$predict(tsk),
    lrn("classif.featureless", predict_type = "prob")$train(tsk)$predict(tsk)
  )
  metrics = mlr_measures_fairness$key
  for (prd in prds) {
    for (m in metrics) {
      ms = msr(m)
      print(ms$id)
      if (ms$task_type == "classif" && is(ms, "MeasureFairness")) {
        out = prd$score(measures = ms, task = tsk)
        expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
        out = prd$score(measures = msr(m, operation = groupdiff_tau), task = tsk)
        expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
        out = prd$score(measures = msr(m, operation = groupdiff_absdiff), task = tsk)
        expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
      }
    }
  }
})

test_that("fairness measures work as expected - simulated data", {
  tsk = test_task_small()
  prds = list(pred_small())
  metrics = mlr_measures_fairness$key

  for (prd in prds) {
    for (m in metrics) {
      ms = msr(m)
      if (ms$task_type == "classif" & is(ms, "MeasureFairness")) {
        out = prd$score(measures = ms, task = tsk)
        expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
      }
    }
  }
})

test_that("fairness errors on missing pta, works with", {
  df = data.frame(
    tgt = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    variable = c(3, 1, 4, 8, 5, 41, 22, 3, 4, 29, 2, 13, 4, 26, 2, 34),
    pta = as.factor(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    response = as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), "role 'pta'", fixed = TRUE)
  task$col_roles$pta = "pta"
  expect_equal(unname(prd$score(msr("fairness.acc"), task = task)), 0.125)
  expect_lt(prd$score(msr("fairness.fpr"), task = task), 0.1)
  expect_lt(prd$score(msr("fairness.tpr"), task = task) - 0.15, 1e-8)
})

test_that("fairness works with non-binary pta", {
  df = data.frame(
    tgt = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    variable = c(3, 1, 4, 8, 5, 41, 22, 3, 4, 29, 2, 13, 4, 26, 2, 34),
    pta = as.factor(c(1, 1, 1, 1, 1, 1, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    response = as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), "role 'pta'", fixed = TRUE)
  task$col_roles$pta = "pta"
  expect_number(prd$score(msr("fairness.acc"), task = task), lower = 0, upper = 1)
  expect_number(prd$score(msr("fairness.tpr"), task = task), lower = 0, upper = 1)
})

test_that("fairness works on non-binary target", {
  df = data.frame(
    tgt = as.factor(c(1, 1, 2, 3, 1, 1, 2, 3, 3, 2, 2, 1, 1, 1, 2, 1)),
    variable = c(3, 1, 4, 8, 5, 41, 22, 3, 4, 29, 2, 13, 4, 26, 2, 34),
    pta = as.factor(c(1, 1, 1, 1, 1, 1, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3))
  )
  task = mlr3::TaskClassif$new("example", df, target = "tgt")
  prd = mlr3::PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    response = as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  )
  expect_error(prd$score(msr("fairness.acc"), task = task), "role 'pta'", fixed = TRUE)
  task$col_roles$pta = "pta"
  expect_number(prd$score(msr("fairness.acc"), task = task), lower = 0, upper = 1)
  suppressWarnings(expect_warning(prd$score(msr("fairness.tpr"), task = task), "is missing properties"))
})


delta = 1e-15
test_data = test_task_small()
predictions = pred_small()

test_that("fairness.fpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fpr"))
  expect_equal(unname(round(predictions$score(msr_obj, test_data), 4)), 0.0833)
})

test_that("fairness.acc can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.acc"))
  expect_lt(abs(predictions$score(msr_obj, test_data) - 0.125), delta)
})

test_that("fairness.fnr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fnr"))
  expect_lt(abs(predictions$score(msr_obj, test_data) - 0.15), delta)
})

test_that("fairness.tpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.tpr"))
  expect_lt(abs(predictions$score(msr_obj, test_data) - 0.15), delta)
})

test_that("fairness.ppv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"))
  expect_lt(abs(predictions$score(msr_obj, test_data) - 0.25), delta)
})

test_that("fairness.npv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.npv"))
  expect_lt(abs(predictions$score(msr_obj, test_data) - 0), delta)
})

test_that("fairness.fp can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fp"))
  expect_equal(unname(predictions$score(msr_obj, test_data)), 1)
})

test_that("fairness.fn can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fn"))
  expect_equal(unname(predictions$score(msr_obj, test_data)), 0)
})

test_that("fairness.pp (disparate impact score) can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.pp"))
  expect_equal(unname(predictions$score(msr_obj, test_data)), 0)
})

test_that("fairness.composite no id", {
  msr_obj = msr("fairness.composite", measures = msrs(c("classif.fpr", "classif.fnr")))
  expect_equal(msr_obj$id, "fairness.fpr_fnr")
})

test_that("fairness constraint measures - simulated data", {
  tsk = test_task_small()
  prds = list(pred_small())
  metrics = c("fairness.acc", "fairness.eod")
  map(prds, function(prd) {
    map_dbl(metrics, function(m) {
      fair = prd$score(measures = msr(m), task = tsk)
      perf = prd$score(measures = msr("classif.acc"), task = tsk)
      mm = msr("fairness.constraint", performance_measure = msr("classif.acc"), fairness_measure = msr(m), epsilon = Inf)
      out = prd$score(measures = mm, task = tsk)
      expect_true(out == perf)
      mm = msr("fairness.constraint", performance_measure = msr("classif.acc"), fairness_measure = msr(m), epsilon = 0)
      out = prd$score(measures = mm, task = tsk)
      expect_true(out == 0 - fair)
      perf = prd$score(measures = msr("classif.ce"), task = tsk)
      mm = msr("fairness.constraint", performance_measure = msr("classif.ce"), fairness_measure = msr(m), epsilon = 1)
      out = prd$score(measures = mm, task = tsk)
      expect_true(out == perf)
      mm = msr("fairness.constraint", performance_measure = msr("classif.ce"), fairness_measure = msr(m), epsilon = 0)
      out = prd$score(measures = mm, task = tsk)
      expect_true(out == 1 + fair)
    })
  })
})

test_that("Args are passed on correctly", {
  skip_if_not_installed("rpart")
  MeasureTestArgs = R6::R6Class("MeasureTestArgs",
    inherit = mlr3::Measure,
    public = list(
      initialize = function() {
        private$.args = list(train_set = 1:10, learner = NULL)
        super$initialize(
          id = "classif.testargs",
          predict_type = "response",
          range = c(0, 1),
          minimize = TRUE,
          task_type = "classif"
        )
      }
    ),
    private = list(
      .args = NULL,
      .score = function(prediction, task, ...) {
        args = list(...)
        pmap(list(args[names(private$.args)], private$.args), function(x, y) {
          expect_equal(x, y)
        })
        return(1)
      }
    )
  )

  mta = MeasureTestArgs$new()
  t = suppressWarnings(tsk("compas"))
  l = lrn("classif.rpart")
  prd = l$train(t)$predict(t)
  prd$score(mta, task = t, train_set = 1:10)
  expect_error(prd$score(mta, task = t, train_set = 1:2))

  mfa = msr("fairness", base_measure = mta)
  prd$score(mfa, task = t, train_set = 1:10)
  prd$score(groupwise_metrics(mta, t), task = t, train_set = 1:10)
  prd$score(msr("fairness.constraint", fairness_measure = mta, performance_measure = mta), task = t, train_set = 1:10)
})


test_that("fairness measures work as expected - simulated data", {
  tsks = list(
    test_task_intersect("classif"),
    test_task_multipta("classif"),
    test_task_multicl("classif"),
    test_task_contpta("classif")
  )
  lrn = lrn("classif.featureless")

  metrics = mlr_measures_fairness$key
  for (tsk in tsks) {
    prd = lrn$train(tsk)$predict(tsk)
    for (m in metrics) {
      ms = msr(m)
      if (ms$task_type == "classif" & is(ms, "MeasureFairness")) {
        if (tsk$properties == "twoclass") {
          out = prd$score(measures = ms, task = tsk)
          expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
        }
        if (tsk$properties == "multiclass") {
          if ("twoclass" %in% ms$base_measure$task_properties) {
            suppressWarnings(expect_warning(prd$score(measures = ms, task = tsk)))
          } else {
            out = prd$score(measures = ms, task = tsk)
            expect_number(out, lower = 0, upper = Inf, na.ok = TRUE)
          }
        }
      }
    }
  }
})
