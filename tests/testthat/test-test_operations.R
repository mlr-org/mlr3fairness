delta = 1e-15
test_data = simple_test_data()
predictions = simple_pred_data()

test_that("MeasureFairness can be created correctly", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fpr"))
  expect_true(inherits(msr_obj, "Measure"))
})

test_that("MeasureFairness can be loaded from msr library correctly", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"), operation = "groupwise_quotient")
  expect_true(inherits(msr_obj, "Measure"))
})

test_that("MeasureFairness should raise pta error for datatasks without pta", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"), operation = "groupwise_quotient")
  new_test_data = simple_test_data(need_pta = F)
  expect_error(predictions$score(msr_obj, new_test_data))
})

test_that("MeasureFairness with absolute difference operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_abs_diff")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value - 0.15) < delta)
})

test_that("MeasureFairness with difference operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_diff")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value + 0.15) < delta)
})

test_that("MeasureFairness with quotient operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_quotient")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value - 0.8) < delta)
})
