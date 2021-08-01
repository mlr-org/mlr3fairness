source(file.path(getwd(), "data_helper.R"))

# Test preparation
######################################################################
test_data = tsk("adult_train")
prediction = adult_rpart_prob_predictions()
benchmark_obj = adult_benchmark_result()
resample_obj = adult_resample_result()
fairness_measure = msr("fairness.acc")
fairness_measures = msrs(c("fairness.tpr", "fairness.fnr"))
######################################################################

# fairness_accuracy_tradeoff Tests
test_that("fairness_accuracy_tradeoff work properly with PredictionClassif", {
  expect_error(fairness_accuracy_tradeoff(prediction, fairness_measure, test_data), NA)
  expect_warning(fairness_accuracy_tradeoff(prediction, fairness_measure, test_data), NA)
})

test_that("fairness_accuracy_tradeoff work properly with ResampleResult", {
  expect_error(fairness_accuracy_tradeoff(resample_obj, fairness_measure), NA)
  expect_warning(fairness_accuracy_tradeoff(resample_obj, fairness_measure), NA)
})

test_that("fairness_accuracy_tradeoff work properly with BenchmarkResult", {
  expect_error(fairness_accuracy_tradeoff(benchmark_obj, fairness_measure), NA)
  expect_warning(fairness_accuracy_tradeoff(benchmark_obj, fairness_measure), NA)
})

# fairness_compare Tests
test_that("fairness_compare work properly with PredictionClassif", {
  # Single Measure
  expect_error(fairness_compare(prediction, fairness_measure, test_data), NA)
  expect_warning(fairness_compare(prediction, fairness_measure, test_data), NA)

  # Multiple Measures
  expect_error(fairness_compare(prediction, fairness_measures, test_data), NA)
  expect_warning(fairness_compare(prediction, fairness_measures, test_data), NA)
})

test_that("fairness_compare work properly with ResampleResult", {
  # Single Measure
  expect_error(fairness_compare(resample_obj, fairness_measure), NA)
  expect_warning(fairness_compare(resample_obj, fairness_measure), NA)

  # Multiple Measures
  expect_error(fairness_compare(resample_obj, fairness_measures), NA)
  expect_warning(fairness_compare(resample_obj, fairness_measures), NA)
})

test_that("fairness_compare work properly with BenchmarkResult", {
  # Single Measure
  expect_error(fairness_compare(benchmark_obj, fairness_measure), NA)
  expect_warning(fairness_compare(benchmark_obj, fairness_measure), NA)

  # Multiple Measures
  expect_error(fairness_compare(benchmark_obj, fairness_measures), NA)
  expect_warning(fairness_compare(benchmark_obj, fairness_measures), NA)
})

# fairness_prediction_density Tests
test_that("fairness_prediction_density work properly with PredictionClassif", {
  expect_error(fairness_prediction_density(prediction, test_data), NA)
  expect_warning(fairness_prediction_density(prediction, test_data), NA)
})
