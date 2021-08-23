# Test preparation
######################################################################
test_data = medium_test_data()
prediction = simple_rpart_prob_predictions()
benchmark_obj = simple_benchmark_result()
resample_obj = simple_resample_result()
fairness_measure = msr("fairness.acc")
fairness_measures = msrs(c("fairness.tpr", "fairness.fnr"))
######################################################################

# check plot satisfy those three conditions

# fairness_accuracy_tradeoff Tests
test_that("fairness_accuracy_tradeoff work properly with PredictionClassif", {
  check_plot(fairness_accuracy_tradeoff(prediction, fairness_measure, test_data))
})

test_that("fairness_accuracy_tradeoff work properly with ResampleResult", {
  check_plot(fairness_accuracy_tradeoff(resample_obj, fairness_measure))
})

test_that("fairness_accuracy_tradeoff work properly with BenchmarkResult", {
  check_plot(fairness_accuracy_tradeoff(benchmark_obj, fairness_measure))
})

# fairness_compare Tests
test_that("fairness_compare work properly with PredictionClassif", {
  # Single Measure
  check_plot(fairness_compare(prediction, fairness_measure, test_data))

  # Multiple Measures
  check_plot(fairness_compare(prediction, fairness_measures, test_data))
})

test_that("fairness_compare work properly with ResampleResult", {
  # Single Measure
  check_plot(fairness_compare(resample_obj, fairness_measure))

  # Multiple Measures
  check_plot(fairness_compare(resample_obj, fairness_measures))
})

test_that("fairness_compare work properly with BenchmarkResult", {
  # Single Measure
  check_plot(fairness_compare(benchmark_obj, fairness_measure))

  # Multiple Measures
  check_plot(fairness_compare(benchmark_obj, fairness_measures))
})

# fairness_prediction_density Tests
test_that("fairness_prediction_density work properly with PredictionClassif", {
  check_plot(fairness_prediction_density(prediction, test_data))
})
