# Test preparation
######################################################################
  lrns  = list(lrn("classif.rpart"), lrn("classif.featureless"))
  tasks = list(tsk("adult_train"), tsk("compas"))
  fairness_measures = msrs(c("fairness.tpr", "fairness.fnr"))
  bmr = benchmark(benchmark_grid(tasks = tasks, learners = lrns, rsmp("cv", folds = 3L)))
######################################################################

# fairness_accuracy_tradeoff Tests
test_that("fairness_accuracy_tradeoff", {
  # BMR
  map(fairness_measures, function(fmsr) {
    check_plots(fairness_accuracy_tradeoff(bmr, fmsr))
  })

  # RR
  map(bmr$resample_results$resample_result, function(rr) {
    check_plots(fairness_accuracy_tradeoff(rr, fairness_measures[[1]]))
  })

  # PRDS
  map(bmr$resample_results$resample_result[1:2], function(rr) {
    map(rr$predictions(), function(prd) {
       check_plots(fairness_accuracy_tradeoff(prd, fairness_measures[[1]], task = tasks[[1]]))
    })
  })
})


test_that("fairness_accuracy_tradeoff", {
  # BMR
  check_plots(fairness_compare(bmr, fairness_measures))

  # RR
  map(bmr$resample_results$resample_result, function(rr) {
    check_plots(airness_compare(rr, fairness_measures[[1]]))
  })

  # PRDS
  map(bmr$resample_results$resample_result[1:2], function(rr) {
    map(rr$predictions(), function(prd) {
       check_plots(fairness_compare(prd, fairness_measures[[1]], tasks[[1]]))
    })
  })
})

# test_that("fairness_accuracy_tradeoff work properly with ResampleResult", {
#   check_plot(fairness_accuracy_tradeoff(resample_obj, fairness_measure))
# })

# test_that("fairness_accuracy_tradeoff work properly with BenchmarkResult", {
#   check_plot(fairness_accuracy_tradeoff(benchmark_obj, fairness_measure))
# })

# # fairness_compare Tests
# test_that("fairness_compare work properly with PredictionClassif", {
#   # Single Measure
#   check_plot(fairness_compare(prediction, fairness_measure, test_data))

#   # Multiple Measures
#   check_plot(fairness_compare(prediction, fairness_measures, test_data))
# })

# test_that("fairness_compare work properly with ResampleResult", {
#   # Single Measure
#   check_plot(fairness_compare(resample_obj, fairness_measure))

#   # Multiple Measures
#   check_plot(fairness_compare(resample_obj, fairness_measures))
# })

# test_that("fairness_compare work properly with BenchmarkResult", {
#   # Single Measure
#   check_plot(fairness_compare(benchmark_obj, fairness_measure))

#   # Multiple Measures
#   check_plot(fairness_compare(benchmark_obj, fairness_measures))
# })

# # fairness_prediction_density Tests
# test_that("fairness_prediction_density work properly with PredictionClassif", {
#   check_plot(fairness_prediction_density(prediction, test_data))
# })
