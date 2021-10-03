# Test preparation
######################################################################
  lrns  = list(lrn("classif.rpart", predict_type = "prob"), lrn("classif.featureless", predict_type = "prob"))
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

test_that("compare_metrics", {
  # BMR
  check_plots(compare_metrics(bmr, fairness_measures))

  # RR
  map(bmr$resample_results$resample_result, function(rr) {
    check_plots(compare_metrics(rr, fairness_measures[[1]]))
  })

  # PRDS
  map(bmr$resample_results$resample_result[1:2], function(rr) {
    map(rr$predictions(), function(prd) {
       check_plots(compare_metrics(prd, fairness_measures[[1]], tasks[[1]]))
    })
  })
})


test_that("fairness_accuracy_tradeoff", {
  # BMR
  check_plots(fairness_prediction_density(bmr))

  # RR
  map(bmr$resample_results$resample_result, function(rr) {
    check_plots(fairness_prediction_density(rr))
  })

  # PRDS
  map(bmr$resample_results$resample_result[1:2], function(rr) {
    map(rr$predictions(), function(prd) {
       check_plots(fairness_prediction_density(prd, tasks[[1]]))
    })
  })
})
