#--- Prepare
tasks = test_tasks()
fairness_measures = test_measures()
bmr = test_bmr()
#---

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

test_that("prediction_density", {
  # BMR
  check_plots(fairness_prediction_density(bmr))
  check_plots(fairness_prediction_density(bmr, type = "violin"))

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
