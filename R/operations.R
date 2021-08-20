#' Groupwise Differenence (Fairness Metrics)
#' @name groupwise_diff
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Difference of base measues between binary protected groups.
groupwise_diff = function(prediction, base_measure, task, ...) {
  assert_binary_pta(task, "groupwise_diff")

  measure_list = binary_measure_score(prediction, base_measure, task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  return(msr1 - msr2)
}

#' Groupwise Absolute Differenence (Fairness Metrics)
#' @name groupwise_abs_diff
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Absolute difference of base measues between binary protected groups.
groupwise_abs_diff <- function(prediction, base_measure, task, ...) {
  groupwise_diff_val = groupwise_diff(prediction, base_measure, task)
  return(abs(groupwise_diff_val))
}

#' Groupwise Quotient (Fairness Metrics)
#' @name groupwise_quotient
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Quotient of base measues between binary protected groups.
groupwise_quotient = function(prediction, base_measure, task, ...) {
  assert_binary_pta(task, "groupwise_quotient")

  measure_list = binary_measure_score(prediction, base_measure, task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  #Do we need a value check to avoid msr2 = 0 (numerical error)?
  return(msr1/msr2)
}
