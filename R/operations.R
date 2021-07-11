#' Groupwise Absolute Differenence (Fairness Metrics)
#' @name groupwise_abs_diff
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param data_task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Absolute difference of base measues between binary protected groups.
groupwise_abs_diff <- function(prediction, base_measure, data_task, ...) {
  assert_binary_pta(data_task, "groupwise_abs_diff")

  measure_list = binary_measure_score(prediction, base_measure, data_task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  return(abs(msr1 - msr2))
}

#' Groupwise Differenence (Fairness Metrics)
#' @name groupwise_diff
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param data_task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Difference of base measues between binary protected groups.
groupwise_diff = function(prediction, base_measure, data_task, ...) {
  assert_binary_pta(data_task, "groupwise_diff")

  measure_list = binary_measure_score(prediction, base_measure, data_task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  return(msr1 - msr2)
}

#' Groupwise Quotient (Fairness Metrics)
#' @name groupwise_quotient
#'
#' @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
#' @param base_measure (`Measure()`)\cr The base measures used to evaluate.
#' @param data_task (`TaskClassif()`)\cr The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Quotient of base measues between binary protected groups.
groupwise_quotient = function(prediction, base_measure, data_task, ...) {
  assert_binary_pta(data_task, "groupwise_quotient")

  measure_list = binary_measure_score(prediction, base_measure, data_task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  #Do we need a value check to avoid msr2 = 0 (numerical error)?
  return(msr1/msr2)
}
