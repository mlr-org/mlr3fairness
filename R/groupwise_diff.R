#' Groupwise Differenence (Fairness Metrics)
#' @name groupwise_diff
#'
#' @param prediction The predictions of the learner.
#' @param base_measure The base measures used to evaluate.
#' @param data_task The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Difference of base measues between binary protected groups.
#' @export
groupwise_diff = function(prediction, base_measure, data_task, ...) {
  #Assert the status for all the parameters

  measure_list = binary_measure_score(prediction, base_measure, data_task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  return(msr1 - msr2)
}
