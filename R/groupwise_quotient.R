#' Groupwise Quotient (Fairness Metrics)
#' @name groupwise_quotient
#'
#' @param prediction The predictions of the learner.
#' @param base_measure The base measures used to evaluate.
#' @param data_task The data task for the fairness metric.
#' @param ... Further arguments, currently ignored.
#'
#' @return Quotient of base measues between binary protected groups.
#' @export
groupwise_quotient = function(prediction, base_measure, data_task, ...) {
  #Assert the status for all the parameters

  measure_list = binary_measure_score(prediction, base_measure, data_task)
  msr1 = measure_list[1]
  msr2 = measure_list[2]

  #Do we need a value check to avoid msr2 = 0 (numerical error)?
  return(msr1/msr2)
}
