#' Groupwise Quotient (Fairness Metrics)
#' @name groupwise_quotient
#'
#' @param prediction The predictions of the learner.
#' @param base_measure The base measures used to evaluate.
#' @param data_task The data task for the fairness metric.
#' @param ... Additional parameters
#'
#' @return Quotient of base measues between binary protected groups.
#' @export
#'
groupwise_quotient = function(prediction, base_measure, data_task, ...){
  #Assert the status for all the parameters

  subcol = data_task$col_roles$pta
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol))
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)
  #Do we need a value check to avoid msr2 = 0 (numerical error)?
  return(msr1/msr2)
}

#' @include measures.R
add_measure(groupwise_quotient, "Groupwise Quotient", "regr", -Inf, Inf, FALSE)
