#' Groupwise Absolute Differenence (Fairness Metrics)
#' @name groupwise_abs_diff
#'
#' @param prediction The predictions of the learner.
#' @param base_measure The base measures used to evaluate.
#' @param data_task The data task for the fairness metric.
#' @param ...
#'
#' @return Absolute difference of base measues between binary protected groups.
#' @export
#'
groupwise_abs_diff <- function(prediction, base_measure, data_task, ...){
  #Assert the status for all the parameters

  subcol = data_task$col_roles$pta
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol))
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)
  return(abs(msr1 - msr2))
}

#' @include measures.R
add_measure(groupwise_abs_diff, "Groupwise Absolute Difference", "regr", 0, Inf, FALSE)
