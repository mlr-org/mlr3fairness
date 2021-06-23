groupwise_abs_diff <- function(prediction, base_measure, positive, data_task, response = NULL, ...){
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
