groupwise_diff = function(prediction, base_measure, data_task, ...){
  #Assert the status for all the parameters

  subcol = data_task$col_roles$pta
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol))
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)
  return(msr1 - msr2)
}

#' @include measures.R
add_measure(groupwise_diff, "Groupwise Difference", "regr", -Inf, Inf, FALSE)
