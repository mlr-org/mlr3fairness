#' @importFrom mlr3measures fpr
fprb <- function(truth, response, positive, task, subcol, ...){
  #Assert the status for all the parameters

  subgroup_truth = split(as.data.table(truth), task$data(cols = subcol))
  subgroup_response = split(as.data.table(response), task$data(cols = subcol))
  print(typeof(subgroup_truth[[1]]))
  fp_1 = fpr(subgroup_truth[[1]], subgroup_response[[1]], positive = positive)
  fp_2 = fpr(subgroup_truth[[2]], subgroup_response[[2]], positive = positive)
  return(abs(fp_1 - fp_2))
}

#' @include measures.R
add_measure(fprb, "False Positive Rate Bias", "binary", 0, 1, FALSE)
