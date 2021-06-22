fprb <- function(truth, response, positive, data_task, ...){
  #Assert the status for all the parameters

  subcol = data_task$col_roles$pta
  subgroup_truth = embedded_to_factor(split(as.data.table(truth), data_task$data(cols = subcol)))
  subgroup_response = embedded_to_factor(split(as.data.table(response), data_task$data(cols = subcol)))
  fp_1 = fpr(subgroup_truth[[1]], subgroup_response[[1]], positive = positive)
  fp_2 = fpr(subgroup_truth[[2]], subgroup_response[[2]], positive = positive)
  return(abs(fp_1 - fp_2))
}


embedded_to_factor <- function(embedded_list){
  for(j in c(1, length(embedded_list))){
    embedded_list[[j]] = lapply(subgroup_truth[[j]], as.factor)
  }
  return(embedded_list)
}

#' @include measures.R
add_measure(fprb, "False Positive Rate Bias", "binary", 0, Inf, FALSE)
