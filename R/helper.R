as_backend = function(id) {
  # Integrated from mlr3data subpackage
  # this looks a bit funny, but is required because of strange
  # behavior with roxygen2/pkgload
  ee = new.env(hash = FALSE, parent = emptyenv())
  data(list = id, package = "mlr3fairness", envir = ee)
  mlr3::as_data_backend(ee[[id]])
}

# Binary Measure Score
#
# @description
# This helper function returns a list of 2 measure scores based on a binary protected feature in datatask.
#
# @param prediction (`PredictionClassif()`)\cr The predictions of the learner.
# @param base_measure (`Measure()`)\cr The base measures used to evaluate.
# @param data_task (`TaskClassif()`)\cr The data task for the fairness metric.
#
# @return c(measure_score_1, measure_score_2)
binary_measure_score = function(prediction, base_measure, data_task){
  subcol = data_task$col_roles$pta
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol, rows = prediction$row_ids)[[1]])
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)

  return(c(msr1, msr2))
}

# Get Measures Name
# @description
# This helper function returns name of msr or msrs
#
# @param object one single measure (msr) or multiple measures (msrs)
get_msrs_name <- function(object){
  if( any(class(object) == "Measure") ) { return(object$id) }

  result = rep(NA, length(object))
  for(i in c(1:length(object))){
    result[i] = object[[i]]$id
  }
  return(result)
}
