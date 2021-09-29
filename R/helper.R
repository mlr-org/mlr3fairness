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
  # FIXME: Subsume when https://github.com/mlr-org/mlr3/issues/695 is fixed
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)

  return(c(msr1, msr2))
}


# score_groupwise = function(prediction, base_measure, task) {
#   # Get protected attribute vector
#   groups = task$data(cols = task$col_roles$pta, rows = prediction$row_ids)[[1]]
#   # Split prediction
#   map_dbl(split(as.data.table(prediction), groups), function(x) {
#     as_prediction()
#   })
# }