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
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol))
  prediction = map(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)

  return(c(msr1, msr2))
}

# Conditional Positive Score Over Protected Attributes
#
# @description
# This helper function count the conditional positive/negative cases on fairness data tasks over protected attributes
# The first element of returned scores: positive_privileged = count of positive and privileged cases
# The second element of returned scores: positive_unprivileged = count of positive and unprivileged cases
# The third element of returned scores: negative_privileged = count of negative and privileged cases
# The fourth element of returned scores: negative_unprivileged = count of negative and unprivileged cases
#
#
# @param task (`TaskClassif()`)\cr The data task with protected attributes. (Fairness Dataset)
# @param data (`data.table`)\cr The data table contained in data task
# @param positive (`character`)\cr The positive label of data task target.
# @param pta (`character`)\cr The name of the protected attribute in data task.
# @param privileged (`factor`) or (`character`)\cr The privileged group of data task.
#
# @return c(positive_privileged, positive_unprivileged, negative_privileged, negative_unprivileged)
binary_classif_pta_count <- function(task, data, positive, pta, privileged) {
  tar = task$target_names

  N_all = dim(data)[1]
  privileged_data = data[get(pta) == privileged]
  unprivileged_data = data[get(pta) != privileged]

  positive_privileged = dim(privileged_data[get(tar) == positive])[1]
  positive_unprivileged = dim(unprivileged_data[get(tar) == positive])[1]
  negative_privileged = dim(privileged_data)[1] - positive_privileged
  negative_unprivileged = dim(unprivileged_data)[1] - negative_privileged

  return( c(positive_privileged, positive_unprivileged, negative_privileged, negative_unprivileged) )
}
