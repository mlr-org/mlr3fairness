as_backend = function(id) {
  # Integrated from mlr3data subpackage
  # this looks a bit funny, but is required because of strange
  # behavior with roxygen2/pkgload
  ee = new.env(hash = FALSE, parent = emptyenv())
  data(list = id, package = "mlr3fairness", envir = ee)
  mlr3::as_data_backend(ee[[id]])
}


#' Is this task a fairness task?
#' @name is_fairness_task
#'
#' @param task [`TaskClassif`][mlr3::TaskClassif]\cr
#' The data task used for evaluation
#'
#' @return result (`logical`)
#' @export
#'
#' @examples
#' library(mlr3fairness)
#' library(mlr3)
#' task = tsk("compas")
#' is_fairness_task(task)
is_fairness_task <- function(task){
  return(!is.na(task$col_roles$pta))
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

# Conditional Binary Target Count Over Binary Protected Attribute
#
# @description
# This helper function count the conditional positive/negative cases on fairness data tasks over protected attributes
# The first element of returned scores: positive_privileged = count of positive and privileged cases
# The second element of returned scores: positive_unprivileged = count of positive and unprivileged cases
# The third element of returned scores: negative_privileged = count of negative and privileged cases
# The fourth element of returned scores: negative_unprivileged = count of negative and unprivileged cases
#
# @param data (`data.table`)\cr The data table contained in data task
# @param target (`character`)\cr The target col name of the data task
# @param pta (`character`)\cr The name of the protected attribute in data task.
# @param privileged (`factor`) or (`character`)\cr The privileged group of data task.
#
# @return c(positive_privileged, positive_unprivileged, negative_privileged, negative_unprivileged)
conditional_binary_target_pta_count <- function(data, target, pta, privileged) {
  # count grouped data and cast from long to wide
  tab = data[, .N, by = c(target, pta)]
  tab = dcast(tab, formulate(pta, target), value.var = "N", drop = FALSE)

  # reorder pta to have [privileged] in first row
  #
  # NB: cols do not need to be reordered, the positive class is always first class level
  # if the data is extracted from `task$data()`.
  order = unique(c(match(privileged, tab[[pta]]), seq_row(tab)))
  tab = tab[order]

  # convert to matrix to be able to operate via colSums() and rowSums()
  as.matrix(tab[, !pta, with = FALSE], rownames = tab[[pta]])
}

# Get the weights used for reweighing algorithm (Helper Function)
#
# @param data (`data.table`)\cr The data table contained in data task
# @param target (`character`)\cr The target col name of the data task
# @param positive (`factor`) or (`character`)\cr The positive label of data task target.
# @param pta (`character`)\cr The name of the protected attribute in data task.
# @param privileged (`factor`) or (`character`)\cr The privileged group of data task.
#
# @return matrix(c(weight_positive_privileged, weight_negative_privileged, weight_positive_unprivileged, weight_negative_unprivileged), nrow = 2)
get_reweighing_weights = function(data, target, positive, pta, privileged) {
  N_all = dim(data)[1]
  binary_classify_count = conditional_binary_target_pta_count(data, target, pta, privileged)

  N_pos_privileged = binary_classify_count[1,1]
  N_pos_unprivileged = binary_classify_count[2,1]
  N_neg_privileged = binary_classify_count[1,2]
  N_neg_unprivileged = binary_classify_count[2,2]

  N_positive = N_pos_privileged + N_pos_unprivileged
  N_negative = N_neg_privileged + N_neg_unprivileged
  N_privileged = N_pos_privileged + N_neg_privileged
  N_unprivileged = N_pos_unprivileged + N_neg_unprivileged

  W_positive_privileged = (N_positive * N_privileged)/(N_all * N_pos_privileged)
  W_negative_privileged = (N_negative * N_privileged)/(N_all * N_neg_privileged)
  W_positive_unprivileged = (N_positive * N_unprivileged)/(N_all * N_neg_unprivileged)
  W_negative_unprivileged = (N_negative * N_unprivileged)/(N_all * N_neg_unprivileged)

  return( c(W_positive_privileged, W_negative_privileged, W_positive_unprivileged, W_negative_unprivileged) )
}
