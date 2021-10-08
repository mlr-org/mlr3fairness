as_backend = function(id) {
  # Integrated from mlr3data subpackage
  # this looks a bit funny, but is required because of strange
  # behavior with roxygen2/pkgload
  ee = new.env(hash = FALSE, parent = emptyenv())
  data(list = id, package = "mlr3fairness", envir = ee)
  mlr3::as_data_backend(ee[[id]])
}


# Score weights per group (indicated by 'pta')
# @param prediction [Prediction] A prediction
# @param base_measure [Measure] The measure to compute in each group.
# @param task [Task] the task prediction was made on.
# @return [numeric] Computed score
score_groupwise = function(prediction, base_measure, task) {
  # Get protected attribute vector
  groups = task$data(cols = task$col_roles$pta, rows = prediction$row_ids)[[1]]

  # Split prediction
  map_dbl(split(prediction$row_ids, groups), function(rws) {
    prediction$clone()$filter(rws)$score(base_measure, task = task)
  })
}

# Compute weights for PipeOpReweighing*
# @param task [Task] the task
# @param alpha [numeric] Debiasing strength
# @return [data.table] A data.table with counts and weights for each feature.
compute_reweighing_weights = function (task, alpha = 1) {
  dt = task$data(cols = c(task$target_names, task$col_roles$pta))
  tab = as.data.table(table(dt))
  tab[, n_tgt := sum(N), by = eval(task$target_names)]
  tab[, n_pta := sum(N), by = eval(task$col_roles$pta)]
  tab[, wt := (n_tgt * n_pta) / (sum(N) * N)]
  tab[, wt := (1-alpha) * wt + alpha * wt]
  return(tab)
}

# Same as task$filter(), but allows duplicate row IDs
# @param task [Task] the task
# @param row_ids [numeric] the row IDs to select
# @return [Task] the modified task
task_filter_ex = function(task, row_ids) {
  addedrows = row_ids[duplicated(row_ids)]
  newrows = task$nrow + seq_along(addedrows)
  if (length(addedrows)) {
    task$rbind(task$data(rows = addedrows))
  }
  # row ids can be anything, we just take what mlr3 happens to assign.
  row_ids[duplicated(row_ids)] = task$row_ids[newrows]
  task$filter(row_ids)
}

#' @title Compute the fairness tensor given a prediction and task.
#'
#' @description
#'   A fairness tensor is a list-of-groupwise confusion matrices.
#' @param object [`data.table`|`PredictionClassif`]\cr
#'   A data.table with columns `truth` and `prediction` or a `PredictionClassif`.
#' @param task [`TaskClassif`]\cr
#'   A [`TaskClassif`]. Needs `col_role` `"pta"` to be set.
#' @return
#'   `list` of confusion matrix for every group in `"pta"`.
#' @examples
#' library(mlr3)
#' t = tsk("compas")
#' prd = lrn("classif.rpart")$train(t)$predict(t)
#' fairness_tensor(prd, t)
#' @export
fairness_tensor = function(object, task, ...){
  UseMethod("fairness_tensor")
}

#' @export
fairness_tensor.data.table = function(object, task) {
  assert_true(all(c("truth", "prediction") %in% colnames(object)))
  dt = data.table(
    row_ids = seq_len(nrow(object)),
    truth = object$truth,
    response = object$prediction
  )
  prd = as_prediction_classif(dt[, c("row_ids", "truth", "response")])
  fairness_tensor(prd, task)
}

#' @export
fairness_tensor.PredictionClassif = function(object, task) {
  assert_pta_task(task)
  ft = map(
    split(task$data(cols = c(task$backend$primary_key))[[1]], task$data(cols = task$col_roles$pta)),
    function(rows) {
      object$clone()$filter(rows)$confusion
  })
  return(ft)
}