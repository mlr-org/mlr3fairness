#' @title Compute the fairness tensor given a prediction and task.
#'
#' @description
#'   A fairness tensor is a list-of-groupwise confusion matrices.
#' @param object [`data.table`]|[`PredictionClassif`]\cr
#'   A data.table with columns `truth` and `prediction` or a [`PredictionClassif`].
#' @param task [`TaskClassif`][mlr3::TaskClassif]\cr
#'   A [`TaskClassif`]. Needs `col_role` `"pta"` to be set.
#' @param ... `any`\cr
#'   Currently not used.
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
fairness_tensor.data.table = function(object, task, ...) {
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
fairness_tensor.PredictionClassif = function(object, task, ...) {
  assert_pta_task(task)
  ft = map(
    split(task$data(cols = c(task$backend$primary_key))[[1]], task$data(cols = task$col_roles$pta)),
    function(rows) {
      object$clone()$filter(rows)$confusion
  })
  return(ft)
}