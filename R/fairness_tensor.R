#' @title Compute the Fairness Tensor given a Prediction and a Task
#'
#' @description
#' A fairness tensor is a list of groupwise confusion matrices.
#'
#' @param object ([data.table()] | [PredictionClassif] | [ResampleResult])\cr
#'   A data.table with columns `truth` and `prediction`,
#'   a [PredictionClassif] or a [ResampleResult].
#' @param ... `any`\cr
#'   Currently not used.
#' @return
#'   `list()` of confusion matrix for every group in `"pta"`.
#' @export
#' @examples
#' task = tsk("compas")
#' prediction = lrn("classif.rpart")$train(task)$predict(task)
#' fairness_tensor(prediction, task)
fairness_tensor = function(object, ...) {
  UseMethod("fairness_tensor")
}

#' @rdname fairness_tensor
#' @param task ([TaskClassif])\cr
#'   A [TaskClassif]. Needs `col_role` `"pta"` to be set.
#' @export
fairness_tensor.data.table = function(object, task, ...) { # nolint
  assert_names(colnames(object), must.include = c("truth", "prediction"))

  dt = data.table(
    row_ids = object$row_ids %??% seq_len(nrow(object)),
    truth = object$truth,
    response = object$prediction
  )
  prd = as_prediction_classif(dt[, c("row_ids", "truth", "response")])
  fairness_tensor(prd, task)
}

#' @rdname fairness_tensor
#' @export
fairness_tensor.PredictionClassif = function(object, task, ...) { # nolint
  assert_pta_task(task)
  get_confusion = function(row_ids) {
    object$clone()$filter(row_ids)$confusion
  }

  cols = c(task$backend$primary_key, task$col_roles$pta)
  data = task$data(cols = cols)
  map(split(data, by = cols[2L], keep.by = FALSE), function(x) get_confusion(x[[1L]]))
}


#' @rdname fairness_tensor
#' @export
fairness_tensor.ResampleResult = function(object, ...) { # nolint
  fairness_tensor(object$prediction(), task = object$task, ...)
}
