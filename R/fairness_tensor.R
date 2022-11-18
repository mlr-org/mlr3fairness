#' @title Compute the Fairness Tensor given a Prediction and a Task
#'
#' @description
#' A fairness tensor is a list of groupwise confusion matrices.
#' 
#' @template pta
#'
#' @param object ([data.table()] | [PredictionClassif] | [ResampleResult])\cr
#'   A data.table with columns `truth` and `prediction`,
#'   a [PredictionClassif] or a [ResampleResult].
#' @param normalize (`character`)\cr
#'   How should the fairness tensor be normalized? 
#'   "all" normalizes entries by dividing by dataset size,
#'   "group" normalizes entries by dividing by group size and
#'   "none" does not conduct any normalization at all.
#' @param ... `any`\cr
#'   Currently not used.
#' @return
#'   `list()` of confusion matrix for every group in `"pta"`.
#' @export
#' @examples
#' library("mlr3")
#' task = tsk("compas")
#' prediction = lrn("classif.rpart")$train(task)$predict(task)
#' fairness_tensor(prediction, task = task)
fairness_tensor = function(object, normalize = "all", ...) {
  UseMethod("fairness_tensor")
}

#' @rdname fairness_tensor
#' @param task ([TaskClassif])\cr
#'   A [TaskClassif]. Needs `col_role` `"pta"` to be set.
#' @export
fairness_tensor.data.table = function(object, normalize = "all", task, ...) { # nolint
  assert_names(colnames(object), must.include = c("truth", "prediction"))

  dt = data.table(
    row_ids = object$row_ids %??% seq_len(nrow(object)),
    truth = object$truth,
    response = object$prediction
  )
  prd = as_prediction_classif(dt[, c("row_ids", "truth", "response")])
  fairness_tensor(prd, task = task)
}

#' @rdname fairness_tensor
#' @export
fairness_tensor.PredictionClassif = function(object, normalize = "all", task, ...) { # nolint
  assert_pta_task(task)
  assert_choice(normalize, c("all", "group", "none"))
  get_confusion = function(row_ids) {
    object$clone()$filter(row_ids)$confusion
  }

  cols = c(task$backend$primary_key, task$col_roles$pta)
  data = task$data(cols = cols)
  tensors = map(split(data, by = cols[2L], keep.by = FALSE), function(x) get_confusion(x[[1L]]))
  if (normalize == "all") {
    map(tensors, function(x) {x / nrow(data)})
  } else if (normalize == "group") {
    map(tensors, function(x) x / sum(x))
  } else if (normalize == "none") {
    return(tensors)
  }
}


#' @rdname fairness_tensor
#' @export
fairness_tensor.ResampleResult = function(object, normalize = "all", ...) { # nolint
  fairness_tensor(object$prediction(), task = object$task, ...)
}
