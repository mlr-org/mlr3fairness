#' Compute metrics for non-mlr3 predictions.
#' 
#' @description 
#' Allows computing metrics for predictions that do not stem from mlr3, and were 
#' e.g. being made by models outside of mlr3.
#' Currently only `classif` and `regr` - style predictions are supported.
#' 
#' @template pta
#' 
#' @param data (`data.table`) \cr The dataset used for predicting.
#' @param target (`character`) \cr The name of the target variable. Must be available in `data`.
#' @param protected_attribute (`character`) \cr The name(s) of the protected attributes(s). Must be available in `data`.
#' @param prediction (`vector`) \cr A vector containing predictions. 
#' @param metrics (`Metric`|`list`) \cr (List of) mlr3 metrics to apply.
#' @export
#' @examples
#' library("mlr3")
#' # Get adult data as a data.table
#' train = tsk("adult_train")$data()
#' mod = rpart::rpart(target ~ ., train)
#' 
#' # Predict on test data
#' test = tsk("adult_test")$data()
#' yhat = predict(mod, test, type = "vector")
#' 
#' # Convert to a factor with the same levels
#' yhat = as.factor(yhat)
#' levels(yhat) = levels(test$target)
#' 
#' compute_metrics(
#'   data = test, 
#'   target = "target",
#'   prediction = yhat,
#'   protected_attribute = "sex",
#'   metrics = msr("fairness.acc")
#' )
compute_metrics = function(data, target, protected_attribute, prediction, metrics = NULL) {
  assert_data_frame(data)
  assert_choice(target, colnames(data))
  assert_vector(prediction)
  assert_choice(protected_attribute, colnames(data))

  if (inherits(data[[target]], "factor")) {
    t = as_task_classif(data, target = target)
  } else if (class(data[[target]]) %in% c("integer", "numeric")) {
    t = as_task_regr(data, target = target)
  } else {
    stop("compute_metrics currently only handles classif (factor) or regr (integer|numeric) targets!")
  }
  t$col_roles$pta = protected_attribute
  
  df = data.table(
    "row_ids"  = t$row_ids,
    "truth" = t$truth(),
    "response" = prediction
  )

  if (inherits(data[[target]], "factor")) {
    assert_factor(prediction, levels = t$levels(target)[[1]])
    prd = as_prediction_classif(df)
  } else if (class(data[[target]]) %in% c("integer", "numeric"))  {
    prd = as_prediction_regr(df)
  }

  prd$score(metrics, task = t)
}