MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    fun = NULL,
    na_value = NaN,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(name) {
      info = mlr3fairness::measures[[name]]
      super$initialize(
        id = paste0("fairness.", name),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        task_properties = "twoclass",
        packages = "mlr3fairness",
        man = paste0("mlr3::mlr_measures_fairness.", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3fairness"), mode = "function")
    }
  ),

  private = list(
    .score = function(prediction, data_task, ...) {
    truth = prediction$truth
    positive = levels(truth)[1L]
    invoke(self$fun, .args = self$param_set$get_values(),
           truth = truth, response = prediction$response, prob = prediction$prob[, positive],
           positive = positive, na_value = self$na_value, data_task = data_task
           )
    }
  )
)

mlr3::mlr_measures$add("fairness.fprb", MeasureFairness, name = "fprb")
