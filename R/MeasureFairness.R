#' @importFrom mlr3 MeasureClassif
#' @importFrom R6 R6Class
MeasureClassifFairness = R6Class("MeasureClassifFairness",
  inherit = MeasureClassif,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name) {
    info = mlr3fairness::measures[[name]] #I need to change this info to mlr3 fairness info
    super$initialize(
      id = paste0("classif.", name),
      range = c(info$lower, info$upper),
      minimize = info$minimize,
      predict_type = info$predict_type,
      task_properties = "twoclass",
      packages = "mlr3fairness",
      man = paste0("mlr3::mlr_measures_fairness_classif.", name)
    )
    self$fun = get(name, envir = asNamespace("mlr3fairness"), mode = "function")
    }
  ),
  private = list(
    .score = function(prediction, task, subcol, ...) {
      self$fun(truth = prediction$truth, response = prediction$response,
               positive = levels(prediction$truth)[1L], na_value = self$na_value,
               task = task, subcol = subcol
            )
    },

    .extra_hash = c("fun", "na_value")
  )
)

mlr3::mlr_measures$add("classif.fprb", MeasureClassifFairness, name = "fprb")
