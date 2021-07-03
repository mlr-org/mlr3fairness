#' @title Fairness Measure
#' @name mlr_measures_fairness
#'
#' @description
#' This measure specializes [Measure] to allow for measuring statistical group fairness:
#' A common approach to quantifying a model's fairness is to compute the difference between a protected and an unprotected group according to some performance metric such as `classification error`, `false positive rate` or others.
#' This measure allows for the composition of a fairness-metric from such a `base_measure` by computing group-wise difference according to some strategy.
#' Several options wrt. computing difference such as [groupwise_abs_diff()], [groupwise_diff()], or [groupwise_quotient()] are available.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' Predefined operations can be found in the [mlr3fairness].
#'
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' library(mlr3)
#' data_task = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(data_task)
#' measure = MeasureFairness$new("groupwise_quotient", base_measure = msr("classif.ppv"))
#' predictions$score(measure, task = data_task)
#'
#' @export
MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @template field_base_measure
    base_measure = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param base_measure (`Measure()`)\cr
    #' The measure used to perform fairness operations.
    #'
    #' @param operation (`character()`)\cr
    #' The operation name performed on the base measures.
    #' * Possible inputs should be one of:
    #'   - "groupwise_abs_diff"
    #'   - "groupwise_diff"
    #'   - "groupwise_quotient"
    initialize = function(operation = "groupwise_abs_diff", base_measure) {
      assert_choice(operation, c("groupwise_abs_diff", "groupwise_diff", "groupwise_quotient"))
      private$fun = get(operation, envir = asNamespace("mlr3fairness"), mode = "function")
      self$base_measure = assert_measure(base_measure)

      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = c(-Inf, Inf),
        properties = "reqiures_task",
        minimize = TRUE,
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness"
      )
    }
  ),

  private = list(
    fun = NULL,

    .score = function(prediction, task, ...) {
      invoke(private$fun, prediction = prediction, data_task = task, base_measure = self$base_measure)
    }
  )
)

mlr_measures$add("fairness", MeasureFairness, name = "fairness")
