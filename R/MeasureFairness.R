#' @title Fairness Measure
#' @name mlr_measures_fairness
#'
#' @description
#' This measure specializes [mlr3::Measure()] to allow for measuring statistical group fairness:
#' A common approach to quantifying a model's fairness is to compute the difference between a protected and an
#' unprotected group according to some performance metric such as `classification error`,
#' `false positive rate` or others.
#' This measure allows for the composition of a fairness-metric from such a `base_measure` by computing
#' group-wise difference. This difference metric can be specified using the  'operation' parameter.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' @seealso groupdiff_tau
#'
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' library(mlr3)
#' data_task = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(data_task)
#' measure = msr("fairness", base_measure = msr("classif.ppv"))
#' predictions = learner$predict(data_task)
#' predictions$score(measure, task = data_task)
MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @template field_base_measure
    base_measure = NULL,
    #' @template field_operation
    operation = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param base_measure (`Measure()`)\cr
    #' The measure used to perform fairness operations.
    #'
    #' @param operation (`function`)\cr
    #' The operation used to compute the difference. A function with args 'x' and 'y' that returns
    #' a single value. Defaults to `abs(x - y)`.
    #' @param minimize (`logical`)\cr
    #' Should the measure be minimized?
    #'
    initialize = function(base_measure, operation = function(x, y) {abs(x - y)}, minimize = TRUE, range = c(-Inf, Inf)) {
      self$operation = assert_function(observation, nargs = 2)
      self$base_measure = assert_measure(base_measure)

      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = assert_numeric(range, len = 2),
        properties = "requires_task",
        minimize = assert_flag(minimize),
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness"
      )
    }
  ),
  private = list(
    .score = function(prediction, task, ...) {
      mvals = binary_measure_score(prediction, base_measure, data_task)
      invoke(self$operation, mvals)
    }
  )
)

mlr_measures$add("fairness", MeasureFairness)

#' @title Fairness Measures in mlr3
#' @name mlr_measures_fairness
#'
#' @section Predefined measures:
#' `mlr3fairness` comes with a set of predefined fairness measures listed below.
#' For full flexibility, [`MeasureFairness`] can be used to construct classical
#' group fairness measures based on a difference between a performance metrics across groups
#' by combining a performance measure with an operation for measuring differences.
#'
#' A set of widely used metrics is included in mlr3fairness.
#' * `fairness.fpr` :: Abs. difference in false positive rates across groups (False positive error rate balance/Part of Equalized Odds)
#' * `fairness.acc` :: Abs. difference in accuracy across groups (Overall accuracy equality)
#' * `fairness.fnr` :: Abs. difference in False negative rates across groups (False negative error rate balance)
#' * `fairness.tpr` :: Abs. difference in True positive rates across groups (Part of Equalized Odds)
#' * `fairness.ppv` :: Abs. difference in Positive predictive values across groups (Part of Conditional use accuracy equality)
#' * `fairness.npv` :: Abs. difference in Negative predictive values across groups (Part of Conditional use accuracy equality)
#' * `fairness.fp`  :: Abs. difference in False positives across groups (Part of Treatment equality)
#' * `fairness.fn`  :: Abs. difference in False negatives across groups (Part of Treatment equality)
#' @examples
#' # Predfined measures:
#' msr("fairness.fpr")
#' msr("fairness.acc")
#' msr("fairness.fnr")
#' msr("fairness.tpr")
#' msr("fairness.ppv")
#' msr("fairness.npv")
#' msr("fairness.fp")
#' msr("fairness.fn")
NULL
