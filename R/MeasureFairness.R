#' @title Fairness Measure
#' @name mlr_measures_fairness
#'
#' @description
#'   This measure specializes [mlr3::Measure()] to allow for measuring statistical group fairness:
#'   A common approach to quantifying a model's fairness is to compute the difference between a
#'   protected and an
#'   unprotected group according to some performance metric such as `classification error`,
#'   `false positive rate` or others.
#'   This measure allows for the composition of a fairness-metric from such a `base_measure` by computing
#'   group-wise difference. This difference metric can be specified using the  'operation' parameter.
#'
#' @section Overview:
#'   For an overview of measures see [`mlr_measures_fairness`].
#'   Composite measures encompasing multiple fairness metrics can be built using
#'   [`MeasureFairnessComposite`].
#'
#'   Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#'
#' @seealso groupdiff_tau groupdiff_absdiff
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
MeasureFairness = R6::R6Class("MeasureFairness", inherit = Measure,
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
    #' @param operation (`function`)\cr
    #' The operation used to compute the difference. A function that returns
    #' a single value given input: computed metric for each subgroup.
    #' Defaults to `groupdiff_absdiff`.
    #' @param minimize (`logical`)\cr
    #' Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric`)\cr
    #' Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(base_measure, operation = groupdiff_absdiff, minimize = TRUE,
      range = c(-Inf, Inf)) {
      self$operation = assert_function(operation)
      self$base_measure = assert_measure(base_measure)

      # regr|classif|... to fairness
      id = paste0("fairness", gsub(paste0(mlr_reflections$task_types$type, collapse = "|"), "",
        base_measure$id))

      super$initialize(
        id = id,
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
      assert_pta_task(task)
      mvals = score_groupwise(prediction, self$base_measure, task)
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
#' \tabular{ll}{
#'  fairness.eod \tab Equalized Odds: Abs. difference between true positive and false positive rates across groups \cr
#'  fairness.fpr \tab Abs. difference in false positive rates across groups \cr
#'  fairness.acc \tab Abs. difference in accuracy across groups (Overall accuracy equality) \cr
#'  fairness.tpr \tab Abs. difference in True positive rates across groups \cr
#'  fairness.ppv \tab Abs. difference in Positive predictive values across groups \cr
#'  fairness.npv \tab Abs. difference in Negative predictive values across groups \cr
#'  fairness.fp  \tab Abs. difference in False positives across groups  \cr
#'  fairness.fn  \tab Abs. difference in False negatives across groups
#' }
#'
#' @examples
#' # Predefined measures:
#' msr("fairness.eod")
#' msr("fairness.fpr")
#' msr("fairness.acc")
#' msr("fairness.fnr")
#' msr("fairness.tpr")
#' msr("fairness.ppv")
#' msr("fairness.npv")
#' msr("fairness.fp")
#' msr("fairness.fn")
mlr_measures_fairness = rowwise_table(
  ~key, ~description,
  "fairness.eod", "Equalized Odds: Sum of abs. difference between true positive and
    false positive rates across groups",
  "fairness.fpr", "Abs. difference in false positive rates across groups",
  "fairness.acc", "Abs. difference in accuracy across groups (Overall accuracy equality)",
  "fairness.tpr", "Abs. difference in true positive rates across groups",
  "fairness.tnr", "Abs. difference in true negative rates across groups",
  "fairness.ppv", "Abs. difference in positive predictive values across groups ",
  "fairness.npv", "Abs. difference in negative predictive values across groups",
  "fairness.fp", "Abs. difference in false positives across groups",
  "fairness.fn", "Abs. difference in false negatives across groups"
)
