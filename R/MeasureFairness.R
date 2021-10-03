#' @title Fairness Measure
#' @name mlr_measures_fairness
#'
#' @description
#'   This measure specializes [mlr3::Measure()] to allow for measuring statistical group fairness:
#'   A common approach to quantifying a model's fairness is to compute the difference between a protected and an
#'   unprotected group according to some performance metric such as `classification error`,
#'   `false positive rate` or others.
#'   This measure allows for the composition of a fairness-metric from such a `base_measure` by computing
#'   group-wise difference. This difference metric can be specified using the  'operation' parameter.
#'
#' @section Overview:
#'   For an overview of measures see [`mlr_measures_fairness`].
#'   Composite measures encompasing multiple fairness metrics can be built using [`MeasureFairnessComposite`].
#'
#'   Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#'   See [`groupdiff_tau`] for a simple way to construct difference functions.
#'
#' @seealso groupdiff_tau
#' @export
#' @examples
#'   # Create MeasureFairness to measure the Predictive Parity.
#'   library(mlr3)
#'   data_task = tsk("adult_train")
#'   learner = lrn("classif.rpart", cp = .01)
#'   learner$train(data_task)
#'   measure = msr("fairness", base_measure = msr("classif.ppv"))
#'   predictions = learner$predict(data_task)
#'   predictions$score(measure, task = data_task)
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
    #' @param operation (`function`)\cr
    #' The operation used to compute the difference. A function with args 'x' and 'y'(optional) that returns
    #' a single value. Defaults to `max(abs(diff(x)))`.
    #' @param minimize (`logical`)\cr
    #' Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric`)\cr
    #' Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(base_measure, operation = function(x) {max(abs(diff(x)))}, minimize = TRUE, range = c(-Inf, Inf)) {
      self$operation = assert_function(operation)
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
      assert_pta_task(task)
      mvals = score_groupwise(prediction, self$base_measure, task)
      invoke(self$operation, mvals)
    }
  )
)

mlr_measures$add("fairness", MeasureFairness)


#' @title Composite Fairness Measure
#'
#' @description
#'   Compute a composite measure from multiple fairness metrics.
#'   Aggregates using `aggfun`, defaults to `mean()`.
#' @export
#' @examples
#'   # Create MeasureFairness to measure the Predictive Parity.
#'   library(mlr3)
#'   library(mlr3fairness)
#'   # Equalized Odds Metric
#'   MeasureFairnessComposite$new(measures = list("fairness.classif.fpr", "fairness.classif.tpr"))
#'   # Other metrics e.g. based on negative rates
#'   MeasureFairnessComposite$new(measures = list("fairness.classif.fnr", "fairness.classif.tnr"))
MeasureFairnessComposite = R6Class("MeasureFairnessComposite", inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id [`character`]\cr
    #'   Id of the measure. Defaults to the concatenation of id's in `measure`.
    #' @param measures list of [`MeasureFairness`| `character`]\cr
    #'   List of fairness measures to aggregate (or valid keys to be used with `msr()`).
    #' @param aggfun (`function()`)\cr
    #'   Aggregation function used to aggregate results from respective measures. Defaults to `sum`.
    #' @param operation (`function`)\cr
    #'   The operation used to compute the difference. A function with args 'x' and 'y'(optional) that returns
    #'   a single value. See `MeasureFairness` for more information. Only used if `measures` is of type `character`.
    #' @param minimize (`logical`)\cr
    #'   Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric`)\cr
    #'   Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(id = NULL, measures, aggfun = function(x) mean(x, na.rm = TRUE), operation = function(x) {max(abs(diff(x)))}, minimize = TRUE, range = c(-Inf, Inf)) {

      if (all(map(measures, class) == "character")) {
        measures = msrs(unlist(measures), operation = operation)
      }
      private$.measures = assert_measures(measures)
      private$.aggfun = assert_function(aggfun)

      if (is.null(id)) {
        id = paste0(gsub("classif.", "", gsub("fairness.", "", ids(measures))), collapse = "_")
      }

      super$initialize(
        id = sprintf("fairness.%s", assert_character(id)),
        range = assert_numeric(range, len = 2),
        properties = "requires_task",
        minimize = assert_flag(minimize),
        predict_type =  unique(unlist(map(measures, "predict_type"))),
        packages = "mlr3fairness",
        man = "mlr_measures_fairness_composite"
      )
    }
  ),
  private = list(
    .measures = NULL,
    .aggfun = NULL,
    .score = function(prediction, task, ...) {
      private$.aggfun(
        map_dbl(private$.measures, function(m) {
          prediction$score(m, task = task, ...)
        })
      )
    }
  )
)

mlr_measures$add("fairness.composite", MeasureFairnessComposite)


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
#'  fairness.EOd \tab Equalized Odds: Abs. difference between true positive and false positive rates across groups           \cr
#'  fairness.fpr \tab Abs. difference in false positive rates across groups                                                  \cr
#'  fairness.acc \tab Abs. difference in accuracy across groups (Overall accuracy equality)                               \cr
#'  fairness.tpr \tab Abs. difference in True positive rates across groups                                                   \cr
#'  fairness.ppv \tab Abs. difference in Positive predictive values across groups (Part of Conditional use accuracy equality)\cr
#'  fairness.npv \tab Abs. difference in Negative predictive values across groups (Part of Conditional use accuracy equality)\cr
#'  fairness.fp  \tab Abs. difference in False positives across groups (Part of Treatment equality)                          \cr
#'  fairness.fn  \tab Abs. difference in False negatives across groups (Part of Treatment equality)
#' }
#'
#' @examples
#' # Predfined measures:
#' msr("fairness.EOd")
#' msr("fairness.fpr")
#' msr("fairness.acc")
#' msr("fairness.fnr")
#' msr("fairness.tpr")
#' msr("fairness.ppv")
#' msr("fairness.npv")
#' msr("fairness.fp")
#' msr("fairness.fn")
mlr_measures_fairness = rowwise_table(
  ~key, ~ description,
  "fairness.EOd" , "Equalized Odds: Sum of abs. difference between true positive and false positive rates across groups",
  "fairness.fpr" , "Abs. difference in false positive rates across groups",
  "fairness.acc" , "Abs. difference in accurq()acy across groups (Overall accuracy equality)",
  "fairness.tpr" , "Abs. difference in True positive rates across groups",
  "fairness.ppv" , "Abs. difference in Positive predictive values across groups (Part of Conditional use accuracy equality)",
  "fairness.npv" , "Abs. difference in Negative predictive values across groups (Part of Conditional use accuracy equality)",
  "fairness.fp"  , "Abs. difference in False positives across groups (Part of Treatment equality)",
  "fairness.fn"  , "Abs. difference in False negatives across groups (Part of Treatment equality)"
 )
