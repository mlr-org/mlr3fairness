
#' @title Composite Fairness Measure
#'
#' @description
#' Compute a composite measure from multiple fairness metrics.
#' Aggregates using `aggfun`, defaults to `mean()`.
#'
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' library(mlr3)
#' library(mlr3fairness)
#' # Equalized Odds Metric
#' MeasureFairnessComposite$new(measures = list("fairness.fpr", "fairness.tpr"))
#' # Other metrics e.g. based on negative rates
#' MeasureFairnessComposite$new(measures = list("fairness.fnr", "fairness.tnr"))
MeasureFairnessComposite = R6::R6Class("MeasureFairnessComposite", inherit = Measure,
  public = list(
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   #'
   #' @param id [`character`]\cr
   #'   Id of the measure. Defaults to the concatenation of id's in `measure`.
   #' @param measures list of [`MeasureFairness`] | [`character`]\cr
   #'   List of fairness measures to aggregate (or valid keys to be used with `msr()`).
   #' @param aggfun (`function`)\cr
   #'   Aggregation function used to aggregate results from respective measures. Defaults to `sum`.
   #' @param operation (`function`)\cr
   #' The operation used to compute the difference. A function that returns
   #' a single value given input: computed metric for each subgroup.
   #' Defaults to `groupdiff_absdiff`.
   #' See `MeasureFairness` for more information. Only used if `measures` is of type `character`.
   #' @param minimize (`logical`)\cr
   #'   Should the measure be minimized? Defaults to `TRUE`.
   #' @param range (`numeric`)\cr
   #'   Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
   initialize = function(id = NULL, measures, aggfun = function(x) sum(x, na.rm = TRUE),
                         operation = groupdiff_absdiff, minimize = TRUE, range = c(-Inf, Inf)) {

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
       predict_type = unique(unlist(map(measures, "predict_type"))),
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
