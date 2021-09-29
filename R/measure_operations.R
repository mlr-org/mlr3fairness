#' Groupwise Ratio
#'
#' Computes `min(x/y, y/x)` i.e. the smallest symmetric ratio between x and y
#' that is smaller than 1. If x is a vector, the symmetric ratio between all
#' elements in 'x' is computed.
#' @param x [´numeric`] \cr
#'   Measured performance in group 1.
#' @param y [´numeric`] \cr
#'   Measured performance in group 2. Defaults to `NULL`.
#' @param fun [character`] \cr
#'   Difference function, see `FUN` in `outer()`. Defaults to  `"\"`.
#' @export
groupdiff_tau = function(x, y = NULL, FUN = "/") {
  if (is.null(y)) assert_numeric(x, min.len = 2L)
  assert_numeric(c(x,y))
  mat = outer(c(x, y), c(x,y), FUN = FUN)
  min(mat[mat <= 1 & (lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)])
}


#' @title Equalized Odds Fairness Measure
#'
#' @description
#'   Compute equalized odds fairness.
MeasureFairnessEOd = R6Class("MeasureFairnessEOd", inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param base_measure (`Measure()`)\cr
    #' The measure used to perform fairness operations.
    #' @param operation (`function`)\cr
    #' The operation used to compute the difference. A function with args 'x' and 'y'(optional) that returns
    #' a single value. Defaults to `abs(x - y)`.
    #' @param minimize (`logical`)\cr
    #' Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric`)\cr
    #' Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(operation = function(x, y) {abs(x - y)}, minimize = TRUE, range = c(-Inf, Inf)) {
      private$.measures = list(
         fpr = MeasureFairness$new(operation = operation, base_measure = msr("classif.fpr")),
         tpr = MeasureFairness$new(operation = operation, base_measure = msr("classif.tpr"))
       )
      super$initialize(
        id = paste0("fairness.EO"),
        range = assert_numeric(range, len = 2),
        properties = "requires_task",
        minimize = assert_flag(minimize),
        predict_type =  msr("classif.fpr")$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness"
      )
    }
  ),
  private = list(
    .measures = NULL,
    .score = function(prediction, task, ...) {
      mean(
        prediction$score(measures = self$measures$fpr, task = task),
        prediction$score(measures = self$measures$tpr, task = task)
      )
    }
  )
)