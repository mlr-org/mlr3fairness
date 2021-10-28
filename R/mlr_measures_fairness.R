#' @title Fairness Measures in mlr3
#' @name mlr_measures_fairness
#'
#' @section Predefined measures:
#' \CRANpkg{mlr3fairness} comes with a set of predefined fairness measures as listed below.
#' For full flexibility, [MeasureFairness] can be used to construct classical
#' group fairness measures based on a difference between a performance metrics across groups
#' by combining a performance measure with an operation for measuring differences.
#'
#' `r tabular(mlr_measures_fairness)`
#'
#' @export
#' @examples
#' # Predefined measures:
#' mlr_measures_fairness$key
mlr_measures_fairness = rowwise_table(
  ~key, ~description,
  "fairness.acc", "Absolute differences in accuracy across groups (Overall accuracy equality)",
  "fairness.eod", "Equalized Odds: Sum of absolute differences between true positive and false positive rates across groups",
  "fairness.fn", "Absolute differences in false negatives across groups",
  "fairness.fnr", "Absolute differences in false negative rates across groups",
  "fairness.fp", "Absolute differences in false positives across groups",
  "fairness.fpr", "Absolute differences in false positive rates across groups",
  "fairness.npv", "Absolute differences in negative predictive values across groups",
  "fairness.ppv", "Absolute differences in positive predictive values across groups ",
  "fairness.tn", "Absolute differences in true negatives across groups",
  "fairness.tnr", "Absolute differences in true negative rates across groups",
  "fairness.tp", "Absolute differences in true positives across groups",
  "fairness.tpr", "Absolute differences in true positive rates across groups"
)