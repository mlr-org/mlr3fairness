#' @title Fairness Measures in mlr3
#' @name mlr_measures_fairness
#'
#' @section Predefined measures:
#' \CRANpkg{mlr3fairness} comes with a set of predefined fairness measures as listed below.
#' For full flexibility, [MeasureFairness] can be used to construct classical
#' group fairness measures based on a difference between a performance metrics across groups
#' by combining a performance measure with an operation for measuring differences.
#' Furthermore [MeasureSubgroup] can be used to measure performance in a given subgroup, or alternatively
#' groupwise_metrics(measure, task) to instantiate a measure for each subgroup in a [Task].
#'
#' `r tabular(mlr_measures_fairness)`
#'
#' @export
#' @return A data.table containing an overview of available fairness metrics.
#' @examples
#' library("mlr3")
#' # Predefined measures:
#' mlr_measures_fairness$key
mlr_measures_fairness = rowwise_table(
  ~key, ~description,
  "fairness.acc", "Absolute differences in accuracy across groups",
  "fairness.mse", "Absolute differences in mean squared error across groups",

  "fairness.fnr", "Absolute differences in false negative rates across groups",
  "fairness.fpr", "Absolute differences in false positive rates across groups",
  "fairness.tnr", "Absolute differences in true negative rates across groups",
  "fairness.tpr", "Absolute differences in true positive rates across groups",

  "fairness.npv", "Absolute differences in negative predictive values across groups",
  "fairness.ppv", "Absolute differences in positive predictive values across groups ",
  "fairness.fomr", "Absolute differences in false omission rates across groups ",

  "fairness.fp", "Absolute differences in false positives across groups",
  "fairness.tp", "Absolute differences in true positives across groups",
  "fairness.tn", "Absolute differences in true negatives across groups",
  "fairness.fn", "Absolute differences in false negatives across groups",

  "fairness.cv", "Difference in positive class prediction, also known as Calders-Wevers gap or demographic parity",
  "fairness.eod", "Equalized Odds: Mean of absolute differences between true positive and false positive rates across groups",
  "fairness.pp", "Predictive Parity: Mean of absolute differences between ppv and npv across groups",

  "fairness.acc_eod=.05", "Accuracy under equalized odds < 0.05 constraint",
  "fairness.acc_ppv=.05", "Accuracy under ppv difference < 0.05 constraint"
)
