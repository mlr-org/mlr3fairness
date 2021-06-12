#' @title Compas Dataset
#'
#' @name compas
#' @aliases Compas
#'
#' @description
#' This dataset include the processed COMPAS datas between 2013-2014. The data cleaning process followed the guidance in the original COMPAS repo.
#' Contains 6172 observations and 14 features. The target column could either be "is_recid" or "two_year_recid", but often "two_year_recid" is prefered.
#'
#' @section Pre-processing:
#' * Identication columns are removed
#' * Removed the outliers for abs(days_b_screening_arrest) >= 30.
#' * Removed observations where is_recid != -1.
#' * Removed observations where c_charge_degree != "O".
#' * Removed observations where score_text != 'N/A'.
#' * Factorize the features that are categorical.
#' * Add length of stay (c_jail_out - c_jail_in) in the dataset.
#'
#' @section Metadata:
#' * (integer) Age : The age of defendants
#' * (factor) c_charge_degree : The charge degree of defendants. F: Felony M: Misdemanor
#' * (factor) race: The race of defendants.
#' * (facotr) age: The age category of defendants.
#' * (factor) score_text: The score category of defendants.
#' * (factor) sex: The sex of defendants.
#' * (integer) priors_count: The prior criminal records of defendants.
#' * (integer) days_b_screening_arrest: The count of days between screening date and (original) arrest date. If they are too far apart, that may indicate an error. If the value is negative, that indicate the screening date happened before the arrest date.
#' * (integer) decile_score: Indicate the risk of recidivism (Min=1, Max=10)
#' * (integer) is_recid: Binary variable indicate whether defendant is rearrested at any time.
#' * (factor) two_year_recid: Binary variable indicate whether defendant is rearrested at within two years.
#' * (character) c_jail_in: The date in the jail.
#' * (character) c_jain_out: The date leave the jail.
#' * (numeric) length_of_stay: The count of days stay in jail.
#'
#' @source  @url https://github.com/propublica/compas-analysis
#'
#' @docType data
#' @keywords data
#' @examples
#' data("compas", package = "mlr3fairness")
NULL

get_compas_task = function() {
  b = as_backend("compas")
  task = mlr3::TaskClassif$new("compas", b, target = "two_year_recid")
  b$hash = task$man = "mlr3fairness::mlr_tasks_compas"
  task
}
