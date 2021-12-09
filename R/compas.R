#' @title COMPAS Dataset
#'
#' @name compas
#' @aliases Compas
#'
#' @description
#' The COMPAS dataset includes the processed COMPAS data between 2013-2014.
#' The data cleaning process followed the guidance in the original COMPAS repo.
#' Contains 6172 observations and 14 features.
#' The target column could either be "is_recid" or "two_year_recid", but often "two_year_recid" is prefered.
#' The column `"sex"` is set as protected attribute, but more often `"race"` is used.
#'
#' @section Pre-processing:
#' * Identifying columns are removed
#' * Removed the outliers for abs(days_b_screening_arrest) >= 30.
#' * Removed observations where is_recid != -1.
#' * Removed observations where c_charge_degree != "O".
#' * Removed observations where score_text != 'N/A'.
#' * Factorize the features that are categorical.
#' * Add length of stay (c_jail_out - c_jail_in) in the dataset.
#' * `Pre-processing Resouce:` @url https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb
#'
#' @section Metadata:
#' * (integer) age : The age of defendants.
#' * (factor) c_charge_degree : The charge degree of defendants. F: Felony M: Misdemeanor
#' * (factor) race: The race of defendants.
#' * (factor) age_cat: The age category of defendants.
#' * (factor) score_text: The score category of defendants.
#' * (factor) sex: The sex of defendants.
#' * (integer) priors_count: The prior criminal records of defendants.
#' * (integer) days_b_screening_arrest: The count of days between screening date and (original) arrest date.
#'   If they are too far apart, that may indicate an error. If the value is negative,
#'   that indicate the screening date happened before the arrest date.
#' * (integer) decile_score: Indicate the risk of recidivism (Min=1, Max=10)
#' * (integer) is_recid: Binary variable indicate whether defendant is rearrested at any time.
#' * (factor) two_year_recid: Binary variable indicate whether defendant is rearrested at within two years.
#' * (numeric) length_of_stay: The count of days stay in jail.
#'
#' @source \url{https://github.com/propublica/compas-analysis}
#'
#' @docType data
#' @keywords data
#' @examples
#' data("compas", package = "mlr3fairness")
NULL


#' @title COMPAS Classification Task
#'
#' @name compas
#' @format [R6::R6Class] inheriting from [TaskClassif].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("compas")
#' tsk("compas")
#' ```
#'
#' @description
#' A classification task for the [compas] data set.
NULL

get_compas_task = function() { # nocov start
  b = as_data_backend(mlr3fairness::compas)
  task = mlr3::TaskClassif$new("compas", b, target = "two_year_recid")
  task$col_roles$pta = "sex"
  b$hash = task$man = "mlr3fairness::mlr_tasks_compas"
  task
} # nocov end


#' @title COMPAS Classification Task
#'
#' @name compas
#' @format [R6::R6Class] inheriting from [TaskClassif].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("compas_race_binary")
#' tsk("compas_race_binary")
#' ```
#'
#' @description
#' A classification task for the [compas] data set.
#' The observations have been filtered, keeping only observations with race
#' `"Caucasian"` and `"African-American"`. The protected attribute has been set
#' to `"race"`.
NULL

get_compas_task_race_binary = function() { # nocov start
  keep = c("age", "age_cat", "c_charge_degree", "days_b_screening_arrest",
    "decile_score", "length_of_stay", "priors_count", "score_text",
    "sex", "race", "two_year_recid")
  data = mlr3fairness::compas[get("race") %in% c("Caucasian", "African-American"), keep, with = FALSE]
  b = as_data_backend(droplevels(data))
  task = mlr3::TaskClassif$new("compas_race_binary", b, target = "two_year_recid")
  task$col_roles$pta = "race"
  b$hash = task$man = "mlr3fairness::mlr_tasks_compas_race_binary"
  task
} # nocov end
