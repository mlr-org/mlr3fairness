
#' @title Assertion for mlr3fairness
#'
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3fairness}.
#' Most assertion functions ensure the right class attribute, and optionally additional properties.
#'
#' If an assertion fails, an exception is raised.
#' Otherwise, the input object is returned invisibly.
#'
#' @noRd
NULL

# Assert task contains a pta column.
assert_pta_task = function(task, measure = NULL) {
  if (length(task$col_roles$pta) == 0L) {
    stop("Task must have col_roles 'pta' (protected attribute) for fairness operations")
  }
}
