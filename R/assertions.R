#' @title Assertions for mlr3fairness
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
# Single: Has only one pta column.
assert_pta_task = function(task, measure = NULL, single = FALSE) {
  if (length(task$col_roles$pta) == 0L) {
    stopf("Task '%s' must have a column with role 'pta' (protected attribute) for fairness operations", task$id)
  }
  if (single && length(task$col_roles$pta) > 1L) {
    stopf("The operation only allows for a single column specified as 'pta'.", task$id)
  }
  assert_task(task)
}

