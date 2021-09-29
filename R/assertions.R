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

# Assert the pta column to be a binary field.
assert_pta_binary = function(data_task, operation) {
  assert_pta(data_task)
  pta = data_task$data(cols = data_task$col_roles$pta)
  if (nrow(unique(pta)) != 2) {
    stopf("Operation '%s' requires binary pta field, but got '%d' unique factors in pta", operation, nrow(unique(pta)))
  }
}

# Assert data_task contains a pta column.
assert_pta = function(data_task) {
  if (is.null(data_task$col_roles$pta)) {
    stop("Task must have col_roles 'pta' (protected attribute) for fairness operations")
  }
}
