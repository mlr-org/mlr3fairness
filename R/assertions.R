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
assert_pta_binary = function(t, operation) {
  assert_pta(task)
  pta = task$data(cols = task$col_roles$pta)
  if (nrow(unique(pta)) != 2) {
    stopf("Operation '%s' requires binary pta field, but got '%d' unique factors in pta", operation, nrow(unique(pta)))
  }
}

# Assert task contains a pta column.
assert_pta = function(task) {
  if (is.null(task$col_roles$pta)) {
    stop("Task must have col_roles 'pta' (protected attribute) for fairness operations")
  }
}
