#' Groupwise Ratio
#'
#' Computes `min(x/y, y/x)` i.e. the smallest symmetric ratio between x and y
#' that is smaller than 1. If x is a vector, the symmetric ratio between all
#' elements in 'x' is computed.
#' @param x `numeric` \cr
#'   Measured performance in group 1, 2, ...
#' @examples
#'   groupdiff_tau(runif(3))
#' @export
groupdiff_tau = function(x) {
  assert_numeric(x, min.len = 2L)
  if (any(is.na(x))) return(NA)
  if (all(x == 0)) return(0)
  mat = outer(x, x, FUN = "/")
  mat = mat[mat <= 1 & (lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)]
  min(mat, na.rm = TRUE)
}

#' Groupwise Absolute Differences
#'
#' Computes `max(abs(x-y, y-x))` i.e. the smallest symmetric ratio between x and y
#' that is smaller than 1. If x is a vector, the symmetric ratio between all
#' elements in 'x' is computed.
#' @param x ´numeric` \cr
#'   Measured performance in group 1, 2, ...
#' @examples
#'   groupdiff_absdiff(runif(3))
#' @export
groupdiff_absdiff = function(x) {
  assert_numeric(x, min.len = 2L)
  if (any(is.na(x))) return(NA)
  if (all(x == 0)) return(0)
  mat = outer(x, x, FUN = "-")
  mat = mat[(lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)]
  max(abs(mat), na.rm = TRUE)
}
