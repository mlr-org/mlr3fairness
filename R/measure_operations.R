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
  min(mat[mat <= 1 & (lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)], ra.rm = TRUE)
}