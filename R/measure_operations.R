#' Groupwise Ratio
#'
#' @description
#' Computes \eqn{min(x/y, y/x)}, i.e. the smallest symmetric ratio between \eqn{x} and eqn{y}
#' that is smaller than 1. If \eqn{x} is a vector, the symmetric ratio between all
#' elements in \eqn{x} is computed.
#'
#' @param x (`numeric()`)\cr
#'   Measured performance in group 1, 2, ...
#' @export
#' @examples
#' groupdiff_tau(1:3)
groupdiff_tau = function(x) {
  assert_numeric(x, min.len = 2L)
  if (anyMissing(x)) {
    return(NA)
  }

  if (all(x == 0)) {
    return(0)
  }

  mat = outer(x, x, FUN = "/")
  mat = mat[mat <= 1 & (lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)]
  min(mat, na.rm = TRUE)
}

#' Groupwise Absolute Differences
#'
#' Computes \eqn{max(abs(x-y, y-x))}, i.e. the smallest symmetric ratio between \eqn{x} and \eqn{y}
#' that is smaller than 1. If \eqn{x} is a vector, the symmetric ratio between all
#' elements in \eqn{x} is computed.
#' @param x (´numeric()`)\cr
#'   Measured performance in group 1, 2, ...
#' @export
#' @examples
#' groupdiff_absdiff(1:3)
groupdiff_absdiff = function(x) {
  assert_numeric(x, min.len = 2L)
  if (anyMissing(x)) {
    return(NA)
  }

  if (all(x == 0)) {
    return(0)
  }

  mat = outer(x, x, FUN = "-")
  mat = mat[(lower.tri(mat) | upper.tri(mat)) & !is.nan(mat)]

  # FIXME: WTF?
  max(abs(mat), na.rm = TRUE)
}
