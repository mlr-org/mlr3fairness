#' @title Groupwise Operations
#'
#' @description
#' `groupdiff_tau()` computes \eqn{min(x/y, y/x)}, i.e. the smallest symmetric ratio between \eqn{x} and eqn{y}
#' that is smaller than 1. If \eqn{x} is a vector, the symmetric ratio between all
#' elements in \eqn{x} is computed.
#'
#' `groupdiff_absdiff()` computes \eqn{max(abs(x-y, y-x))}, i.e. the smallest absolute difference
#' between \eqn{x} and \eqn{y}.
#' If \eqn{x} is a vector, the symmetric absolute difference between all elements in \eqn{x} is computed.
#'
#' @template pta
#'
#' @param x (`numeric()`)\cr
#'   Measured performance in group 1, 2, ...
#' @export
#' @return A single `numeric`.
#' @examples
#' groupdiff_tau(1:3)
#' groupdiff_diff(1:3)
#' groupdiff_absdiff(1:3)
groupdiff_tau = function(x) {
  assert_numeric(x, min.len = 2L)
  if (anyMissing(x)) {
    return(NA)
  }

  if (all(x == 0)) {
    return(0)
  }

  mat = outer(x, x, FUN = "/")
  mat = mat[mat <= 1 & !diag(x) & !is.nan(mat)]
  min(mat, na.rm = TRUE)
}

#' @export
#' @rdname groupdiff_tau
groupdiff_absdiff = function(x) {
  assert_numeric(x, min.len = 2L)
  if (anyMissing(x)) {
    return(NA)
  }

  if (all(x == 0)) {
    return(0)
  }

  max(dist(x, method = "manhattan"), na.rm = TRUE)
}

#' @export
#' @rdname groupdiff_tau
groupdiff_diff = function(x) {
  assert_numeric(x, min.len = 2L)
  if (anyMissing(x)) {
    return(NA)
  }

  if (all(x == 0)) {
    return(0)
  }

  # All pairwise differences
  xs = outer(x, x, "-")
  xs = xs[upper.tri(xs)]
  # Get the one with the maximum difference.
  xs[which.max(abs(xs))]
}
