#' Groupwise Ratio
#'
#' Computes `min(x/y, y/x)` i.e. the symmetric ratio between x and y
#' that is smaller than 1.
#' @param x [´numeric`] \cr
#'   Measured performance in group 1.
#' @param y [´numeric`] \cr
#'   Measured performance in group 1.
#' @param eps [´numeric`] \cr
#'   A small value to ensure no division by 0.
groupdiff_tau = function(x, y, eps = .01) {
  min(c(x/max(c(y,eps)), y/max(c(x, eps))))
}

