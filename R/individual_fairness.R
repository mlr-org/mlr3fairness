#' @title ompute observation-wise individual fairness.
#' 
#' @description 
#' Implements a fairness notion roughly requiring that "similar individuals should be treated similarly".
#' Mathematically, computing individual fairness requires defining two distance metrics \eqn{d_x}{dX} and \eqn{d_y}{dY}
#' defined on feature and prediction spaces respectively. 
#' \deqn{
#'    d_y(f(x1), f(x2)) \leq d_x(x1, x2)
#' }{
#'    dY(f(x1), f(x2) <= dX(f1, x2))
#' }
#'
#' This is computed by comparing differences between predictions and differences between features, be default using the
#' Gower distance. The user is encouraged to adapt the distance metric and features used for computation to form a
#' relevant distance metric that encodes ethical or regulatory considerations.
#' @details 
#' Both dX and dY need to be functions that take as input arguments `x`, `y` (two data.tables) and 
#' an argument `cols` which specifies the colnames to be used for computing distances.
#' 
#' @references
#' `r format_bib("dwork")`
#' `r format_bib("mukherjee")`
#' 
#' @param data [`data.table`] Variables to compare.
#' @param prediction [`Prediction`] Predictions to compare.
#' @param dX [`function] distance metric for features. Defaults to `gower::gower_dist`. See `details` for additional info.
#' @param dY [`function] distance metric for predictions. Defaults to `gower::gower_dist`.  See `details` for additional info.
#' @param fraction [`numeric] For large datasets: Should computation be approximated by computing on only a fraction?
#' 
#' @return 
#'  A [`data.table`] with row_ids and the average individual fairness per observation.
#' 
#' @export
individual_fairness = function(data, prediction, dX = NULL, dY = NULL, frac = 1.0) {
  expect_data_table(data)
  N = nrow(data)
  ij = get_lower_tri_ij(N, frac = frac)
  ydist = prediction_distances(prediction, dY, ij)
  xdist = feature_distances(data, dX, copy(ij))
  # Compute sum of scores for all i,j 
  dt = merge(xdist, ydist, by = c("i", "j"))[, pdiff := abs(dists.y) - dists.x]
  dt = merge(
      dt[, .(isum = sum(pdiff, na.rm = TRUE), icnt = .N), by = "i"],
      dt[, .(jsum = sum(pdiff, na.rm = TRUE), jcnt = .N), by = "j"],
      by.x = "i", by.y = "j", all = TRUE
  )
  dt[is.na(icnt), icnt := 0][is.na(jcnt), jcnt := 0][is.na(isum), isum := 0][is.na(jsum), jsum := 0]
  dt[, individual_fairness := (isum + jsum) / (icnt + jcnt)]
  dt[, row_ids := prediction$row_ids]
  return(dt[, c("row_ids", "individual_fairness")])
}


get_lower_tri_ij = function(i, frac = frac) {
    expect_number(frac, lower = 0, upper = 1)
    ij = map_dtr(seq_len(i), function(x) data.table(i = seq(from = x, to = i), j =x))[i != j]
    ij = ij[sample(seq_len(nrow(ij)), max(nrow(ij), ceiling(frac * nrow(ij)))),]
    return(ij)
}

feature_distances = function(data, dX, ij) {
  lower_tri_dist(data, dX, ij)
}

#' @export
prediction_distances = function (prd,  ...) {
   UseMethod("prediction_distances", prd)
 }

 prediction_distances.PredictionRegr = function (prd, ...) {
   lower_tri_dist(data.table(prd$data$response), ...)
 }

prediction_distances.PredictionClassif = function (prd, ...) {
    if ("prob" %in% prd$predict_types) {
      lower_tri_dist(data.table(prd$data$prob), ...)
    } else {
      lower_tri_dist(data.table(prd$data$response), ...)
    }
}

prediction_distances.PredictionSurv = function (prd, ...) {
    assert("lp" %in%  prd$predict_types)
    lower_tri_dist(as.data.table(exp(prd$data$lp)), ...)
}

lower_tri_dist = function(data, dX, ij) {
  if (is.null(dX)) {
    require_namespaces("gower")
    dX = function(x, y, cols) {
      gower::gower_dist(x, y, pair_x = cols, pair_y = cols)
    }
  }

  cns = which(map_lgl(data, function(x) return(length(unique(x)) > 1L)))
  # Construct lower trianlge
  ij[, dists := dX(data[i,], data[j,], cns)]
  if (length(cns) == 0L) {
    ij[, dists := 0.]
  }

  return(ij)
}



