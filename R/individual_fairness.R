#' Compute observation wise individual fairness.
#' 
#' The corresponding notion is "similar individuals should be treated similarly".
#' This is computed by comparing differences between predictions  and differences between features, using the
#' Gower distance.
#' 
#' @param data [`data.table`] Variables to compare.
#' @param prediction [`Prediction`] Predictions to compare.
#' @param fraction [`numeric] For large datasets: Should computation be approximated by computing on only a fraction?
#' 
#' @return 
#'  A [`data.table`] with row_ids and the average individual fairness per observation.
#' 
#' @export
individual_fairness = function(data, prediction, frac = 1.0) {
  expect_data_table(data)
  N = nrow(data)
  ij = get_lower_tri_ij(N, frac = frac)
  ydist = prediction_distances(prediction, ij)
  xdist = lower_tri_gower(data, copy(ij))
  dt = merge(xdist, ydist, by = c("i", "j"))[, pdiff := abs(dists.y) - dists.x]
  dt = merge(
      dt[, .(isum = sum(pdiff, na.rm = TRUE), icnt = .N), by = "i"],
      dt[, .(jsum = sum(pdiff, na.rm = TRUE), jcnt = .N), by = "j"],
      by.x = "i", by.y = "j", all = TRUE
  )
  dt[is.na(icnt), icnt := 0][is.na(jcnt), jcnt := 0][is.na(isum), isum := 0][is.na(jsum), jsum := 0]
  dt[, mean := (isum + jsum) / (icnt + jcnt)]
  dt[, row_ids := prediction$row_ids]
  return(dt[, c("row_ids", "mean")])
}


get_lower_tri_ij = function(i, frac = frac) {
    expect_number(frac, lower = 0, upper = 1)
    ij = map_dtr(seq_len(i), function(x) data.table(i = seq(from = x, to = i), j =x))[i != j]
    ij = ij[sample(seq_len(nrow(ij)), max(nrow(ij), ceiling(frac * nrow(ij)))),]
    return(ij)
}

#' @export
prediction_distances = function (prd, ...) {
   UseMethod("prediction_distances", prd)
 }

 prediction_distances.PredictionRegr = function (prd, ...) {
   lower_tri_gower(data.table(prd$data$response), ...)
 }

prediction_distances.PredictionClassif = function (prd, ...) {
    if ("prob" %in% prd$predict_types) {
      lower_tri_gower(data.table(prd$data$prob), ...)
    } else {
      lower_tri_gower(data.table(prd$data$response), ...)
    }
}


prediction_distances.PredictionSurv = function (prd, ...) {
    if ("distr" %in% prd$predict_types) {
      lower_tri_gower(as.data.table(prd$data$distr), ...)
    } else if ("prob" %in% prd$predict_types) {
      lower_tri_gower(data.table(prd$data$prob), ...)
    } else {
      lower_tri_gower(data.table(prd$data$crank), ...)
    }
}

lower_tri_gower = function(data, ij) {
    require_namespaces("gower")
    cns = which(map_lgl(data, function(x) return(length(unique(x)) > 1L)))
    # Construct lower trianlge
    ij[, dists := gower::gower_dist(data[i,], data[j,], pair_x = cns, pair_y = cns)]
    if (length(cns) == 0L) {
      ij[, dists := 0.]
    }

    return(ij)
}



