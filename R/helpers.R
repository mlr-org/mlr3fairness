as_backend = function(id) {
  # Integrated from mlr3data subpackage
  # this looks a bit funny, but is required because of strange
  # behavior with roxygen2/pkgload
  ee = new.env(hash = FALSE, parent = emptyenv())
  data(list = id, package = "mlr3fairness", envir = ee)
  mlr3::as_data_backend(ee[[id]])
}

score_groupwise = function(prediction, base_measure, task) {
  # Get protected attribute vector
  groups = task$data(cols = task$col_roles$pta, rows = prediction$row_ids)[[1]]

  # Split prediction
  map_dbl(split(prediction$row_ids, groups), function(rws) {
    prediction$clone()$filter(rws)$score(base_measure, task = task)
  })
}

compute_reweighing_weights = function (task, alpha = 1) {
  dt = task$data(cols = c(task$target_names, task$col_roles$pta))
  tab = as.data.table(table(dt))
  tab[, n_tgt := sum(N), by = eval(task$target_names)]
  tab[, n_pta := sum(N), by = eval(task$col_roles$pta)]
  tab[, wt := (n_tgt * n_pta) / (sum(N) * N)]
  tab[, wt := (1-alpha) * wt + alpha * wt]
  return(tab)
}
