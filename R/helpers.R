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
