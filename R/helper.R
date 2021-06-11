as_backend = function(id) {
  # Integrated from mlr3data subpackage
  # this looks a bit funny, but is required because of strange
  # behavior with roxygen2/pkgload
  ee = new.env(hash = FALSE, parent = emptyenv())
  data(list = id, package = "mlr3data", envir = ee)
  mlr3::as_data_backend(ee[[id]])
}
