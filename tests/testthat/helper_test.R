# check plot satisfy those three conditions
# is_ggplot, no errors, no warnings
check_plots = function(ggplot_obj) {
  expect_true(is.ggplot(ggplot_obj))
  expect_error(ggplot_obj, NA)
  expect_warning(ggplot_obj, NA)
}

library(mlr3)
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "helper", full.names = TRUE), source)
