# stopifnotdataset = function(data) {
#   stopifnot(
#     is.data.frame(data),
#     colnames(data) == tolower(colnames(data)),
#     colnames(data) == make.names(colnames(data), unique = TRUE),
#     is.integer(attr(data, "row.names"))
#   )
# }

# data("compas", package = "mlr3fairness")
# stopifnotdataset(compas)
# stopifnot(nrow(compas) == 6172L, ncol(compas) == 12L)

# data("adult_test", package = "mlr3fairness")
# stopifnotdataset(adult_test)
# stopifnot(nrow(adult_test) == 15315L, ncol(adult_test) == 13L)

# data("adult_train", package = "mlr3fairness")
# stopifnotdataset(adult_train)
# stopifnot(nrow(adult_train) == 30718L, ncol(adult_train) == 13L)


# if (requireNamespace("mlr3")) {
#   stopifnot(inherits(mlr3::tsk("compas"), "TaskClassif"))
#   stopifnot(inherits(mlr3::tsk("adult_test"), "TaskClassif"))
#   stopifnot(inherits(mlr3::tsk("adult_test"), "TaskClassif"))
# }



test_that("tasks can be loaded", {
  for (name in c("compas", "adult_train", "adult_test")) {
    tsk = tsk(name)
    expect_true(inherits(tsk, "TaskClassif"))
  }
})
