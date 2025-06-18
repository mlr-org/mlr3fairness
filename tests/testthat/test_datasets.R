check_data_format = function(data) {
  expect_true(is.data.frame(data))
  expect_true(all(colnames(data) == tolower(colnames(data))))
  expect_true(all(colnames(data) == make.names(colnames(data), unique = TRUE)))
  expect_true(is.integer(attr(data, "row.names")))
}

test_that("tasks can be loaded", {
  for (name in c("compas", "adult_train", "adult_test")) {
    tsk_obj = tsk(name)
    expect_true(inherits(tsk_obj, "TaskClassif"))
  }
})

test_that("compas dataset can be loaded with correct format", {
  skip_on_cran()
  compas = tsk("compas")
  expect_r6(compas, "TaskClassif")
  compas_data = compas$data()
  check_data_format(compas_data)
  expect_true(nrow(compas_data) == 6172L)
  expect_true(ncol(compas_data) == 11L)
  expect_true(compas$col_roles$pta == "sex")
  assert_col_type = (sapply(compas_data, class) == c(
    "factor", "integer", "factor", "factor",  "integer", "integer",
    "integer", "integer", "factor",  "factor",  "factor")
  )
  expect_true(all(assert_col_type))
})

test_that("adult_train dataset can be loaded with correct format", {
  skip_on_cran()
  adult_train = tsk("adult_train")
  expect_r6(adult_train, "TaskClassif")
  adult_train_data = adult_train$data()
  expect_true(nrow(adult_train_data) == 30718L)
  expect_true(ncol(adult_train_data) == 13L)
  expect_true(adult_train$col_roles$pta == "sex")

  assert_col_type = (sapply(adult_train_data, class) == c("factor", "integer", "integer", "integer",
    "factor", "integer", "integer", "factor",
    "factor", "factor", "factor", "factor", "factor"))
  expect_true(all(assert_col_type))
})

test_that("adult_test dataset can be loaded with correct format", {
  skip_on_cran()
  adult_test = tsk("adult_test")
  expect_r6(adult_test, "TaskClassif")
  adult_test_data = adult_test$data()
  expect_true(nrow(adult_test_data) == 15315L)
  expect_true(ncol(adult_test_data) == 13L)
  expect_true(adult_test$col_roles$pta == "sex")

  assert_col_type = (sapply(adult_test_data, class) == c("factor", "integer", "integer", "integer",
    "factor", "integer", "integer", "factor",
    "factor", "factor", "factor", "factor", "factor"))
  expect_true(all(assert_col_type))
})
