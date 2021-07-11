check_data_format = function(data) {
  expect_true(is.data.frame(data))
  expect_true(sum(colnames(data) == tolower(colnames(data))) == length(colnames(data))) #Check all True
  expect_true(sum(colnames(data) == make.names(colnames(data), unique = TRUE)) == length(colnames(data)))
  expect_true(is.integer(attr(data, "row.names")))
}

test_that("tasks can be loaded", {
  for (name in c("compas", "adult_train", "adult_test")) {
    tsk_obj = tsk(name)
    expect_true(inherits(tsk_obj, "TaskClassif"))
  }
})

test_that("compas dataset can be loaded with correct format", {
  compas = tsk("compas")
  check_data_format(compas$data())
  expect_true(nrow(compas$data()) == 6172L)
  expect_true(ncol(compas$data()) == 12L)
  expect_true(compas$col_roles$pta == "sex")
})

test_that("adult_train dataset can be loaded with correct format", {
  adult_train = tsk("adult_train")
  expect_true(nrow(adult_train$data()) == 30718L)
  expect_true(ncol(adult_train$data()) == 13L)
  expect_true(adult_train$col_roles$pta == "sex")
})

test_that("adult_test dataset can be loaded with correct format", {
  adult_test = tsk("adult_test")
  expect_true(nrow(adult_test$data()) == 15315L)
  expect_true(ncol(adult_test$data()) == 13L)
  expect_true(adult_test$col_roles$pta == "sex")
})
