#Data Task generator
simple_test_data <- function() {
  example_data <- data.frame(
    value = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)))
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  task$col_roles$pta = "pta"
  return(task)
}

#Tolerance for numerical comparison
delta = 1e-15
test_data = simple_test_data()
predictions = PredictionClassif$new(row_ids = c(1:16),
                                    truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
                                    response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)))



test_that("MeasureFairness can be created correctly", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fpr"))
  expect_true(inherits(msr_obj, "Measure"))
})

test_that("MeasureFairness can be loaded from msr library correctly", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"), operation = "groupwise_quotient")
  expect_true(inherits(msr_obj, "Measure"))
})

test_that("MeasureFairness with absolute difference operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_abs_diff")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value - 0.15) < delta)
})

test_that("MeasureFairness with difference operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_diff")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value + 0.15) < delta)
})

test_that("MeasureFairness with quotient operations works", {
  msr_obj = MeasureFairness$new(base_measure = msr("classif.fnr"), operation = "groupwise_quotient")
  msr_value = predictions$score(msr_obj, test_data)
  expect_true(abs(msr_value - 0.8) < delta)
})
