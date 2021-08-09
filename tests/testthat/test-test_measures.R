delta = 1e-15
test_data = simple_test_data()
predictions = simple_pred_data()

test_that("fairness.fpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fpr"), operation = "groupwise_abs_diff")
  expect_true( round(predictions$score(msr_obj, test_data),4) == 0.0833 )
})

test_that("fairness.acc can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.acc"), operation = "groupwise_abs_diff")
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.125) < delta )
})

test_that("fairness.fnr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fnr"), operation = "groupwise_abs_diff")
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.15) < delta )
})

test_that("fairness.tpr can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.tpr"), operation = "groupwise_abs_diff")
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.15) < delta )
})

test_that("fairness.ppv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.ppv"), operation = "groupwise_abs_diff")
  expect_true( abs(predictions$score(msr_obj, test_data) - 0.25) < delta )
})

test_that("fairness.npv can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.npv"), operation = "groupwise_abs_diff")
  expect_true( abs(predictions$score(msr_obj, test_data) - 0) < delta )
})

test_that("fairness.fp can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fp"), operation = "groupwise_abs_diff")
  expect_true( predictions$score(msr_obj, test_data) == 1 )
})

test_that("fairness.fn can be loaded and work as expected", {
  msr_obj = msr("fairness", base_measure = msr("classif.fn"), operation = "groupwise_abs_diff")
  expect_true( predictions$score(msr_obj, test_data) == 0)
})
