delta = 1e-15
test_data = simple_test_data()
predictions = simple_pred_data()

test_that("fairness.fpr can be loaded and work as expected", {
  metrics = c("fairness.acc", "fairness.EOd", "fairness.fn", "fairness.fnr", "fairness.fp",
    "fairness.fpr", "fairness.npv", "fairness.ppv", "fairness.tpr")
  map(metrics, function (m) {
    out = predictions$score(measures = msr(m), task = test_data)
    expect_number(out)
  })
})

