test_that("measure", {
  skip_on_cran()
  skip_if_not_installed("mlr3proba")
  m = msr("fairness.surv_group")
  t = tsk("rats")
  t$col_roles$pta = "sex"
  l = lrn("surv.coxph")
  l$train(t)
  out = l$predict(t)$score(m, t)
  expect_number(out, lower = -Inf, upper = Inf)
})