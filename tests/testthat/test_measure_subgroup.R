test_that("measure constructor works", {
  m = MeasureSubgroup$new(base_measure = msr("classif.acc"), subgroup = 1L)
  expect_equal(m$id, "subgroup.acc_1")
  expect_equal(m$base_measure, msr("classif.acc"))
  expect_equal(m$range, c(0, 1))
  expect_equal(m$task_type, "classif")

  m = MeasureSubgroup$new(base_measure = msr("regr.mse"), subgroup = "Foo")
  expect_equal(m$id, "subgroup.mse_Foo")
  expect_equal(m$base_measure, msr("regr.mse"))
  expect_equal(m$range, c(0, Inf))
  expect_equal(m$task_type, "regr")
})

test_that("measure", {
  m = MeasureSubgroup$new(base_measure = msr("classif.acc"), subgroup = "Female")
  t = tsk("compas")
  l = lrn("classif.rpart")
  out = l$train(t)$predict(t)$score(m, t)
  expect_number(out, lower = 0, upper = 1)
  expect_true(names(out) == "subgroup.acc_Female")
})

test_that("measure", {
  t = tsk("compas")
  l = lrn("classif.rpart")
  m = groupwise_metrics(msr("classif.acc"), t, intersect = FALSE)
  expect_set_equal(map_chr(m, "subgroup"), t$levels(t$col_roles$pta)[[1]])
  map(m, expect_class, "Measure")
  out = l$train(t)$predict(t)$score(m, t)
  expect_numeric(out, len = 2L, upper = 1, lower = 0)
  expect_set_equal(names(out), c("subgroup.acc_Female", "subgroup.acc_Male"))

  pta = get_pta(t, rows = t$row_ids)
  rw_1 = t$row_ids[pta == levels(pta[[1]])[1]]
  rw_2 = t$row_ids[pta == levels(pta[[1]])[2]]
  outi = c(
    l$predict(t, row_ids = rw_1)$score(msr("classif.acc")),
    l$predict(t, row_ids = rw_2)$score(msr("classif.acc"))
  )
  expect_true(all(sort(out) == sort(outi)))
})

test_that("multi pta", {
  t = tsk("compas")
  t$col_roles$pta = c("sex", "race")
  l = lrn("classif.rpart")
  m = groupwise_metrics(msr("classif.acc"), t)
  map(m, expect_class, "Measure")
  expect_true(length(map(m, "subgroup")) == 12L)
  prd = l$train(t)$predict(t)
  out = prd$score(m, t)
  expect_numeric(out, len = 12L, upper = 1, lower = 0)

  m = msr("fairness.acc")
  out2 = prd$score(m, t)
  expect_true(out2 == max(out) - min(out))
})


test_that("pp differences", {
  skip_on_cran()
  t = tsk("adult_train")
  l = as_learner(po("reweighing_os") %>>% lrn("classif.rpart"))
  l$train(t)
  prd = l$predict_newdata(t$data())

  out = prd$score(msr("fairness.pp"), t)
  expect_number(out, lower = 0, upper = 1)
  
  out = prd$score(msr("fairness.cv"), t)
  expect_number(out, lower = -1, upper = 1)

})