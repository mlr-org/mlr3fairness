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
  m = MeasureSubgroup$new(base_measure = msr("classif.acc"), subgroup = 1L)
  t = tsk("compas")
  l = lrn("classif.rpart")
  out = l$train(t)$predict(t)$score(m, t)
  expect_number(out, lower = 0, upper = 1)
  expect_true(names(out) == "subgroup.acc_1")
})

test_that("measure", {
  t = tsk("compas")
  l = lrn("classif.rpart")
  m = groupwise_metrics(msr("classif.acc"), t)
  expect_true(all(map_chr(m, "subgroup") == t$levels(t$col_roles$pta)[[1]]))
  map(m, expect_class, "Measure")
  out = l$train(t)$predict(t)$score(m, t)
  expect_numeric(out, len = 2L, upper = 1, lower = 0)
  expect_true(all(names(out) == c("subgroup.acc_Female", "subgroup.acc_Male")))
})
