test_that("reweighing PipeOp can be loaded and works with disparate impact score fairness measures", {
  task = tsk("adult_train")$filter(1:300)
  reweighing = po("reweighing_wts")
  graph = reweighing %>>% lrn("classif.rpart")
  glrn = GraphLearner$new(graph)
  glrn$train(task)
  tem = glrn$predict(task)
  expect_true(!is.null(glrn$state))
  expect_r6(tem, "PredictionClassif")
})

test_that("reweighing_wts", {
  tsk = po("reweighing_wts")$train(list(tsk("adult_train")$filter(1:300)))[[1]]
  expect_true(tsk$col_roles$weight == "reweighing.WEIGHTS")
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")), tsk$weights)
  dt = dt[, mean(weight) * .N, by = .(sex, target)][, sum(V1 /sum(V1)), by = "target"]
  expect_true(all(abs(dt$V1 - 1) < 1e-3))
})

test_that("reweighing_wts", {
  tsk = po("reweighing_os")$train(list(tsk("adult_train")$filter(1:600)))[[1]]
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")))
  tab = table(dt$sex, dt$target)
  expect_true(abs(diff(tab[1,] / tab[2,])) < 20/nrow(dt))
})