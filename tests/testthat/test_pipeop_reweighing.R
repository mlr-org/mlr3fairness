test_that("reweighing PipeOp can be loaded and works with disparate impact score fairness measures", {
  skip_on_cran()
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
  skip_on_cran()
  tsk = po("reweighing_wts")$train(list(tsk("adult_train")$filter(1:300)))[[1]]
  expect_true(tsk$col_roles$weight == "reweighing.WEIGHTS")
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")), tsk$weights)
  dt = dt[, mean(weight) * .N, by = .(sex, target)][, sum(V1 / sum(V1)), by = "target"]
  expect_true(all(abs(dt$V1 - 1) < 1e-3))
})

test_that("reweighing_wts", {
  skip_on_cran()
  tsk = po("reweighing_os")$train(list(tsk("adult_train")$filter(1:1000)))[[1]]
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")))
  tab = table(dt$sex, dt$target)
  expect_true(abs(diff(tab[1, ] / tab[2, ])) < .1)
})

test_that("reweighing_wts with initial weights", {
  skip_on_cran()
  t1 = tsk("compas")
  t2 = t1$clone()
  t2$set_col_roles("age", "weight")

  p1 = po("reweighing_wts")
  p2 = p1$clone()

  ot1 = p1$train(list(t1))[[1]]
  ot2 = p2$train(list(t2))[[1]]
  w1 = ot1$weights$weight * t1$data(cols = "age")[["age"]]
  w2 = ot2$weights$weight
  expect_true(abs(mean(w1 - w2)) < 1e-2)
})

test_that("reweighing errors on multiclass", {
  skip_on_cran()
  t = tsk("iris")
  t$set_col_roles("Petal.Length", "pta")
  expect_error(po("reweighing_wts")$train(list(t))[[1]], "Only binary")
  expect_error(po("reweighing_os")$train(list(t))[[1]], "Only binary")
})


test_that("reweighing int to char conversion", {
  skip_on_cran()
  task = tsk("adult_train")$filter(1:300)
  dt = task$data()
  
  # integer
  dt[, sex := as.integer(sex)]
  t = TaskClassif$new("adult_int", backend = dt, target = "target")
  t$col_roles$pta = "sex"
  tsk = po("reweighing_wts")$train(list(t))[[1]]
  expect_true(tsk$col_roles$weight == "reweighing.WEIGHTS")
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")), tsk$weights)
  dt = dt[, mean(weight) * .N, by = .(sex, target)][, sum(V1 / sum(V1)), by = "target"]
  expect_true(all(abs(dt$V1 - 1) < 1e-3))  

  # numeric
  dt = task$data()
  dt[, sex := as.numeric(sex)]
  t = TaskClassif$new("adult_int", backend = dt, target = "target")
  t$col_roles$pta = "sex"
  tsk = po("reweighing_wts")$train(list(t))[[1]]
  expect_true(tsk$col_roles$weight == "reweighing.WEIGHTS")
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")), tsk$weights)
  dt = dt[, mean(weight) * .N, by = .(sex, target)][, sum(V1 / sum(V1)), by = "target"]
  expect_true(all(abs(dt$V1 - 1) < 1e-3))  

  # ordered
  dt = task$data()
  dt[, sex := as.ordered(sex)]
  t = TaskClassif$new("adult_int", backend = dt, target = "target")
  t$col_roles$pta = "sex"
  tsk = po("reweighing_wts")$train(list(t))[[1]]
  expect_true(tsk$col_roles$weight == "reweighing.WEIGHTS")
  dt = cbind(tsk$data(cols = c("..row_id", "sex", "target")), tsk$weights)
  dt = dt[, mean(weight) * .N, by = .(sex, target)][, sum(V1 / sum(V1)), by = "target"]
  expect_true(all(abs(dt$V1 - 1) < 1e-3))  
})
