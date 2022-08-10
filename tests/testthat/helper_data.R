library(mlr3learners)

test_task_small = function(need_pta = TRUE) {
  example_data = data.frame(
    value = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    variable = c(3, 1, 4, 8, 5, 41, 22, 3, 4, 29, 2, 13, 4, 26, 2, 34),
    pta = as.factor(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)))
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  if (need_pta) task$col_roles$pta = "pta"
  return(task)
}

pred_small = function() {
  PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1)),
    response = as.factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2))
  )
}

test_tasks = function() {
  tasks = list(tsk("adult_train")$filter(1:500), tsk("compas")$filter(1:500))
}

test_measures = function() {
  msrs(c("fairness.tpr", "fairness.fnr"))
}

test_bmr = function() {
  lrns = list(lrn("classif.rpart", predict_type = "prob"), lrn("classif.featureless", predict_type = "prob"))
  tasks = test_tasks()
  fairness_measures = test_measures()
  benchmark(benchmark_grid(tasks = tasks, learners = lrns, rsmp("cv", folds = 3L)))
}

make_classif_regr_task = function(data, task_type) {
  if (task_type == "classif") {
    data$value = as.factor(data$value)
    b = as_data_backend(data)
    task = mlr3::TaskClassif$new("example", b, target = "value")
  } else if (task_type == "regr") {
    b = as_data_backend(data)
    task = mlr3::TaskRegr$new("example", b, target = "value")
  } else {
    stop("Task type must be classif or regr!")
  }
  return(task)
}

# One non-binary pta
test_task_multipta = function(task_type, need_pta = TRUE) {
  example_data = data.frame(
    value = rep(1:2, 10),
    variable = rep(rep(c(1, 4, 3, 6), each = 5)),
    var2 = rnorm(20),
    pta = as.factor(rep(1:4, 5))
  )
  task = make_classif_regr_task(example_data, task_type)
  if (need_pta) task$col_roles$pta = "pta"
  return(task)
}

# Two ptas
test_task_intersect = function(task_type, need_pta = TRUE) {
  example_data = data.frame(
    value = rep(1:2, 10),
    variable = rep(rep(c(1, 4, 3, 6), each = 5)),
    var2 = rnorm(20),
    pta1 = as.factor(rep(1:2, 5)),
    pta2 = as.factor(rep(1:2, each = 5))
  )
  task = make_classif_regr_task(example_data, task_type)
  if (need_pta) task$col_roles$pta = c("pta1", "pta2")
  return(task)
}

# Multiclass outcome / two ptas
test_task_multicl = function(task_type, need_pta = TRUE) {
  example_data = data.frame(
    value = rep(1:4, 5),
    variable = rep(rep(c(1, 4, 3, 6), each = 5)),
    var2 = rnorm(20),
    pta1 = as.factor(rep(1:2, 5)),
    pta2 = as.factor(rep(1:2, each = 5))
  )
  task = make_classif_regr_task(example_data, task_type)
  if (need_pta) task$col_roles$pta = c("pta1", "pta2")
  return(task)
}

# continuous protected attribute
test_task_contpta = function(task_type, need_pta = TRUE) {
  example_data = data.frame(
    value = rep(1:4, 5),
    variable = rep(rep(c(1, 4, 3, 6), each = 5)),
    var2 = rnorm(20),
    pta = rnorm(20)
  )
  task = make_classif_regr_task(example_data, task_type)
  if (need_pta) task$col_roles$pta = "pta"
  return(task)
}
