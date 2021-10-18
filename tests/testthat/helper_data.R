library(mlr3learners)

test_task_small = function(need_pta = T) {
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
