library(mlr3learners)

test_task_small <- function(need_pta = T) {
  example_data <- data.frame(
    value = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)))
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  if(need_pta) task$col_roles$pta = "pta"
  return(task)
}

test_task_big <- function(need_pta = T) {
  set.seed(5)
  example_data <- data.frame(
    value = as.factor(sample.int(2, 100, T)),
    variable.a = sample.int(100, 100, T),
    variable.b = sample.int(100, 100, T),
    variable.c = sample.int(100, 100, T),
    variable.d = sample.int(100, 100, T),
    pta = as.factor(sample.int(2, 100, T))
  )
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  if(need_pta) task$col_roles$pta = "pta"
  return(task)
}

pred_small <- function() {
  PredictionClassif$new(
    row_ids = c(1:16),
    truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2))
  )
}
