library(mlr3learners)

simple_test_data <- function(need_pta = T) {
  example_data <- data.frame(
    value = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
    variable = c(3,1,4,8,5,41,22,3,4,29,2,13,4,26,2,34),
    pta = as.factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)))
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  if(need_pta) task$col_roles$pta = "pta"
  return(task)
}

medium_test_data <- function(need_pta = T) {
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

biased_test_data <- function(need_pta = T) {
  set.seed(5)
  example_data <- data.frame(
    value = as.factor(sample.int(2, 100, T)),
    variable.a = c(sample(c(1:50), 80, T), sample(c(80:100), 20, T)),
    variable.b = c(sample(c(1:50), 80, T), sample(c(80:100), 20, T)),
    variable.c = c(sample(c(1:50), 80, T), sample(c(80:100), 20, T)),
    variable.d = sample.int(100, 100, T),
    pta = as.factor(c(rep(1,80), rep(2, 20)))
  )
  b = as_data_backend(example_data)
  task = mlr3::TaskClassif$new("example", b, target = "value")
  if(need_pta) task$col_roles$pta = "pta"
  return(task)
}

simple_pred_data <- function() {
  PredictionClassif$new(row_ids = c(1:16),
                        truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
                        response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)))
}

simple_benchmark_result <- function(){
  design = benchmark_grid(
    tasks = medium_test_data(),
    learners = lrns(c("classif.ranger", "classif.rpart"),
                    predict_type = "prob", predict_sets = c("train", "test")),
    resamplings = rsmps("cv", folds = 3)
  )

  return(benchmark(design))
}

simple_resample_result <- function(){
  task = medium_test_data()
  learner = lrn("classif.rpart")
  resampling = rsmp("cv")
  resampling$instantiate(task)
  rr = resample(task, learner, resampling)
  return(rr)
}

simple_rpart_prob_predictions <- function(){
  data_task = medium_test_data()
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(data_task)
  predictions = learner$predict(data_task)
  return(predictions)
}
