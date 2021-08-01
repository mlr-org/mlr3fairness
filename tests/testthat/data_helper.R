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

simple_pred_data <- function() {
  PredictionClassif$new(row_ids = c(1:16),
                        truth = as.factor(c(1,1,2,2,1,1,2,1,2,2,2,1,1,1,2,1)),
                        response = as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)))
}

adult_benchmark_result <- function(){
  design = benchmark_grid(
    tasks = tsk("adult_train"),
    learners = lrns(c("classif.ranger", "classif.rpart"),
                    predict_type = "prob", predict_sets = c("train", "test")),
    resamplings = rsmps("cv", folds = 3)
  )

  return(benchmark(design))
}

adult_resample_result <- function(){
  task = tsk("adult_train")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv")
  resampling$instantiate(task)
  rr = resample(task, learner, resampling)
  return(rr)
}

adult_rpart_prob_predictions <- function(){
  data_task = tsk("adult_train")
  learner = lrn("classif.ranger", predict_type = "prob")
  learner$train(data_task)
  predictions = learner$predict(data_task)
  return(predictions)
}
