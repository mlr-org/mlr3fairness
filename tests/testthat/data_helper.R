#Data Task generator
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
