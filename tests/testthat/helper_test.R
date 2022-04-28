# check plot satisfy those three conditions
# is_ggplot, no errors, no warnings
check_plots = function(ggplot_obj) {
  expect_true(is.ggplot(ggplot_obj))
  expect_error(ggplot_obj, NA)
  expect_warning(ggplot_obj, NA)
}

library(mlr3)
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "helper", full.names = TRUE), source)


run_autotest = function(learner, N = 30L, exclude = NULL, predict_types = learner$predict_types, check_replicable = TRUE) {
  learner = learner$clone(deep = TRUE)
  id = learner$id
  tasks = generate_tasks(learner, N = N)
  map(tasks, function(x) {
    pta = data.table(
      pta = sample(factor(rep_len(c("f1", "f2"), x$nrow), levels = c("f1", "f2"))),
      noisevar = runif(x$nrow)
    )
    x$cbind(pta)
    x$col_roles$pta = "pta"
  })
  
  if (!is.null(exclude)) {
    tasks = tasks[!grepl(exclude, names(tasks))]
  }


  sanity_runs = list()
  make_err = function(msg, ...) {
    run$ok = FALSE
    run$error = sprintf(msg, ...)
    run
  }

  # FIXME: Requires CRAN update for mlr3
  # param_tags = unique(unlist(learner$param_set$tags))
  # if (!test_subset(param_tags, mlr_reflections$learner_param_tags)) {
  #   return(make_err("Invalid parameter tag(s), check `mlr_reflections$learner_param_tags`."))
  # }

  for (task in tasks) {
    for (predict_type in predict_types) {
      learner$id = sprintf("%s:%s", id, predict_type)
      learner$predict_type = predict_type

      run = run_experiment(task, learner)
      if (!run$ok) {
        return(run)
      }

      # re-run task with same seed for feat_all
      if (startsWith(task$id, "feat_all")) {
        repeated_run = run_experiment(task, learner, seed = run$seed)

        if (!repeated_run$ok) {
          return(repeated_run)
        }

        if (check_replicable && !isTRUE(all.equal(as.data.table(run$prediction), as.data.table(repeated_run$prediction)))) {
          return(make_err("Different results for replicated runs using fixed seed %i", run$seed))
        }
      }

      if (task$task_type == "classif" && task$id == "sanity") {
        sanity_runs[[predict_type]] = run
      }
    }
    if (task$task_type == "classif" && length(sanity_runs) > 1L) {
      responses = lapply(sanity_runs, function(r) r$prediction$response)
      if (!isTRUE(Reduce(all.equal, responses))) {
        return(make_err("Response is different for different predict types"))
      }
    }
  }
  return(TRUE)
}
