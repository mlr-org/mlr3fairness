library(gh)

"~/articles/"
dir = tempdir()
report_file = paste0(dir, "/datasheet")
unlink(report_file, recursive = TRUE)
fp = report_datasheet(report_file)
render(fp)

report_file = paste0(dir, "/modelcard")
unlink(report_file, recursive = TRUE)
report_modelcard(report_file)
render(fp)


task = tsk("compas")
learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(task, learner, rsmp("cv", folds = 3L))
report_file = paste0(dir, "/fairness")
unlink(report_file, recursive = TRUE)
fp = report_fairness(report_file, list(task = task, resample_result = rr))
render(fp)
