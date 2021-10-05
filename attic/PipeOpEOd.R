if (FALSE) {
  t = tsk("compas")
  l = po("learner_cv", lrn("classif.rpart"))
  ot = l$train(list(t))[[1]]
  pta = ot$col_roles$pta
  tgt = ot$col_roles$target
  prd = ot$col_roles$feature

  dt = ot$data(cols = c(pta, tgt, prd))
  dt[, colnames(dt) := map(.SD, as.factor), .SDcols = colnames(dt)]
  br = dt[, .N, by = pta][["N"]]

  r = dt[, map(list(fpr, fnr, tpr, tnr, .N), function(fn) fn(get(tgt), get(prd), ot$positive)), by = pta]
  names(r) = c(pta, c("fpr", "fnr", "tpr", "tnr", "base_rate"))
  r

  c = np.array([fpr0 - tpr0, tnr0 - fnr0, fpr1 - tpr1, tnr1 - fnr1])
  cst = c(
    r$fpr[1] - r$tpr[1],
    r$tnr[1] - r$fnr[1],
    r$fpr[2] - r$fpr[2],
    r$tnr[2] - r$fnr[2]
  )

        #  M. Hardt, E. Price, and N. Srebro, "Equality of Opportunity in
        #    Supervised Learning," Conference on Neural Information Processing
        #    Systems, 2016.
        #  G. Pleiss, M. Raghavan, F. Wu, J. Kleinberg, and
        #    K. Q. Weinberger, "On Fairness and Calibration," Conference on Neural
        #    Information Processing Systems, 2017.

}