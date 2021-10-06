if (FALSE) {
  library(mlr3fairness)
  t = tsk("compas")
  l = po("learner_cv", lrn("classif.rpart", cp = 0.001, maxdepth = 8))
  ot = l$train(list(t))[[1]]
  pta = ot$col_roles$pta
  tgt = ot$col_roles$target
  prd = ot$col_roles$feature
  pos = ot$positive
  prv = ot$levels(pta)[[pta]][1]
  dt = ot$data(cols = c(pta, tgt, prd))
  dt[, colnames(dt) := map(.SD, as.factor), .SDcols = colnames(dt)]
  table(dt$two_year_recid, dt$classif.rpart.response, dt$sex)
  br = dt[, .N, by = pta]
  sbr = br[get(pta) == prv][["N"]]
  obr = br[get(pta) != prv][["N"]]

  r = dt[, map(list(fpr, fnr, tpr, tnr, .N), function(fn) fn(get(tgt), get(prd), pos)), by = pta]
  names(r) = c(pta, c("fpr", "fnr", "tpr", "tnr", "base_rate"))
  r[, base_rate := base_rate / nrow(dt)]
  r

  is_prv = dt[,get(pta) == prv]

  # Compute priviledged/unpriviledged pos. and negative samples
  sconst = dt[is_prv, get(prd) == pos]
  sflip =  dt[is_prv, get(prd) != pos]
  oconst = dt[!is_prv, get(prd) == pos]
  oflip =  dt[!is_prv, get(prd) != pos]

  y_true = dt[[tgt]]

  sm_tn = (y_true[is_prv] != pos) & sflip
  sm_fn = (y_true[is_prv] == pos) & sflip
  sm_fp = (y_true[is_prv] != pos) & sconst
  sm_tp = (y_true[is_prv] == pos) & sconst
  om_tn = (y_true[!is_prv] != pos) & oflip
  om_fn = (y_true[!is_prv] == pos) & oflip
  om_fp = (y_true[!is_prv] != pos) & oconst
  om_tp = (y_true[!is_prv] == pos) & oconst

  # Inequality constraints (upper, lower)
  A_ineq = rbind(
    c( 1,  0,  0,  0),
    c(-1,  0,  0,  0),
    c( 0,  1,  0,  0),
    c( 0, -1,  0,  0),
    c( 0,  0,  1,  0),
    c( 0,  0, -1,  0),
    c( 0,  0,  0,  1),
    c( 0,  0,  0, -1)
  )
  b_ineq = c(1, 0, 1, 0, 1, 0, 1, 0)

  # Equality constraints
  A_eq = cbind(c(
      mean(sconst*sm_tp) - mean(sflip  * sm_tp) / sbr,
      mean(sflip*sm_fn)  - mean(sconst * sm_fn) / sbr,
      mean(oflip*om_tp)  - mean(oconst * om_tp) / obr,
      mean(oconst*om_fn) - mean(oflip  * om_fn) / obr),
    c(
      mean(sconst*sm_fp) - mean(sflip  * sm_fp) / (1-sbr),
      mean(sflip*sm_tn)  - mean(sconst * sm_tn) / (1-sbr),
      mean(oflip*om_fp)  - mean(oconst * om_fp) / (1-obr),
      mean(oconst*om_tn) - mean(oflip  * om_tn) / (1-obr)
    )
  )
  b_eq = c(
    (mean(oflip*om_tp) + mean(oconst*om_fn)) / obr     - (mean(sflip*sm_tp) + mean(sconst*sm_fn)) / sbr,
    (mean(oflip*om_fp) + mean(oconst*om_tn)) / (1-obr) - (mean(sflip*sm_fp) + mean(sconst*sm_tn)) / (1-sbr)
  )


  Amat = rbind(
    cbind(A_ineq, t(matrix(0, nrow = nrow(A_eq), ncol = nrow(A_ineq)))),
    cbind(matrix(0, nrow = ncol(A_eq), ncol = nrow(A_eq)), t(A_eq))
  )
  Amat = rbind(A_ineq, t(A_eq))
  bvec = c(b_ineq, b_eq)
  cvec = c(
    r$fpr[1] - r$tpr[1],
    r$tnr[1] - r$fnr[1],
    r$fpr[2] - r$fpr[2],
    r$tnr[2] - r$fnr[2]
  )
  const_dir = c(rep("<=", length(b_ineq)), rep("==", length(b_eq)))
  solveLP(cvec, bvec, Amat, const.dir = const_dir, lpSolve = TRUE, maxiter = 1e4, zero=1e-16)
  self$coefficients
  library(reticulate)

  o = import("scipy.optimize")
  o$linprog(cvec[1:4], A_ineq, b_ineq, t(A_eq), b_eq)







#'
#'




}