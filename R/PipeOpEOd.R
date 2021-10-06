
#' @title Equalized Odds Debiasing
#' @usage NULL
#' @name mlr_pipeops_equalized_odds
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#'   Fairness post-processing method to achieve equalized odds fairness.
#'   Works by randomly flipping a subset of predictions with pre-computed
#'   probabilities in order to satisfy equalized odds constraints.
#'
#' @details
#' For additional details, see:\cr
#' M. Hardt, E. Price, and N. Srebro, "Equality of Opportunity in
#'    Supervised Learning," Conference on Neural Information Processing
#'   Systems, 2016.
#' G. Pleiss, M. Raghavan, F. Wu, J. Kleinberg, and
#'   K. Q. Weinberger, "On Fairness and Calibration," Conference on Neural
#'   Information Processing Systems, 2017.
#'
#' @section Construction:
#' ```
#' PipeOpEOd*$new(id = "eod", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#' * `param_vals` :: `list`
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`]. Instead of a [`Task`][mlr3::Task], a
#' [`TaskClassif`][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [`Task`][mlr3::Task]. The output during prediction is
#' a [`PredictionClassif`][mlr3::PredictionClassif] with partially flipped predictions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#'  * `alpha` :: `numeric` A number between 0 (no debiasing) and 1 (full debiasing).
#'    Controls the debiasing strength by multiplying the flipping probabilities with alpha.
#'
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examples
#' library(mlr3pipelines)
#' library(mlr3fairness)
#' library(mlr3)
#'
#' eod = po("EOd")
#' learner_po = po("learner_cv", learner = lrn("classif.rpart"))
#'
#' data = tsk("adult_train")
#' graph = learner_po %>>% eod
#' glrn = GraphLearner$new(graph)
#' glrn$train(data)
#' tem = glrn$predict(data)
#' tem$confusion
PipeOpEOd= R6Class("PipeOpEOd",
  inherit = PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][`PipeOp`] R6 class.
    #'
    #' @param id `character` \cr
    #'   The PipeOps identifier in the PipeOps library.
    #' @param param_vals `list` \cr
    #'   The parameter values to be set. See `Parameters`.
    initialize = function(id = "EOd", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamDbl$new("alpha", lower = 0, upper = 1)
     ))
      ps$values = list(alpha = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskClassif",   predict = "TaskClassif"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionClassif"),
        tags = "fairness"
      )

    }
  ),
  private = list(

    .train = function(task) {
      task =  assert_pta_task(task[[1]])
      params = self$param_set$get_values(tags = "train")
      flips = private$.compute_flip_probs()
      self$state = map(flips, function(x) alpha * flips)
    },

    .predict = function(task) {
      flips = self$state$flip_probs
      # Widely used vars
      pta = task$col_roles$pta
      tgt = task$col_roles$target
      prd = task$col_roles$feature
      prv = task$levels(pta)[[pta]][1]

      # Obtain data
      dt = task$data(cols = c(task$backend$primary_key, pta, tgt, prd))
      dt[, c(tgt, prd) := map(.SD, as.factor), .SDcols = c(tgt, prd)]

      # Binary priviledged group indicator
      is_prv = dt[,get(pta) == prv]

      # Get indices to switch
      pp_idx = sample(which(dt[is_prv, get(prd) == task$positive]))
      pn_idx = sample(which(dt[is_prv, get(prd) != task$negative]))
      n2p_idx = pn_idx[seq_len(round( flips$sn2p    * length(pn_idx)))]
      p2n_idx = pp_idx[seq_len(round((1-flips$sp2p) * length(pp_idx)))]

      # Flip predictions
      dt[n2p_idx, (prd) := task$positive]
      dt[p2n_idx, (prd) := task$negative]

      if (task$col_roles$truth %in% colnames())
      # Convert to prediction
      set(dt, j = "row_ids", value = dt[[task$backend$primary_key]])
      set(dt, j = "response", value = dt[[prd]])
      set(dt, j = "truth", value = factor(truth, levels = levels(dt$response)))
      as_prediction_classif(dt[, c("row_ids", "truth", "response")])

    },
    .compute_flip_probs = function(task) {
      # Widely used vars
      pta = task$col_roles$pta
      tgt = task$col_roles$target
      prd = task$col_roles$feature
      pos = task$positive
      prv = task$levels(pta)[[pta]][1]

      # Obtain data
      dt = task$data(cols = c(pta, tgt, prd))
      dt[, colnames(dt) := map(.SD, as.factor), .SDcols = colnames(dt)]
      # Compue base rates
      br = dt[, .N, by = pta]
      sbr = br[get(pta) == prv][["N"]]
      obr = br[get(pta) != prv][["N"]]

      # Compute per-group metrics
      r = dt[, map(list(fpr, fnr, tpr, tnr, .N), function(fn) fn(get(tgt), get(prd), pos)), by = pta]
      names(r) = c(pta, c("fpr", "fnr", "tpr", "tnr", "base_rate"))
      r[, base_rate := base_rate / nrow(dt)]

      # Binary priviledged group indicator
      is_prv = dt[,get(pta) == prv]
      # True target
      y_true = dt[[tgt]]

      # Compute priviledged/unpriviledged pos. and negative samples
      sconst = dt[is_prv, get(prd) == pos]
      sflip =  dt[is_prv, get(prd) != pos]
      oconst = dt[!is_prv, get(prd) == pos]
      oflip =  dt[!is_prv, get(prd) != pos]

      # Matrix entry components
      sm_tn = (y_true[is_prv] != pos) & sflip
      sm_fn = (y_true[is_prv] == pos) & sflip
      sm_fp = (y_true[is_prv] != pos) & sconst
      sm_tp = (y_true[is_prv] == pos) & sconst
      om_tn = (y_true[!is_prv] != pos) & oflip
      om_fn = (y_true[!is_prv] == pos) & oflip
      om_fp = (y_true[!is_prv] != pos) & oconst
      om_tp = (y_true[!is_prv] == pos) & oconst

      ### Set up linear programming problem ###
      # Inequality constraints
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

      # Combine constraints
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

      sol = linprog::solveLP(cvec, bvec, Amat, const.dir = const_dir, lpSolve = TRUE, maxiter = 1e4, zero=1e-16)
      list("flip_probs" = setNames(as.list(sol$solution), c("sp2p", "sn2p", "op2p", "on2p")))
    }
  )
)

mlr_pipeops$add("EOd", PipeOpEOd)
