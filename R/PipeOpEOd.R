# The following code is heavily inspired by the post-processing module in
# https://github.com/Trusted-AI/AIF360
# In the following, we include the two original licenses included in this repository.
# re-licenced under LGPL-v3 here.
#
# Original work Copyright (c) 2017 Geoff Pleiss
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Modified work Copyright 2018 IBM Corporation
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.


#' @title Equalized Odds Debiasing
#' @usage NULL
#' @name mlr_pipeops_equalized_odds
#' @format \link[R6:R6Class]{R6::R6Class} object inheriting from 
#' \link[mlr3pipelines:PipeOpTaskPreproc]{mlr3pipelines::PipeOpTaskPreproc}/
#' \link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}.
#'
#' @description
#'   Fairness post-processing method to achieve equalized odds fairness.
#'   Works by randomly flipping a subset of predictions with pre-computed
#'   probabilities in order to satisfy equalized odds constraints.\cr
#'   NOTE: Carefully assess the correct privileged group.
#'
#' @references
#' `r format_bib("hardt_2016", "pleiss_2017")`
#'
#' @section Construction:
#' ```
#' PipeOpEOd*$new(id = "eod", param_vals = list())
#' ```
#' * `id` (`character(1))`.
#' * `param_vals` (`list()`)
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [mlr3pipelines::PipeOpTaskPreproc]. Instead of a [mlr3::Task], a
#' [mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [mlr3::Task]. The output during prediction is
#' a [mlr3::PredictionClassif] with partially flipped predictions.
#'
#' @section State:
#' The `$state` is a named list with the `$state` elements inherited from [mlr3pipelines::PipeOpTaskPreproc].
#'
#' @section Parameters:
#'  * `alpha` (numeric): A number between 0 (no debiasing) and 1 (full debiasing).
#'    Controls the debiasing strength by multiplying the flipping probabilities with alpha.
#'  * `privileged` (character): The privileged group.
#'
#'
#' @section Fields:
#' Only fields inherited from [mlr3pipelines::PipeOpTaskPreproc]/[mlr3pipelines::PipeOp].
#'
#' @section Methods:
#' Methods inherited from [mlr3pipelines::PipeOpTaskPreproc]/[mlr3pipelines::PipeOp].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examplesIf rlang::is_installed("rpart")
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' eod = po("EOd")
#' learner_po = po("learner_cv",
#'   learner = lrn("classif.rpart"),
#'   resampling.method = "insample"
#' )
#'
#' task = tsk("compas")
#' graph = learner_po %>>% eod
#' glrn = GraphLearner$new(graph)
#' glrn$train(task)
#'
#' # On a Task
#' glrn$predict(task)
#'
#' # On newdata
#' glrn$predict_newdata(task$data(cols = task$feature_names))
PipeOpEOd = R6Class("PipeOpEOd",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6::R6Class][mlr3pipelines::PipeOp] R6 class.
    #'
    #' @param id character \cr
    #'   The PipeOps identifier in the PipeOps library.
    #' @param param_vals list \cr
    #'   The parameter values to be set. See `Parameters`.
    initialize = function(id = "EOd", param_vals = list()) {
      ps = ps(
        alpha = p_dbl(0, 1, tags = "train"),
        privileged = p_uty(tags = "train")
      )
      ps$values = list(alpha = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskClassif", predict = "TaskClassif"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionClassif"),
        tags = "fairness", packages = c("mlr3fairness", "linprog")
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = assert_pta_task(input[[1L]], single = TRUE)
      params = self$param_set$get_values(tags = "train")
      # FIXME: which_max? -> random sampling in case of ties
      private$.privileged = params$privileged %??% names(which.max(table(task$data(cols = task$col_roles$pta))))
      flips = private$.compute_flip_probs(task)
      self$state = list(flip_probs = map(flips, function(x) params$alpha * x))
    },

    .predict = function(input) {
      task = assert_pta_task(input[[1L]], single = TRUE)
      flips = self$state$flip_probs
      # Widely used vars
      pta_ = task$col_roles$pta
      tgt_ = task$col_roles$target
      prd = task$col_roles$feature
      prv = private$.privileged

      # Obtain data
      dt = task$data(cols = c(task$backend$primary_key, pta_, tgt_, prd))
      dt[, c(tgt_, prd) := map(.SD, as.factor), .SDcols = c(tgt_, prd)]
      # Binary privileged group indicator
      is_prv = dt[, get(pta_) == prv]
      if (sum(is_prv) < 1) {
        stop("'privileged' needs to be a valid value in the 'pta' column!") # nocov
      }

      # privileged
      pn_idx = sample(which(dt[, is_prv & get(prd) == task$negative]))
      pp_idx = sample(which(dt[, is_prv & get(prd) == task$positive]))
      if (flips$sn2p > 0) {
        n2p_idx = pn_idx[seq_len(ceiling(flips$sn2p * length(pn_idx)))]
        dt[n2p_idx, (prd) := task$positive]
      }
      if (flips$sp2p > 0) {
        p2n_idx = pp_idx[seq_len(ceiling((1 - flips$sp2p) * length(pp_idx)))]
        dt[p2n_idx, (prd) := task$negative]
      }

      # Unprivileged
      pp_idx = sample(which(dt[, !is_prv & get(prd) == task$positive]))
      pn_idx = sample(which(dt[, !is_prv & get(prd) == task$negative]))
      if (flips$op2p > 0) {
        p2p_idx = pp_idx[seq_len(ceiling((1 - flips$op2p) * length(pp_idx)))]
        dt[p2p_idx, (prd) := task$positive]
      }
      if (flips$on2p > 0) {
        p2n_idx = pn_idx[seq_len(ceiling(flips$on2p * length(pn_idx)))]
        dt[p2n_idx, (prd) := task$negative]
      }

      # Convert to prediction
      set(dt, j = "row_ids", value = dt[[task$backend$primary_key]])
      set(dt, j = "response", value = dt[[prd]])
      if (tgt_ %in% colnames(dt)) {
        set(dt, j = "truth", value = factor(dt[[tgt_]], levels = levels(dt$response)))
      } else {
        set(dt, j = "truth", value = factor(NA, levels = levels(dt$response))) # nocov
      }
      list(as_prediction_classif(dt[, c("row_ids", "truth", "response")]))
    },

    .compute_flip_probs = function(task) {
      # Widely used vars
      pta_ = task$col_roles$pta
      tgt_ = task$col_roles$target
      prd = task$col_roles$feature
      pos = task$positive
      prv = private$.privileged

      # Obtain data
      dt = task$data(cols = c(pta_, tgt_, prd))
      dt[, colnames(dt) := map(.SD, as.factor), .SDcols = colnames(dt)]

      # Compute base rates function
      base_rate = function(truth, prediction, positive) mean(truth == positive)

      # Compute per-group metrics
      r = dt[, map(list(fpr, fnr, tpr, tnr, base_rate), function(fn) fn(get(tgt_), get(prd), pos)), by = pta_]
      names(r) = c(pta_, c("fpr", "fnr", "tpr", "tnr", "base_rate"))
      r[, "dpr" := fpr - tpr][, "dnr" := tnr - fnr]

      # Compute error differences in the different groups and base_rates
      cvec = c(r[get(pta_) == prv]$dpr, r[get(pta_) == prv]$dnr, r[get(pta_) != prv]$dpr, r[get(pta_) != prv]$dnr)
      sbr = r[get(pta_) == prv]$base_rate
      obr = r[get(pta_) != prv]$base_rate


      # Binary privileged group indicator
      is_prv = dt[, get(pta_) == prv]
      if (sum(is_prv) < 1) {
        stop("'privileged' needs to be a valid value in the 'pta' column!")
      }

      # True target
      y_true = dt[[tgt_]]

      # Compute privileged/unprivileged pos. and negative samples
      sconst = dt[is_prv, get(prd) == pos] # Yh[A0] == +
      sflip = dt[is_prv, get(prd) != pos] # Yh[A0] == -
      oconst = dt[!is_prv, get(prd) == pos]
      oflip = dt[!is_prv, get(prd) != pos]

      # Matrix entry components
      sm_tn = (y_true[is_prv] != pos) & sflip
      sm_fn = (y_true[is_prv] == pos) & sflip # Y[A0] == + & Yh[A0] == -
      sm_fp = (y_true[is_prv] != pos) & sconst
      sm_tp = (y_true[is_prv] == pos) & sconst
      om_tn = (y_true[!is_prv] != pos) & oflip
      om_fn = (y_true[!is_prv] == pos) & oflip
      om_fp = (y_true[!is_prv] != pos) & oconst
      om_tp = (y_true[!is_prv] == pos) & oconst

      ### Set up linear programming problem ###
      # Inequality constraints
      A_ineq = rbind( # nolint start
        c( 1,  0,  0,  0),
        c(-1,  0,  0,  0),
        c( 0,  1,  0,  0),
        c( 0, -1,  0,  0),
        c( 0,  0,  1,  0),
        c( 0,  0, -1,  0),
        c( 0,  0,  0,  1),
        c( 0,  0,  0, -1)
      ) # nolint end
      b_ineq = c(1, 0, 1, 0, 1, 0, 1, 0)

      # Equality constraints
      A_eq = cbind(c(
        (mean(sconst * sm_tp) - mean(sflip * sm_tp)) / sbr,
        (mean(sflip * sm_fn) - mean(sconst * sm_fn)) / sbr,
        (mean(oflip * om_tp) - mean(oconst * om_tp)) / obr,
        (mean(oconst * om_fn) - mean(oflip * om_fn)) / obr),
      c(
        (mean(sconst * sm_fp) - mean(sflip * sm_fp)) / (1 - sbr),
        (mean(sflip * sm_tn) - mean(sconst * sm_tn)) / (1 - sbr),
        (mean(oflip * om_fp) - mean(oconst * om_fp)) / (1 - obr),
        (mean(oconst * om_tn) - mean(oflip * om_tn)) / (1 - obr)
      )
      )
      # (Yh[A0] == +) * (Y[A0] == +) & (Yh[A0] == -)
      b_eq = c(
        (mean(oflip * om_tp) + mean(oconst * om_fn)) / obr - (mean(sflip * sm_tp) + mean(sconst * sm_fn)) / sbr,
        (mean(oflip * om_fp) + mean(oconst * om_tn)) / (1 - obr) - (mean(sflip * sm_fp) + mean(sconst * sm_tn)) / (1 - sbr)
      )

      # Combine constraints
      Amat = rbind(
        cbind(A_ineq, t(matrix(0, nrow = nrow(A_eq), ncol = nrow(A_ineq)))),
        cbind(matrix(0, nrow = ncol(A_eq), ncol = nrow(A_eq)), t(A_eq))
      )
      Amat = rbind(A_ineq, t(A_eq))
      bvec = c(b_ineq, b_eq)
      # Directions for constraints
      const_dir = c(rep("<=", length(b_ineq)), rep("==", length(b_eq)))
      #  Sovle
      sol = linprog::solveLP(cvec, bvec, Amat, const.dir = const_dir, lpSolve = TRUE)
      set_names(as.list(sol$solution), c("sp2p", "sn2p", "op2p", "on2p"))
    },

    .privileged = character()
  )
)
