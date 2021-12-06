#' @title Reweighing to balance disparate impact metric
#'
#' @usage NULL
#' @name mlr_pipeops_reweighing
#' @format [R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adjusts class balance and protected group balance in order to achieve fair(er) outcomes.
#'
#' @section PipeOpReweighingWeights:
#' Adds a class weight column to the [Task][mlr3::Task] that different [`Learner`][mlr3::Learner]s
#' may be using. In case initial weights are present, those are multiplied with new weights.
#' Caution: Only fairness tasks are supported. Which means tasks need to have protected field.
#' `tsk$col_roles$pta`.
#'
#' @section PipeOpReweighingOversampling:
#' Oversamples a [Task][mlr3::Task] for more balanced ratios in subgroups and protected groups.
#' Can be used if a learner does not support weights.
#' Caution: Only fairness tasks are supported. Which means tasks need to have protected field.
#' `tsk$col_roles$pta`.
#'
#' @references
#' `r format_bib("kamiran12")`
#'
#' @section Construction:
#' ```
#' PipeOpReweighing*$new(id = "reweighing", param_vals = list())
#' ```
#' * `id` (`character(1)`).
#' * `param_vals` (`list()`)
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpTaskPreproc]. Instead of a [`Task`][mlr3::Task], a
#' [TaskClassif][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [Task][mlr3::Task] with added weights column according
#' to target class. The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [PipeOpTaskPreproc].
#'
#' @section Parameters:
#'  * `alpha` (`numeric()`): A number between 0 (no debiasing) and 1 (full debiasing).
#'
#' @section Internals:
#' Introduces, or overwrites, the "weights" column in the [Task][mlr3::Task].
#' However, the [Learner][mlr3::Learner] method needs to
#' respect weights for this to have an effect.
#'
#' The newly introduced column is named `reweighing.WEIGHTS`; there will be a naming conflict if this
#' column already exists and is *not* a weight column itself.
#'
#' @section Fields:
#' Only fields inherited from [PipeOpTaskPreproc]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [PipeOpTaskPreproc]/[`PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examples
#' library(mlr3pipelines)
#'
#' reweighing = po("reweighing_wts")
#' learner_po = po("learner", learner = lrn("classif.rpart"))
#'
#' data = tsk("adult_train")
#' graph = reweighing %>>% learner_po
#' glrn = GraphLearner$new(graph)
#' glrn$train(data)
#' tem = glrn$predict(data)
#' tem$confusion
PipeOpReweighingWeights = R6Class("PipeOpReweighingWeights",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][PipeOp] R6 class.
    #'
    #' @param id `character` \cr
    #'   The PipeOps identifier in the PipeOps library.
    #' @param param_vals `list` \cr
    #'   The parameter values to be set.
    #'   * alpha: controls the proportion between initial weight (1 if non existing) and reweighing weight.
    #'     Defaults to 1.
    #' Here is how it works:
    #' new_weight = (1 - alpha) * 1 + alpha x reweighing_weight
    #' final_weight = old_weight * new_weight
    initialize = function(id = "reweighing_wts", param_vals = list()) {
      ps = ps(
        alpha = p_dbl(0, 1, tags = "train")
      )
      ps$values = list(alpha = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals,
        task_type = "TaskClassif", packages = "mlr3fairness",
        tags = c("imbalanced data", "fairness"))
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }
      weightcolname = "reweighing.WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname) # nocov
      }
      assert_pta_task(task)

      wtab = compute_reweighing_weights(task, 1)
      wcol = task$data(cols = c(task$backend$primary_key, task$target_names, task$col_roles$pta))
      wcol = wcol[wtab, on = c(task$target_names, task$col_roles$pta)][, c(task$backend$primary_key, "wt"), with = FALSE]
      if (is.null(task$weights)) {
        initial_weights = rep(1, task$nrow)
      } else {
        initial_weights = task$weights[wcol]$weight
      }
      wcol = wcol[, wt := wt * initial_weights]
      setnames(wcol, c(task$backend$primary_key, weightcolname))
      task$cbind(wcol)

      if (length(task$col_roles$weight)) {
        task$set_col_roles(task$col_roles$weight, remove_from = "weight")
      }

      task$set_col_roles(weightcolname, "weight")
      task
    },

    .predict_task = identity
  )
)


#' @rdname mlr_pipeops_reweighing
#' @export
PipeOpReweighingOversampling = R6Class("PipeOpReweighingOversampling",
  inherit = mlr3pipelines::PipeOpTaskPreproc,

  public = list(
    #' @param id `character' \cr
    #'   The PipeOp's id.
    #' @param param_vals `list' \cr
    #'   A list of parameter values.
    initialize = function(id = "reweighing_os", param_vals = list()) {
      ps = ps(
        alpha = p_dbl(0, 1, tags = "train")
      )
      ps$values = list(alpha = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE,
        task_type = "TaskClassif", tags = c("imbalanced data", "fairness"))
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }
      assert_pta_task(task)

      sel = rnd = NULL
      pv = self$param_set$get_values()
      wtab = compute_reweighing_weights(task, pv$alpha)
      dt = task$data(cols = c(task$target_names, task$col_roles$pta, task$backend$primary_key))
      dt = dt[wtab, on = c(task$target_names, task$col_roles$pta)][, sel := 0]
      dt[wt > 1, sel := floor(wt)][wt > 1, wt := wt - sel][, rnd := runif(nrow(dt))]
      dt[wt >= rnd, sel := sel + 1]
      new_ids = sample(unlist(pmap(list(dt[[task$backend$primary_key]], dt[["sel"]]), rep)))
      task_filter_ex(task, new_ids)
    },

    .predict_task = identity
  )
)
