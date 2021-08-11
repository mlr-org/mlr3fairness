#' @title Reweighing to balance disparate impact metric
#'
#' @usage NULL
#' @name mlr_pipeops_reweighing
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds a class weight column to the [`Task`][mlr3::Task] that different [`Learner`][mlr3::Learner]s may be
#' able to use to balance the disparate impack metric. Such approach is usually called reweighing method.
#'
#' Only binary [classification tasks][mlr3::TaskClassif] are supported.
#'
#' Caution: Only fairness tasks are supported. Which means tasks need to have protected field. `tsk$col_roles$pta`
#'
#' @section Construction:
#' ```
#' PipeOpReweighing$new(id = "reweighing", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`]. Instead of a [`Task`][mlr3::Task], a
#' [`TaskClassif`][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [`Task`][mlr3::Task] with added weights column according to target class.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`];
#' .
#' @section Internals:
#' Introduces, or overwrites, the "weights" column in the [`Task`][mlr3::Task]. However, the [`Learner`][mlr3::Learner] method needs to
#' respect weights for this to have an effect.
#'
#' The newly introduced column is named `reweighing.WEIGHTS`; there will be a naming conflict if this column already exists and is *not* a
#' weight column itself.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#' Helper Method: `get_weights()`. Return the weights used for reweighing algorithm calculation \cr
#' `c(W_positive_privileged, W_negative_privileged, W_positive_unprivileged, W_negative_unprivileged)``
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examples
#' NULL # Place Holder
PipeOpReweighing = R6Class("PipeOpReweighing",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][`PipeOp`] R6 class.
    #'
    #' @param id The PipeOps identifier in the PipeOps library.
    initialize = function(id = "reweighing") {
      super$initialize(id, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_task = function(task) {
      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }

      weightcolname = "reweighing.WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      data = task$data()
      positive = task$positive
      pta = task$col_roles$pta
      privileged = unlist(task$data()[1,task$col_roles$pta, with = F])
      target = task$target_names

      weights = private$get_weights(task, data, positive, pta, privileged)
      tem_data = data[,c(pta, target), with=F]
      index_pri_pos = data[list(privileged, positive), on = c(pta, target), which = T]
      index_pri_neg = setdiff(data[list(privileged), on = pta, which = T], index_pri_pos)
      index_unpri_pos = setdiff(data[list(positive), on = target, which = T], index_pri_pos)
      index_unpri_neg = setdiff(c(1:dim(data)[1]), Reduce(union, list(index_pri_pos, index_pri_neg, index_unpri_pos)))

      wcol = setnames(data.table(rep(0, dim(data)[1])), weightcolname)
      wcol[index_pri_pos] = weights[1]
      wcol[index_pri_neg] = weights[2]
      wcol[index_unpri_pos] = weights[3]
      wcol[index_unpri_neg] = weights[4]

      task$cbind(wcol)
      task$col_roles$feature = union(task$col_roles$feature, weightcolname)
      task$col_roles$weight = weightcolname
      task
    },

    .predict_task = function(task){
      return( private$.train_task(task) )
    },

    # Get the weights used for reweighing algorithm (Helper Function)
    #
    # @param task The task
    # @param data The complete data.table in task
    # @param positive Default positive label in target
    # @param pta The name of the protected attribute, (task$col_roles$pta)
    # @param privileged The privileged group in protected attribute
    #
    # @return c(weight_positive_privileged, weight_negative_privileged, weight_positive_unprivileged, weight_negative_unprivileged)
    get_weights = function(task, data, positive, pta, privileged) {
      conditional_counts = binary_classif_pta_count(task, data, positive, pta, privileged)
      N_pos_privileged = conditional_counts[1]
      N_pos_unprivileged = conditional_counts[2]
      N_neg_privileged = conditional_counts[3]
      N_neg_unprivileged = conditional_counts[4]

      N_all = dim(data)[1]
      N_positive = N_pos_privileged + N_pos_unprivileged
      N_negative = N_neg_privileged + N_neg_unprivileged
      N_privileged = N_pos_privileged + N_neg_privileged
      N_unprivileged = N_pos_unprivileged + N_neg_unprivileged

      W_positive_privileged = (N_positive * N_privileged)/(N_all * N_pos_privileged)
      W_negative_privileged = (N_negative * N_privileged)/(N_all * N_neg_privileged)
      W_positive_unprivileged = (N_positive * N_unprivileged)/(N_all * N_neg_unprivileged)
      W_negative_unprivileged = (N_negative * N_unprivileged)/(N_all * N_neg_unprivileged)

      return( c(W_positive_privileged, W_negative_privileged, W_positive_unprivileged, W_negative_unprivileged) )
    }
  )
)

mlr_pipeops$add("reweighing", PipeOpReweighing)
