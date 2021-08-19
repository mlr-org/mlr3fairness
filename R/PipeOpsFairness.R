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
#' * `param_vals` :: `list`
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
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examples
#' library(mlr3pipelines)
#' library(mlr3fairness)
#' library(mlr3)
#'
#' reweighing = po("reweighing")
#' learner_po = po("learner", learner = lrn("classif.rpart"))
#'
#' data = tsk("adult_train")
#' graph = reweighing %>>% learner_po
#' glrn = GraphLearner$new(graph)
#' glrn$train(data)
#' tem = glrn$predict(data)
#' tem$confusion
PipeOpReweighing = R6Class("PipeOpReweighing",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][`PipeOp`] R6 class.
    #'
    #' @param id The PipeOps identifier in the PipeOps library.
    #' @param param_vals The parameter values to be set. There are two parameters that could be set:
    #' * temperature: controls the proportion between constant weight and reweighing weight. Default set to be 1
    #' * const_weight: the constant weight. Default set to be 1
    #' Here is how it works:
    #' first_weight = (1 - temperature) x const_weight + temperature x reweighing_weight
    #' final_reweighing_weight = first_weight * old_weight (if old weight exist, otherwise oldweight = 1)
    initialize = function(id = "reweighing", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("temperature", lower = 0, upper = Inf, tags = "train"),
        ParamDbl$new("const_weight", lower = 0, upper = Inf, tags = "train")
      ))
      ps$values = list(temperature = 1, const_weight = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals,
                       task_type = "TaskClassif", tags = "imbalanced data")
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

      weights = get_reweighing_weights(data, target, positive, pta, privileged)
      tem_data = data[,c(pta, target), with=F]
      index_pri_pos = data[list(privileged, positive), on = c(pta, target), which = T]
      index_pri_neg = setdiff(data[list(privileged), on = pta, which = T], index_pri_pos)
      index_unpri_pos = setdiff(data[list(positive), on = target, which = T], index_pri_pos)
      index_unpri_neg = setdiff(c(1:dim(data)[1]), Reduce(union, list(index_pri_pos, index_pri_neg, index_unpri_pos)))

      wcol = data.table(rep(0, dim(data)[1]))
      wcol[index_pri_pos] = weights[1]
      wcol[index_pri_neg] = weights[2]
      wcol[index_unpri_pos] = weights[3]
      wcol[index_unpri_neg] = weights[4]

      tem = self$param_set$values$temperature
      wcol = (1 - tem) * self$param_set$values$const_weight + tem * wcol
      wcol = ifelse(identical(task$col_roles$weight, character(0)),
                    wcol,
                    wcol * task$weights$weight)
      wcol = setnames(as.data.table(wcol), weightcolname)

      task$cbind(wcol)
      task$col_roles$feature = setdiff(task$col_roles$feature, weightcolname)
      task$col_roles$weight = weightcolname
      task
    },

    .predict_task = identity
  )
)
