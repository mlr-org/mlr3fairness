#' @title PipeOpExplicitPta
#' @usage NULL
#' @name mlr_pipeops_explicit_pta
#' @format [R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#'   Turns the column with col_role 'pta' into an explicit separate column prefixed with ".._internal_pta_".
#'   This keeps it from getting changed or adapted by subsequent pipelines that operate on the feature pta.
#
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [PipeOpTaskPreproc].
#'
#' @section Fields:
#' Only fields inherited from [PipeOpTaskPreproc]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [PipeOpTaskPreproc]/[PipeOp].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
#' @examples
#' library(mlr3pipelines)
#'
#' epta = po("explicit_pta")
#' epta$train(list(tsk("adult_train")))
#'
PipeOpExplicitPta = R6Class("PipeOpExplicitPta",
  inherit = PipeOpTaskPreproc,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][PipeOp] R6 class.
    #'
    #' @param id `character` \cr
    #'   The PipeOps identifier in the PipeOps library.
    #' @param param_vals `list` \cr
    #'   The parameter values to be set. See `Parameters`.
    initialize = function(id = "explicit_pta", param_vals = list()) {
      super$initialize(id, param_set = ParamSet$new(), param_vals = param_vals)
    }
  ),

  private = list(
    .train_task = function(task) {
        private$.transform_task(task)
    },

    .predict_task = function(task) {
        private$.transform_task(task)
    },

    .transform_task = function(task) {
      assert_pta_task(task)
      pta = task$col_roles$pta
      if (any(startsWith(task$feature_names, "..internal_pta"))) {
          stop("Task already has an explicit ..internal_pta column")
      }
      dt = task$data(cols = c(task$backend$primary_key, pta))
      newpta = paste0("..internal_pta_", pta)
      setnames(dt, pta, newpta)
      task$cbind(dt)
      task$set_col_roles(newpta, "pta")  
      task$set_col_roles(pta, remove_from =  "pta")  
      return(task)
    }
  )
)
