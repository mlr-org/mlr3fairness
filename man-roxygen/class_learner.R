<%
lrn = mlr3::lrn(id)
%>
#' @description
#' Calls [<%=lrn$packages[2]%>::<%=caller%>] from package \CRANpkg{<%=lrn$packages[2]%>}.
#'
#' @section Dictionary: This [Learner] can be instantiated via the
#'   [dictionary][mlr3misc::Dictionary] [mlr_learners] or with the associated
#'   sugar function [lrn()]:
#' ```
#' mlr_learners$get("<%= id %>")
#' lrn("<%= id %>")
#' ```
#' 
#' @section Meta Information:
#' `r mlr3misc::rd_info(mlr3::lrn("<%= id %>"))`
#' @md
#'
#' @section Parameters:
#' `r mlr3misc::rd_info(mlr3::lrn("<%= id %>")$param_set)`
#' @md
#' 
#' @family fairness_learners
#' @keywords internal
