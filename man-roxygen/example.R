<%
lrn = mlr3::lrn(id)
%>
#' @examples
#' library("mlr3")
#' # stop example failing with warning if package not installed
#' learner = suppressWarnings(mlr3::lrn("<%= id %>"))
#' print(learner)
#'
#' # available parameters:
#' learner$param_set$ids()
