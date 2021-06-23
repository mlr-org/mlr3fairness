#' @title Measure Registry
#'
#' @description
#' The [environment()] `measures` keeps track of all measures in this package.
#' It stores meta information such as minimum, maximum or if the
#' measure must be minimized or maximized.
#'
#' @export
#' @examples
#' names(measures)
#' measures$tpr
measures = new.env(parent = emptyenv())

# adds items to registry
add_measure = function(obj, title, type, lower, upper, minimize) {
  id = deparse(substitute(obj))
  assign(id, list(
    id = id,
    type = assert_choice(type, c("binary", "classif", "regr")),
    title = assert_string(title),
    lower = assert_number(lower),
    upper = assert_number(upper),
    predict_type = intersect(names(formals(obj)), c("response", "prob", "se")),
    minimize = assert_flag(minimize, na.ok = TRUE,),
    properties = c("requires_task")
  ), envir = measures)
}
