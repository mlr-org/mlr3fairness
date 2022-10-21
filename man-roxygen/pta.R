#' @section Protected Attributes:
#'   The protected attribute is specified as a `col_role` in the corresponding [`Task()`]:\cr
#'     `<Task>$col_roles$pta = "name_of_attribute"` \cr
#'   This also allows specifying more than one protected attribute, 
#'   in which case fairness will be considered on the level of intersecting groups defined by all columns
#'   selected as a predicted attribute.
