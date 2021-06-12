#' @importFrom utils getFromNamespace data
"_PACKAGE"

register_tasks = function() {
  x = getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("adult_train", get_adult_task_train())
  x$add("adult_test", get_adult_task_test())
  x$add("compas", get_compas_task())
}

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  if (isNamespaceLoaded("mlr3")) {
    register_tasks()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_tasks(), action = "append")
} # nocov end


.onUnload = function(libpath) { # nolint
  # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3fairness"], action = "replace")
} # nocov end
