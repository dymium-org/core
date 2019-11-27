#' @title Helper - get model objects from Container
#'
#' @param x a [Container] object with `get_model()` method.
#' @param model_names a character vector
#'
#' @return a named `list` containing [Model] objects
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' testModel <- list(a = 1)
#' world$add(testModel, "testModel")
#' dm_get_model(world, "testModel")
dm_get_model <- function(x, model_names) {
  checkmate::assert_r6(x, classes = "World", public = c("get", "Models"))
  checkmate::assert_character(model_names, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_subset(model_names, choices = names(x$Models))
  model <- list()
  for (.model_name in model_names) {
    .pos <- length(model) + 1
    model[[.pos]] <- x$get(.model_name)$get()
    names(model)[.pos] <- .model_name
  }
  model
}

#' @title Get an object from a Container.
#'
#' @param x a [Container] object.
#' @param name a `character` or a [R6::R6Class] generator.
#'
#' @return a [R6::R6Class] object.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' # both lines return the same object.
#' dm_ref(world, Individual)
#' dm_ref(world, "Individual")
dm_ref <- function(x, name) {
  checkmate::assert_r6(x, classes = "Container", public = c("get"))
  if (inherits(name, "R6ClassGenerator")) {
    name <- name$classname
  }
  x$get(name)
}
