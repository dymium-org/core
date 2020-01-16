#' @title Get model objects from [World].
#'
#' @param x a [World] object.
#' @param model_names a character vector of the named [Model] objects to be retrieved.
#'
#' @return a named `list` containing [Model] objects
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' testModel <- list(a = 1)
#' world$add(testModel, "testModel")
#' get_models(world, "testModel")
get_models <- function(x, model_names) {
  checkmate::assert_r6(x, classes = "World", public = c("get", "models"))
  checkmate::assert_character(model_names, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_subset(model_names, choices = names(x$models))
  model <- list()
  for (.model_name in model_names) {
    .pos <- length(model) + 1
    model[[.pos]] <- x$get(.model_name)$get()
    names(model)[.pos] <- .model_name
  }
  model
}

#' has the event been scheduled?
#'
#' @param time_steps a numeric vector
#'
#' @return a logical value
#' @export
is_scheduled <- function(time_steps) {
  if (!is.null(time_steps))
    stopifnot(all(is.numeric(time_steps)))
  if (is.null(time_steps))
    # time_steps is not specified
    return(TRUE)
  if (.get_sim_time() %in% time_steps)
    # the current time step matches a time in time_steps
    return(TRUE)
  else
    # doesn't match the current time step
    return(FALSE)
}


#' @title Get model objects from [World].
#' @inherit get_models
#' @export
dm_get_model <- function(x, model_names) {
  get_models(x, model_names)
}


