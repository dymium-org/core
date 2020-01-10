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


#' Check for the required models in the model argument.
#'
#' @description
#' This function is meant to be used inside an Event function. It checks if the
#' model argument has all the required models.
#'
#' @param x a model object.
#' @param names names of the required models
#'
#' @return TRUE if all the required models exist else throws an error.
#' @export
check_required_models <- function(x, names) {

  sapply(x, check_supported_model)
  chknms_result <- check_names(x, names)

  if (!all(chknms_result)) {
    cli::cli_alert_danger("Not all required models are present.")
    sl <- cli::cli_ol()
    for (i in seq_along(chknms_result)) {
      if (chknms_result[[i]]) {
        cli::cli_li("{symbol$tick} {names[i]}")
      } else {
        cli::cli_li("{symbol$cross} {names[i]}")
      }
    }
    cli::cli_end(id = sl)
    checkmate::assert_names(names(x), identical.to = names)
  }

  invisible(TRUE)
}


#' Check if a model is supported by Transition
#'
#' @param x any R object.
#'
#' @return TRUE if it is supported otherwise an error will be thrown.
#' @export
#'
#' @examples
#'
#' check_supported_model(list(1))
check_supported_model <- function(x) {
  x_class <- class(x)[[1]]
  if (!x_class %in% SupportedTransitionModels()) {
    stop(glue("Object x is a {x_class} class which is not one of the supported \\
              models in Transition."))
  }
  return(TRUE)
}
