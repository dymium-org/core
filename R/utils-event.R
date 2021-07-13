#' @title Get model objects from [World].
#'
#' @param x a [World] object.
#' @param model_names a character vector of the named [Model] objects to be retrieved.
#' @param as_r6model default as FALSE, if TRUE all `required_models` will be returned
#' as [Model] objects.
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
get_models <- function(x, model_names, as_r6model = FALSE) {
  checkmate::assert_r6(x, classes = "World", public = c("get", "models"))
  checkmate::assert_character(model_names, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_subset(model_names, choices = names(x$models))
  model <- list()
  for (.model_name in model_names) {
    .pos <- length(model) + 1
    if (as_r6model) {
      model[[.pos]] <- x$get(.model_name)
    } else {
      model[[.pos]] <- x$get(.model_name)$get()
    }
    names(model)[.pos] <- .model_name
  }
  model
}

#' Pick models
#'
#' @description
#'
#' Pick models from `model` and `world` while giving a higher priority to any model
#' object that has the same name between the two. For example, if both have a model
#' object called `model_one`, although they have totally different values, this will
#' pick the `model_one` from `model` and not the other `model_one` in `world`. The
#' function goes over both objects to find all models with the names in `required_models`.
#'
#' @param model `logical(1)`\cr
#' A named list that contains models that [Trans] supports.
#' @param world a [World] object.
#' @param required_models `character()`\cr
#' A character vector contains names of required models.
#' @param as_r6model default as FALSE, if TRUE all `required_models` will be returned
#' as [Model] objects.
#'
#' @note
#' This is used by event functions to prioritise which models, from the user argument
#' or the one added to [World], to use. An error will be raised if not all required
#' models are found.
#'
#'
#' @return a list of models.
#' @export
#'
#' @examples
#'
#' # pick_model looks for 'model_one'
#' create_toy_world()
#' my_model <- list(model_two = list(yes = 0.1, no = 0.9))
#' world$add(x = list(yes = 0.5, no = 0.5), "model_one")
#' world$add(x = list(yes = 0.5, no = 0.5), "model_two")
#' REQUIRED_MODELS <- c("model_one", "model_two")
#' final_model <- pick_models(my_model, world, REQUIRED_MODELS)
#'
#' # you can see that the final pick picked model_one from `my_model` and
#' # not the one that was added to world as it gives hihger priority to the object
#' # in `model`.
#' final_model
pick_models <- function(model, world, required_models, as_r6model = FALSE) {
  checkmate::assert_list(model,
    types = c("Model", SupportedTransitionModels()),
    any.missing = FALSE, null.ok = TRUE, names = "strict"
  )
  checkmate::assert_r6(world, classes = "World", public = c("get", "models"))
  checkmate::assert_character(required_models, unique = TRUE, null.ok = TRUE)

  if (is.null(required_models)) {
    return(NULL)
  }

  # give first priority to `model`
  models_not_found <- required_models[!required_models %in% names(model)]

  # find the rest of the models in world
  assert_required_models(world$models,
    names = models_not_found,
    check_supported_model = FALSE
  )
  models_from_world <-
    get_models(world, model_names = models_not_found, as_r6model = as_r6model)

  # return complete list of models
  append(model[names(model) %in% required_models], models_from_world)
}


#' @title Get model objects from [World].
#' @inherit get_models
#' @export
dm_get_model <- function(x, model_names) {
  get_models(x, model_names)
}

#' has the event been scheduled?
#'
#' @param time_steps a numeric vector
#'
#' @return a logical value
#' @export
is_scheduled <- function(time_steps) {
  if (!is.null(time_steps)) {
    stopifnot(all(is.numeric(time_steps)))
  }
  if (is.null(time_steps)) {
    # time_steps is not specified
    return(TRUE)
  }
  if (.get_sim_time() %in% time_steps) {
    # the current time step matches a time in time_steps
    return(TRUE)
  } else {
    # doesn't match the current time step
    return(FALSE)
  }
}
