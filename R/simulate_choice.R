#' Simulate a choice situation
#'
#' @description
#' This function simulates a choice situation using a model object and data to
#' predict probabilities.
#'
#' @param model a [Model] object or an object in [SupportedTransitionModels()].
#' @param newdata a data.frame object to use for making prediction.
#' @param target a [Target] object or a named list this is for aligning the simulation
#' outcome to an external target.
#' @param ... dots
#'
#' @return a character vector
#' @export
"simulate_choice" <-
  function(model, ...){
    UseMethod("simulate_choice")
  }

#' @rdname simulate_choice
#' @export
simulate_choice.train <- function(model, newdata, target = NULL, ...) {
  checkmate::assert_true(model$modelType == "Classification")
  probs <- predict(model, newdata, type = "prob")
  simulate_choice(probs, target)
}

#' @rdname simulate_choice
#' @export
simulate_choice.glm <- function(model, newdata, target = NULL, ...) {
  if (model$family$family != "binomial") {
    stop("Only `glm` objects of the binomial family can be used in `simulate_choice()`.")
  }
  choices <- levels(model$model[[1]])
  probs <-
    predict(model, newdata, type = "response") %>%
    {data.table::data.table(x1 = .,
                           x2 = 1 - .)} %>%
    data.table::setnames(choices)
  simulate_choice(probs, target)
}

simulate_choice.Model <- function(model, newdata, target = NULL, ...) {
  if (!is.null(model$preprocessing_fn)) {
    newdata <- model$preprocessing_fn(newdata)
  }
  simulate_choice(model$get(), newdata, target = target, ...)
}

simulate_choice.WrappedModel <- function(model, newdata, target = NULL, ...) {
  if (model$learner$type != "classif") {
    stop("Only `mlr` models of type `classif` (Classification) is supported.")
  }
  if (model$learner$predict.type != "prob") {
    stop("`mlr` model object must have predict.type equal to 'prob' to simulate choice.")
  }
  if (!requireNamespace("mlr")) {
    stop("`mlr` is not installed. Please install the `mlr` package first.")
  }
  # WrappedModel's predict method doesn't accept data.table in newdata
  if (is.data.table(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  pred <- predict(model, newdata = newdata)
  if (length(model$task.desc$class.levels) == 2) {
    choices <- model$task.desc$class.levels
    prob_choice_one <- mlr::getPredictionProbabilities(pred)
    probs <- data.frame(
      choice_one = prob_choice_one,
      choice_two = 1 - prob_choice_one
    )
    colnames(probs) <- choices
  } else {
    probs <- mlr::getPredictionProbabilities(pred)
  }
  simulate_choice(probs, target)
}

#' @rdname simulate_choice
#' @export
simulate_choice.data.frame <- function(model, target = NULL, ...) {
  probs <- model
  checkmate::assert_data_frame(
    probs,
    types = 'double',
    min.cols = 2,
    any.missing = FALSE,
    null.ok = FALSE,
    col.names = 'unique'
  )
  if (!is.data.table(probs)) {
    setDT(probs)
  }
  choices <- names(probs)
  # random draw choices
  if (!is.null(target)) {
    alignment(probs, target)
  } else {
    purrr::pmap_chr(probs, ~ sample_choice(choices, 1, prob = (list(...))))
  }
}

