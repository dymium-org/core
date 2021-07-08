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
  simulate_choice(create_choice_table(probs), target)
}

#' @rdname simulate_choice
#' @export
simulate_choice.list <- function(model, newdata, target = NULL, ...) {
  checkmate::assert_list(
    x = model,
    types = "numeric",
    names = "strict",
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_data_frame(newdata)
  dymiumCore::sample_choice(
    x = names(model),
    size = nrow(newdata),
    prob = model,
    replace = TRUE
  )
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
  simulate_choice(create_choice_table(probs), target)
}

#' @rdname simulate_choice
#' @export
simulate_choice.Model <- function(model, newdata, target = NULL, ...) {
  if (!is.null(model$preprocessing_fn)) {
    newdata <- model$preprocessing_fn(newdata)
  }
  simulate_choice(model$get(), newdata, target = target, ...)
}

#' @rdname simulate_choice
#' @export
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
  simulate_choice(create_choice_table(probs), target)
}

#' @rdname simulate_choice
#' @export
simulate_choice.data.frame <- function(model, newdata, target = NULL, ...) {

  # convert to data.table
  if (!is.data.table(model)) {
    checkmate::assert_data_frame(model, min.rows = 1)
    model <- as.data.table(model)
  }

  if (!is.data.table(newdata)) {
    checkmate::assert_data_frame(newdata, min.rows = 1)
    newdata <- as.data.table(newdata)
  }

  if (!xor("prob" %in% names(model), "probs" %in% names(model))) {
    stop("`model` should contains a numeric probability column named `prob` in a binary",
         " choice case or `probs` in a multiple choice case.")
  }

  match_vars <-
    names(model)[!names(model) %in% c("prob", "probs", "choices")]

  checkmate::assert_names(names(newdata), must.include = match_vars)

  if ("prob" %in% names(model)) {
    probs <-
      merge(newdata, model, match_vars, sort = FALSE) %>%
      .[, .(yes = prob, no = 1 - prob)]
  }

  if ("probs" %in% names(model)) {
    if (!"choices" %in% names(model)) {
      stop("`model` is missing a list column named `choices`.")
    }
    stop("`model` with multiple choices has not been developed yet :(.")
  }

  # check cases
  checkmate::assert_data_table(
    probs,
    types = 'double',
    min.cols = 2,
    nrows = nrow(newdata),
    any.missing = FALSE,
    null.ok = FALSE,
    col.names = 'unique'
  )

  simulate_choice(create_choice_table(probs), target)
}

#' @rdname simulate_choice
#' @param choice_table a `choice_table` object, created by `create_choice_table()`.
#' @export
simulate_choice.dymium.choice_table <- function(choice_table, target = NULL, ...) {
  checkmate::assert_data_frame(
    choice_table,
    types = 'double',
    min.cols = 2,
    any.missing = FALSE,
    null.ok = FALSE,
    col.names = 'unique'
  )
  if (!is.data.table(choice_table)) {
    setDT(choice_table)
  }
  choices <- names(choice_table)
  # random draw choices
  if (!is.null(target)) {
    alignment(choice_table, target)
  } else {
    purrr::pmap_chr(choice_table, ~ sample_choice(choices, 1, prob = (list(...))))
  }
}

#' prepend dymium.choice_table
#'
#' @param x any object.
#'
#' @return `x`
create_choice_table = function(x) {
  class(x) <- c("dymium.choice_table", class(x))
  x
}
