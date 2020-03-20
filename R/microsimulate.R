#' Microsimulate a choice situation
#'
#' @description
#' This function simulates a choice situation using a model object and data to
#' predict the probability.
#'
#' @param model a [Model] object or an object in [SupportedTransitionModels()].
#' @param newdata a data.frame object to use for making prediction.
#' @param target a [Target] object or a named list.
#' @param ... dots
#'
#' @return
#' @export
#'
#' @examples
#'
#' # TODO
"microsimulate" <-
  function(model, ...){
    UseMethod("microsimulate")
  }

#' @rdname microsimulate
#' @export
microsimulate.train <- function(model, newdata, target = NULL, ...) {
  checkmate::assert_true(model$modelType == "Classification")
  probs <- predict(model, newdata, type = "prob")
  simulate_choice(probs, target)
}

#' @rdname microsimulate
#' @export
microsimulate.glm <- function(model, newdata, target = NULL, ...) {
  if (model$family$family != "binomial") {
    stop("Only `glm` objects of the binomial family can be used in `microsimulate()`.")
  }
  choices <- levels(model$model[[1]])
  probs <-
    predict(model, newdata, type = "response") %>%
    {data.table::data.table(x1 = .,
                           x2 = 1 - .)} %>%
    data.table::setnames(choices)
  simulate_choice(probs, target)
}

#' Simulate a choice situation
#'
#' @param probs a data.table where the column names are choices and their values
#'  are probabilities correspoding fors the choices. Each row represent the choice
#'  probabilities of an agent.
#' @param target a Target object or a named list or `NULL`.
#'
#' @return a character vector of the same length as `prediction`.
#' @export
#' @examples
#'
#' # TODO
simulate_choice <- function(probs, target = NULL) {
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
