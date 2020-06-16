#' make Model
#'
#' @description
#' Make a light weight model object that can be used by `transition()`, `TransitionClassification`, `TransitionRegression`.
#'
#' @param model a model object. See `SupportedTransitionModels()`.
#' @param ... dots.
#'
#' @return a `Model` object.
#' @export
makeModel <- function(model, ...) {
  UseMethod("makeModel")
}

#' @rdname makeModel
#' @export
makeModel.train <- function(model, preprocessing_fn = NULL) {

  compatible_methods = c("glm")

  if (!checkmate::test_subset(x = model$method, choices = compatible_methods, empty.ok = FALSE)) {
    stop(
      "ModelCaretTrain only works with: ", paste(compatible_methods,  collapse = ", "),
      ".", " Consider using `Model` instead if the given model is not currently supported."
    )
  }

  if (model$method == "glm") {
    return(ModelBinaryChoice$new(params = model$finalModel$coefficients,
                                 formula = model$terms,
                                 preprocessing_fn = preprocessing_fn))
  }

  # if (model$method == "multinom") {
  #
  #   stop("Not")
  #   browser()
  #
  #   params <- model[["finalModel"]][["wts"]]
  #
  #   names(params) <-
  #     lapply(model[["finalModel"]][["lab"]], function(x) {
  #       c(x, model[["finalModel"]][["coefnames"]])
  #       }) %>% unlist()
  #
  #   model.matrix(model$terms)
  #
  #   m <- model.frame(formula = model$terms, toy_individuals)
  #   Terms <- delete.response(model$terms)
  #   x <- model.matrix(Terms, m, contrasts = model$contrasts)
  #
  #   return(ModelMultinomialLogit$new(params = params,
  #                                    formula = model$terms,
  #                                    preprocessing_fn = preprocessing_fn))
  # }

  stop("something went wrong.")
}

#' @rdname makeModel
makeModel.WrappedModel <- function(model) {
  stop("Not implemented yet.")
}

#' @rdname makeModel
#' @export
makeModel.mlogit <- function(model) {
  ModelMultinomialLogit$new(params = model[['coefficients']], formula = model[['formula']])
}
