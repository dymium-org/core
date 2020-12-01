#' @title ModelCustom
#'
#' @description
#'
#' A ModelCustom class.
#'
#' @usage NULL
#' @format an [R6::R6Class] class inheriting from [Model].
#'
#' @section Public fields:
#'
#' * `params` :: named `numeric()`\cr
#'  A named numeric vector containing parameter values of the model object.
#'
#' * `type` :: `character(1)`\cr
#'  Type of the model.
#'
#' * `formula` :: `formula()`\cr
#'  Model formula.
#'
#' * `terms` :: `character(1)`\cr
#'  terms of the model. This gets generated using `stats::terms` on `formula`
#  during initialisation.
#'
#' @export
ModelCustom <- R6::R6Class(
  classname = "ModelCustom",
  inherit = Model,
  public = list(

    # @field params named `numeric()`\cr
    #  a named numerical vector containing parameter values of the model object.
    params = NULL,
    # @field type `character(1)`\cr
    #  type of the model.
    type = NULL,
    # @field formula `formula()`\cr
    #  model formula.
    formula = NULL,
    # @field terms `character(1)`\cr
    #  terms of the model. This gets generated using `stats::terms` on `formula`
    #  during initialisation.
    terms = NULL,

    # @description
    #
    # Constructor function.
    #
    # @param params a named `numeric()`.
    # @param formula a model `formula()`.
    # @param type type of the model.
    # @param preprocessing_fn a pre-processing function that gets applied to the
    #  data given to the `predict` method before making the prediction.
    #
    # @return `NULL`
    initialize = function(params, formula, type = "custom", preprocessing_fn) {

      self$params = checkmate::assert_numeric(params,
                                              finite = T,
                                              any.missing = FALSE,
                                              names = "unique")
      self$formula = checkmate::assert_formula(formula, null.ok = FALSE)
      self$type = checkmate::assert_string(type, na.ok = FALSE)
      self$preprocessing_fn = checkmate::assert_function(preprocessing_fn,
                                                         nargs = 1,
                                                         null.ok = TRUE)
      self$terms = terms(formula)
      private$.model = self

      invisible(NULL)
    },

    #' @description
    #'
    #' print method.
    print = function() {
      cat('Model type: ', self$type, "\n")
      print(self$params)
    },

    #' @description
    #'
    #' an abstract method. Once implemented it should accept `newdata` as the first
    #' argument and returns a `numeric()` vector or a `data.frame()` that contains
    #' the predicted probabilities calculated using `self$params` and `newdata`.
    predict = function() {
      private$.abstract()
    },

    #' @description
    #'
    #' summary method.
    summary = function() {
      self$print()
    }
  )
)

#' Compute linear combination
#'
#' @param formula (`formula(1)`|[Formula::Formula()])\cr
#'  A formula which
#' @param params (named `numeric()`)\cr
#'  Parameters of the explanatory variables specified in `formala`.
#' @param newdata (`data.frame()`)\cr
#'  A dataset to be evaluated.
#'
#' @return (`data.frame()`).
#' @export
compute_linear_combination <- function(formula, ...) {
  UseMethod("compute_linear_combination")
}

#' @rdname compute_linear_combination
#' @export
compute_linear_combination.formula <- function(formula, params, newdata) {
  browser()
  mm <- model.matrix(formula, newdata)
  return(as.numeric(params %*% t(mm)))
}

#' @rdname compute_linear_combination
#' @export
compute_linear_combination.Formula <- function(formula, params, newdata) {
  # Usually formula is the first argument of `model.frame` but mlogit has this
  # weird order that newdata must be the first arg.
  # see https://github.com/asiripanich/mlogit/blob/a111b401211b647cd458316dcbf5d6adab102935/R/mlogit.R#L346-L353
  mf <- model.frame(newdata, formula)
  # see https://github.com/dymium-org/dymiumCore/issues/84
  mm <- mlogit:::model.matrix.dfidx_mlogit(mf)
  return(as.numeric(params %*% t(mm)))
}

#' @param object a [ModelCustom] object
#'
#' @param newdata a data.frame/data.table
#' @param ... not being used.
#'
#' @rdname ModelCustom
#' @return prediction
#' @export
predict.ModelCustom <- function(object, newdata, ...) {
  return(object$predict(newdata))
}

#' @param object a [ModelCustom] object
#' @param ... not being used.
#'
#' @rdname ModelCustom
#' @return summary
#' @export
summary.ModelCustom <- function(object, ...) {
  return(object$summary())
}
