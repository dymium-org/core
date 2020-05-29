#' ModelCustom
#'
#' @description
#'
#' A ModelCustom class.
#'
#' @export
ModelCustom <- R6::R6Class(
  classname = "ModelCustom",
  inherit = Model,
  public = list(

    #' @field params named `numeric()`\cr
    #'  a named numerical vector containing parameter values of the model object.
    params = NULL,
    #' @field type `character(1)`\cr
    #'  type of the model.
    type = NULL,
    #' @field type `formula()`\cr
    #'  model formula.
    formula = NULL,
    #' @field type `character(1)`\cr
    #'  terms of the model. This gets generated using `stats::terms` on `formula`
    #'  during initialisation.
    terms = NULL,

    #' @description
    #'
    #' Constructor function.
    #'
    #' @param params
    #' @param formula
    #' @param type
    #'
    #' @return `NULL`
    initialize = function(params, formula, type = "custom") {

      self$params = checkmate::assert_numeric(params, finite = T, any.missing = FALSE, names = "unique")
      self$formula = checkmate::assert_formula(formula, null.ok = FALSE)
      self$type = checkmate::assert_string(type, na.ok = FALSE)
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
  ),

  private = list(
    .compute_linear_combination = function(newdata) {
      mm <- model.matrix(self$formula, newdata)
      as.numeric(self$params %*% t(mm))
    }
  )
)

compute_linear_combination <- function(params, formula, newdata) {
  mm <- model.matrix(formula, newdata)
  as.numeric(params %*% t(mm))
}

predict.ModelCustom <- function(object, newdata) {
  object$predict(newdata)
}

summary.ModelCustom <- function(x) {
  x$summary()
}
