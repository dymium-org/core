#' @title Model class
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object inheriting [Generic].
#'
#' @description
#'
#' A container for the supported model objects (see [SupportedTransitionModels]).
#'
#' @section Construction:
#'
#' ```
#' x <- Model$new(x)
#' ```
#'
#' * `x` :: ([caret::train] | [data.table::data.table] | named `list`)\cr
#' A model object that compatible.
#'
#' @section Active field (read-only):
#'
#' * `model`\cr
#' The stored model object in its original form.
#'
#' @section Public Methods:
#'
#'  * `get()`\cr
#'  () -> ([caret::train] | [data.table::data.table] | named `list`)\cr
#'  Get a model object.
#'
#'  * `set(x)`\cr
#'  ([caret::train] | [data.table::data.table] | named `list`)\cr
#'  Get a model object.
#'
#'  * `modify()`\cr
#'  An abstract method.
#'
#'  * `simulate()`\cr
#'  An abstract method.
#'
#'  * `print()`\cr
#'
#' @aliases Models
#'
#' @examples
#'
#' world <- World$new()
#'
#' prob_model <- list(yes = 0.95, no = 0.05)
#'
#' world$add(x = prob_model, name = "prob_model")
#'
#' world$get(x = "prob_model")
#'
#' world$get_model(x = "prob_model")
#'
#' @export
Model <-
  R6::R6Class(
    classname = "Model",
    inherit = Generic,
    public = list(
      initialize = function(x) {
        self$set(x)
      },
      get = function() {
        private$.model
      },
      set = function(x) {
        assert_transition_supported_model(x)
        private$.model <- x
        return(self)
      },
      modify = function() {
        private$abstract()
      },
      simulate = function() {
        private$abstract()
      },
      class = function() {
        class(private$.model)
      },
      print = function() {
        print(private$.model)
      }
    ),
    active = list(
      model = function() {
        if (is.data.table(private$.model)) {
          return(data.table::copy(private$.model))
        }
        get(".model", envir = private)
      }
    ),
    private = list(
      .model = NULL
    )
  )

#' @export
#' @rdname Model
summary.Model <- function(x, ...) {
  summary(x$model)
}
