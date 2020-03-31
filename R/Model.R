#' @title Model class
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object inheriting [Generic].
#'
#' @description
#'
#' A container for the supported model objects (see [SupportedTransitionModels]).
#' When a model object is stored inside Model it can be assess using reference semantics.
#' This is particularly useful when you want to store model objects inside [World].
#' By doing so, you can assess those models from [World] as it is flowing down a
#' microsimulation pipeline just like [Entities].
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
#' @section Public fields:
#'
#' * `preprocessing_fn`\cr
#' Default as NULL, this is to store a preprocessing function which will be
#' used to evaluate the entity data in [Trans] prior to simulating the
#' transition. A situation where this is useful could be when you want to limit
#' the use of a [Model] object to the specific group of agents (e.g: age between
#' `x` and `y`) that was used to estimate the model.
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
#' simple_prob_model <- Model$new(x = list(yes = 0.95, no = 0.05))
#'
#' simple_glm_model <- Model$new(x = stats::glm(factor(sex) ~ age,
#'                               data = toy_individuals, family = "binomial"))
#'
#' # return the original model object
#' simple_prob_model$model
#'
#' # add to world
#' world <- World$new()
#'
#' world$add(simple_prob_model, name = "simple_prob_model")
#' world$add(simple_glm_model, name = "simple_glm_model")
#'
#' # to access
#' world$get("simple_prob_model")
#' world$get("simple_glm_model")
#'
#' # or alternatively you can use `get_model` which makes sure that it only looks
#' # through the named list of stored Model objects inside [World].
#' world$get_model("simple_prob_model")
#' world$get_model("simple_glm_model")
#'
#' @export
Model <-
  R6::R6Class(
    classname = "Model",
    inherit = Generic,
    public = list(
      initialize = function(x, preprocessing_fn = NULL) {
        self$preprocessing_fn <- preprocessing_fn
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
      },
      preprocessing_fn = NULL
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

#' @param object a [Model] object
#' @param ... dots
#'
#' @export
#' @rdname Model
summary.Model <- function(object, ...) {
  if (object$class() == "WrappedModel") {
    summary(object$model$learner.model)
  } else {
    summary(object$model)
  }
}
