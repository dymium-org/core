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
#' @section S3 methods:
#'
#' * `summary(m)`\cr
#'  Ruturns summary of the stored model object.
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


      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param x a model object that is of one of {`r paste(get_supported_models(), collapse = ", ")`}
      #' @param preprocessing_fn a function.
      initialize = function(x, preprocessing_fn = NULL) {
        self$preprocessing_fn <- preprocessing_fn
        self$set(x)
      },

      #' @description
      #'
      #' Returns a copy of the stored model object. This is to prevent returning
      #' a reference of the data.table object that might be used as a model.
      #'
      #' @return a model object.
      get = function() {
        if (is.data.table(private$.model)) {
          return(data.table::copy(private$.model))
        }
        return(private$.model)
      },

      #' @description
      #'
      #' An abstract method.
      #'
      #' @return
      set = function(x) {
        stopifnot(private$.check_model(x))
        private$.model <- x
        return(self)
      },

      #' @description
      #'
      #' Returns the class of the stored model object.
      #'
      #' @return a character
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
      .model = NULL,
      .check_model = function(x) {
        checkmate::assert_subset(class(x)[[1]], choices = c("train", "data.table", "list"), empty.ok = FALSE)
        if (inherits(x, "list")) {
          checkmate::assert_names(names(x), type = "unique")
          # NOTE: this is
          # checkmate::assert_numeric(unlist(x), lower = 0, finite = TRUE, any.missing = FALSE, null.ok = FALSE)
        }
        if (inherits(x, "data.table")) {
          checkmate::assert_data_table(x, col.names = "strict", null.ok = FALSE, min.rows = 1)
        }
        return(TRUE)
      }
    )
  )


#' @export
summary.Model <- function(object, ...) {
  if (object$class() == "WrappedModel") {
    summary(object$model$learner.model)
  } else {
    summary(object$model)
  }
}
