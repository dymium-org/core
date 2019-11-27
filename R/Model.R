#' @title Model class
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object inheriting [Generic].
#'
#' @description
#'
#' Provides an interface for model objects to be use in [Transition] and [World].
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
#' @section Public Methods:
#'
#'  * `get()`\cr
#'  () -> ([caret::train] | [data.table::data.table] | named `list`)\cr
#'  Get a model object.
#'
#'  * `set()`\cr
#'  () -> ([caret::train] | [data.table::data.table] | named `list`)\cr
#'  Get a model object.
#'
#'  * `modify()`\cr
#'
#'  * `simulate()`\cr
#'
#'  * `print()`\cr
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
        stopifnot(private$.check_model(x))
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
    private = list(
      .model = NULL,
      .check_model = function(x) {
        checkmate::assert_subset(class(x)[[1]], choices = c("train", "data.table", "list"), empty.ok = FALSE)
        if (inherits(x, "list")) {
          checkmate::assert_names(names(x), type = "unique")
          checkmate::assert_numeric(unlist(x), lower = 0, finite = TRUE, any.missing = FALSE, null.ok = FALSE)
        }
        if (inherits(x, "data.table")) {
          checkmate::assert_data_table(x, col.names = "strict", null.ok = FALSE, min.rows = 1)
        }
        return(TRUE)
      }
    )
  )
