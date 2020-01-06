#' @title DataBackend class
#'
#' @description
#'
#' The base class of all DataBackends. [DataBackend] isn't supposed to be use directly.
#' It is meant to be implemented by the user. It is recommended that [DataBackendDataTable]
#' is to be used when adding a non-spatial data and [DataBackendSpatialFeature] for
#' spatial data.
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' ```
#' x <- DataBackend$new(.data)
#' ```
#' * `.data`\cr
#'   A data object of any types.
#'
#' @section Fields:
#'
#'  * NULL\cr
#'
#' @section Public Methods:
#'
#'  * `add(x)`\cr
#'  (**Abstract**)
#'
#'  * `get(x)`\cr
#'  (**Abstract**) Return the data.
#'
#'  * `view(x)`\cr
#'  (**Abstract**) Return the data interactively.
#'
#'  * `remove(x)`\cr
#'  (**Abstract**) Remove the data.
#'
#'  * `class(x)`\cr
#'  Return the class of the data.
#'
#'  * `n(x)`\cr
#'  (**Abstract**) Return the number of rows or entries in the data.
#'
#'  * `get_removed()`\cr
#'  Return the removed data.
#'
#' @section Private Fields:
#'
#'  * `.data`::`NULL`\cr
#'
#'  * `.removed_data`::`NULL`\cr
#'
#' @export
DataBackend <- R6::R6Class(
  classname = "DataBackend",
  inherit = Generic,
  public = list(
    initialize = function(.data) {
      private$.data <- .data
      private$.removed_data <- .data[0, ]
    },

    add = function(x) {
      private$abstract()
    },

    get = function(rows, cols, copy = FALSE) {
      private$.data
    },

    view = function(x) {
      private$abstract()
    },

    remove = function() {
      private$abstract()
    },

    class = function() {
      class(private$.data)
    },

    n = function() {
      private$abstract()
    },

    get_removed = function() {
      private$.removed_data
    },

    str = function() {
      str(private$.data)
    }
  ),

  private = list(
    .data = NULL,
    .removed_data = NULL
))
