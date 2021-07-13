#' @title ContainerGeneric
#' @description
#' ContainerGeneric is the base class for the [Container] classes. [ContainerGeneric]
#' isn't supposed to be use directly.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Generic]
#' @include ContainerGeneric.R
#'
#' @section Construction:
#'
#' ```
#' x <- ContainerGeneric$new()
#' ```
#'
#' * NULL\cr
#'
#' @section Public Fields:
#'
#' * `Cont`\cr
#'  A named list that contains objects of any type.
#'
#' @section Public Methods:
#'
#' * `add(x)`\cr
#'  (any objects)\cr
#'  Add `x` to self$Cont.
#'
#' * `check_pos(x)`\cr
#' (`integer(1)`) -> (`logical(1)`)\cr
#'  Check if the position in `x` exists in `self$Cont`.
#'
#' * `get(x)`\cr
#'  (`integer(1)`) -> (an object)\cr
#'  Get a object in `x` element of `self$Cont`.
#'
#' * `n()`\cr
#'  () -> (`integer(1)`)\cr
#'  Returns the length of `self$Cont`.
#'
#' * `print()`\cr
#'
#' * `remove(x)`\cr
#'  (`integer(1)`)\cr
#'  Remove the object at the `x` element of `self$Cont`.
#'
#' * `unpack(target)`\cr
#'  ([Container])\cr
#'  Add all the objects in `self$Cont` to another `target` Container.
#'
#' * `summary()`\cr
#'  Abstract method.
#'
#' @export
ContainerGeneric <- R6Class(
  classname = "ContainerGeneric",
  inherit = Generic,
  public = list(

    Cont = list(),

    initialize = function() {

    },

    print = function() {
      cat("There are", self$n(), "items in self$Cont.\n")
    },

    summary = function() {
      private$abstract()
    },

    add = function(x) {
      self$Cont[[length(self$Cont) + 1L]] <- x
    },

    get = function(pos) {
      self$check_pos(pos)
      self$Cont[[pos]]
    },

    unpack = function(target) {
      checkmate::assert_r6(target, classes = c("Container", "Generic"), public = "unpack")
      for (Obj in self$Cont) {
        lg$info("Unpacking {self$class()} to {target$class()}")
        target$add(Obj)
      }
    },

    n = function() {
      length(self$Cont)
    },

    remove = function(pos) {
      self$check_pos(pos)
      self$Cont[[pos]] <- NULL
      self$Cont <- self$Cont[!is.null(self$Cont)]
      invisible(self)
    },

    check_pos = function(pos) {
      checkmate::assert_number(pos, na.ok = FALSE, finite = TRUE, null.ok = FALSE)

      if (pos > self$n()) {
        stop("pos is greater than the number of available objecst in the container {", self$n(), "}.")
      }

      if (pos <= 0) {
        stop("pos can not be less than or equal to 0.")
      }
    }
  )
)
