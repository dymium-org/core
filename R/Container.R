#' @title Container
#' @description
#' Container is the base class for [World] and [Population]. It has the basic
#' functionalities to store and access references of other [R6::R6Class] objects.
#'
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [dymiumCore::ContainerGeneric]
#' @include ContainerGeneric.R
#'
#' @section Construction:
#'
#' ```
#' x <- Container$new()
#' ```
#'
#' * NULL\cr
#'
#' @section Public Fields:
#'
#' * `Cont`\cr
#'  A named list that contains references of other [R6::R6Class] objects.
#'
#' @section Public Methods:
#'
#' * `add(x, name)`\cr
#'  ([R6::R6Class]|Supported Transition Models)\cr
#'  Add an [R6::R6Class] object or a object of the classes in [dymiumCore::SupportedTransitionModels] to self$Cont.
#'  Name must be given when `x` is an object of the classes in [dymiumCore::SupportedTransitionModels].
#'
#' * `check_pos(x)`\cr
#' (`integer(1)`) -> (`logical(1)`)\cr
#'  Check if the position in `x` exists in `self$Cont`.
#'
#' * `get(x)`\cr
#'  (`character(1)` | `integer(1)` | an [R6::R6Class] generator) -> (an [R6::R6Class] object)\cr
#'  Get a reference to `x` which can be a [R6::R6Class] generator or string or integer.
#'
#' * `n()`\cr
#'  () -> (`integer(1)`)\cr
#'  Returns the length of the references in `self$Cont`.
#'
#' * `names()`\cr
#'  () -> (`character()`)\cr
#'  Returns the names of the references in `self$Cont`.
#'
#' * `print()`\cr
#'
#' * `remove(x)`\cr
#'  (`character(1)` | `integer(1)` | a [R6::R6Class] generator)\cr
#'  Remove the reference of x from self$Cont by the class of the given [R6::R6Class]
#'  generator or string or integer.
#'
#' * `unpack(target)`\cr
#'  ([dymiumCore::Container])\cr
#'  Add all the references inside self$Cont to the `target` container. This is
#'  useful when you have containers within a container so that all references can
#'  be access via the top container. eg: A World container that contains a Population
#'  container can directly access Individual and Household objects.
#'
#' * `summary()`\cr
#'  Abstract method.
#'
#' @export
Container <- R6Class(
  classname = "Container",
  inherit = dymiumCore::ContainerGeneric,
  public = list(

    add = function(x, name) {
      if (!missing(name)) {
        checkmate::assert_names(name, disjunct.from = self$names())
      }
      .pos <- self$n() + 1L
      super$add(x)
      names(self$Cont)[.pos] <- name
      invisible()
    },

    get = function(x) {
      if (inherits(x, "R6ClassGenerator")) {
        x <- x$classname
      }
      if (checkmate::test_integerish(x)) {
        self$check_pos(x)
      } else {
        checkmate::assert_choice(x, choices = self$names())
      }
      self$Cont[[x]]
    },

    names = function() {
      names(self$Cont)
      # .names <- names(self$Cont)
      # .names[!is.na(.names)]
    },

    remove = function(x) {
      if (checkmate::test_integerish(x)) {
        self$check_pos(x)
      } else {
        checkmate::assert_choice(x, choices = self$names())
      }
      self$Cont[[x]] <- NULL
      self$Cont <- self$Cont[!is.null(self$Cont)]
      invisible()
    }
  )
)
