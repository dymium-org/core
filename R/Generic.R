#' @title Generic class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Can be used as a template for other objects. This contains useful utility
#' functions for abstraction objects. See the public members (methods).
#'
#' @section Construction:
#' ```
#' Obj <- Generic$new(...)
#' ```
#'
#' @section Fields:
#'
#'  * `NULL`
#'
#' @section Methods:
#'
#'   * `debug()`\cr
#'   () -> `self & private`\cr
#'   Makes debugging the object easier by assigning self and private to the
#'   global environment of the current seesion.
#'
#'   * `message(x, ...)`\cr
#'   Print `message` onto the console with the object classname append in front of
#'   the message. To turn this off, set options(dymium.agent.verbose = FALSE).
#'
#' @export
Generic <- R6Class(
  classname = "Generic",
  public = list(

    initialize = function(...) {

    },

    debug = function() {
      pos = 1
      assign("self", self, envir = as.environment(pos))
      assign("private", private, envir = as.environment(pos))
      self$message("[self] and [private] assigned to global environment")
      # return self for piping
      return(invisible(self))
    },

    log = function(desc, value) {
      checkmate::assert_string(desc)
      checkmate::assert_numeric(value, len = 1)
      lg$info("SIM_OUTPUT", tag = self$class(), desc = desc, value = value)
    },

    is_dymium_class = function() {
      TRUE
    },

    class = function() {
      class(self)[[1]]
    }
  ),

  private = list(
    abstract = function(msg) {
      # this is a method for abstract methods
      if (!missing(msg))  {
        lg$fatal(msg)
      }
      stop("This is an abstract method which is to be implemented.")
    }
  )
)
