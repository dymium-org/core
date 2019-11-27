
#' @title is dymium class
#' @param x any object
#'
#' @export
#' @return a logical value
#' @rdname is_dymium_class
#' @examples
#'
#' # Not Dymium object
#' library(R6)
#' x  <- R6Class()
#' is_dymium_class(x)
#'
#' # Dymium object
#' Pop <- Population$new()
#' is_dymium_class(Pop)
is_dymium_class <- function(x) {
  tryCatch(
    x$is_dymium_class(),
    error = function(e) {
      return(FALSE)
    }
  )
}

#' get_sim_time
#'
#' get the simulation time from .DMevn$sim_time. This object is readable by all
#' objects in the package but only modifiable by instances of World-class through
#' `$set_time(x)` method.
#'
#' @return a numerical value
#' @export
#' @examples
#'
#' .get_sim_time()
.get_sim_time <- function() {
  get("sim_time", envir = .DMevn)
}


#' Assign reference
#'
#' @description
#'
#' A warper function of [World]'s get() method.
#'
#' @param x a [World] object.
#' @param object_class a character or a [R6::R6Class] generator.
#'
#' @return an [R6::R6Class] object
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' # both calls are equivalent
#' Pop <- assign_reference(world, "Population")
#' Pop <- assign_reference(world, Population)
assign_reference <- function(x, object_class) {
  checkmate::assert_r6(x, classes = "Container", public = c("get"))
  if (inherits(object_class, "R6ClassGenerator")) {
    object_class <- object_class$classname
  }
  x$get(object_class)
}
