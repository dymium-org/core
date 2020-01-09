
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

#' Omit derived variables from a data.frame
#'
#' @description
#' In the attribute data of any inheritances of [Entity], variables that have a
#' `.` prefix such as `.past_partner_id` is considered as a derived variable. It
#' is normally a variable that didn't belong to the original data that used to
#' populate the object in the first place. These derived variables usually get
#' created as the simulation evolves and past outcomes of entities get recorded.
#'
#' @param .data a data.frame object.
#'
#' @return the input object with out columns that have `.` as prefix to their names.
#' @export
#'
#' @seealso normalise_derived_vars
#'
#' @examples
#'
#' x <- data.frame(a = 1, b = 2, .c = 3)
#' omit_derived_vars(x)
omit_derived_vars <- function(.data) {
  checkmate::assert_data_frame(.data, null.ok = FALSE)
  cols <- grep("^\\.", x = names(.data), value = TRUE, invert = TRUE)
  if (is.data.table(.data)) {
    return(.data[, ..cols])
  } else {
    return(.data[cols])
  }
}

#' Normalise the names of derived variables
#'
#' @description
#'
#' Remove the `.` prefix from derived variables, if any, and return the data.frame.
#'
#' @param .data a data.frame object.
#'
#' @return a data.frame object.
#' @export
#' @seealso omit_derived_vars
#'
#' @examples
#'
#' .df <- data.frame(a = rep(10, 10), b = rep(10, 10), .c = rep(10, 10), d.d = rep(10, 10))
#' normalise_derived_vars(.df)
normalise_derived_vars <- function(.data) {
  checkmate::assert_data_frame(.data, null.ok = FALSE)
  flag_derived_vars <- grepl("^\\.", x = names(.data))
  names(.data)[flag_derived_vars] <- substring(names(.data), 2)[flag_derived_vars]
  return(.data)
}
