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
#' Pop <- Population$new(ind_data = toy_individuals,
#'                       hh_data = toy_households,
#'                       pid_col = "pid",
#'                       hid_col = "hid")
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

#' Add log to an entity or container object
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_log <-  function(x, ...) {
  UseMethod("add_log")
}

add_log.World <- function(x, entity, desc, value, tag = NA_character_, ...) {
  e <- x$get(entity)
  e$log(desc = desc, value = value, tag = tag)
  x
}

add_log.Entity <- function(x, desc, value, tag = NA_character_, ...) {
  x$log(desc = desc, value = value, tag = tag)
  x
}

# add_log <- function(world, entity, desc, value, tag = "") {
#   assert_r6(x, class = "World")
#   checkmate::assert_choice(entity, choices = )
# }

#' Get log from Generic
#'
#' @param x a [Generic] object.
#'
#' @return a log data.table.
#' @export
#'
#' @examples
get_log <- function(x) {
  UseMethod("get_log", x)
}

#' @rdname get_log
#' @export
get_log.Generic <- function(x) {
  return(x$.__enclos_env__$private$.log)
}

#' @rdname get_log
#' @export
get_log.Container <- function(x) {
  x_log <- get_log.Generic(x)
  all_logs <- purrr::map(x$Cont, get_log.Generic)
  all_logs[[length(all_logs) + 1L]] <- x_log
  return(
    purrr::keep(all_logs, ~ !is.null(.x)) %>%
      rbindlist(.) %>%
      data.table::setorder(., time, created_timestamp)
  )
}

#' Assign new ids to data
#'
#' @description
#'
#' This function reassign ids to new entities by make sure that none of the existing
#' entities' ids in `x` are duplicated with the new entities' ids.
#'
#' @param x an `Entity` class object
#' @param ... a list of data.frame objects to
#' @param only_primary_id_col :: `logical(1)`\cr
#'  Default as `FALSE`. Only reassign ids for the primary id column of `...`.
#'
#' @note
#' The output of this function is a named list of the data.frame objects provided in `...`. Where
#' the names of the data.frames are the variable names that were given in `...`. For
#' example, if you call `data_lst <- (Ind, new_ind_data)` then the output would be a named
#' list of length 1 where the first element is named `new_ind_data`. To be safe,
#' you may want to refer to that data using the index (e.g. `data_lst[[1]]`).
#'
#' @return a named list of data.frame objects provided in `...` but with new ids assigned
#'  to them.
#'
#' @export
#'
#' @examples
#'
#' create_toy_world()
#'
#' Ind <- world$get("Individual")
#'
#' new_ind_data <- data.table::copy(toy_individuals)
#'
#' data_lst <- register(Ind, new_ind_data)
register = function(x, ..., only_primary_id_col = FALSE) {
  checkmate::assert_r6(x, classes = "Entity")

  dots_variable_names <-
    deparse(substitute(expr = c(...))) %>%
    strsplit(., ",|\\s+") %>%
    unlist() %>%
    gsub("c\\(|\\)|.*\\$", "", .) %>%
    trimws(., which = "both") %>%
    .[. != ""]

  dots <- list(...)

  .data_lst <-
    lapply(dots, function(.x) {
      checkmate::assert_data_frame(.x)
      checkmate::assert_names(names(.x), must.include = x$id_col)
      if (!is.data.table(.x)) {
        return(data.table::setDT(.x))
      } else {
        return(data.table::copy(.x))
      }
    })

  x_primary_id_col <- x$id_col[[1]]
  keys = unique(.data_lst[[1]][[x_primary_id_col]])
  values = x$generate_new_ids(n = length(keys))
  mapping_dt <- data.table(.key = keys, .value = values)

  if (only_primary_id_col) {
    cols_to_be_replaced <- x_primary_id_col
  } else {
    cols_to_be_replaced <- x$id_col
  }

  .data_lst2 <-
    lapply(.data_lst, function(.x) {
      lookup_and_replace2(x = .x, cols = cols_to_be_replaced, mapping = mapping_dt)
    })

  if (length(dots_variable_names) != length(.data_lst2)) {
    lg$warn("Do not know how to extract data names in `...`. Returning a unnamed list. \\
            Please refer to elements in the output list by their index")
  } else {
    names(.data_lst2) <- dots_variable_names
  }

  return(.data_lst2)
}
