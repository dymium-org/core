#' Check if ids exist in Entity.
#'
#' @param x an R6 [Entity] object
#' @param ids [\code{integer()}]\cr
#'  Ids of Entity to check.
#' @param include_removed_data [\code{logical(1)}]\cr
#'  Should the removed data of `x` be checked.
#' @param informative [\code{logical(1)}]\cr
#'  Whether to return the missing ids in error message.
#'
#' @return a logical value.
#' @export
check_entity_ids <- function(x, ids, include_removed_data = FALSE, informative = TRUE) {
  assert_entity(x)
  checkmate::assert_integerish(ids, any.missing = FALSE, lower = 1)
  all_ids <- x$get_ids()
  if (include_removed_data) {
    all_ids <- c(all_ids, x$database$attrs$get_removed()[[x$get_id_col()]])
  }
  res <- checkmate::check_subset(x = ids, choices = all_ids, fmatch = TRUE)
  if (!isTRUE(res) && informative) {
    missing_ids <- ids[!ids %in% all_ids]
    if (length(missing_ids) != 0) {
      return(glue::glue("These ids don't exist in {class(x)[[1]]}: {.ids}",
                        .ids = glue::glue_collapse(missing_ids, sep = ", ", width = 200, last = " and ")))
    }
  }
  return(res)
}

#' @export
#' @rdname check_entity_ids
test_entity_ids <- checkmate::makeTestFunction(check_entity_ids)

#' @export
#' @rdname check_entity_ids
expect_entity_ids <- checkmate::makeExpectationFunction(check_entity_ids)

#' @export
#' @rdname check_entity_ids
assert_entity_ids <- checkmate::makeAssertionFunction(check_entity_ids)

#' Check if an argument is from the Entity class.
#'
#' @param x an object to be checked.
#'
#' @return a logical value if TRUE or an character containing an error message.
#' @export
check_entity <- function(x) {
  checkmate::check_r6(x, classes = "Entity")
}

#' @export
#' @rdname check_entity
assert_entity <- checkmate::makeAssertionFunction(check_entity)

#' @export
#' @rdname check_entity
test_entity <- checkmate::makeTestFunction(check_entity)

#' @export
#' @rdname check_entity
expect_entity <- checkmate::makeExpectationFunction(check_entity)

check_subset2 <- function(x, choices, informative = TRUE) {
  res <- checkmate::check_set_equal(class(x), class(choices))
  if (!isTRUE(res))
    return(res)
  res <- checkmate::check_subset(x = x, choices = choices, fmatch = TRUE)
  if (!isTRUE(res) && informative) {
    missing_x <- x[!x %in% choices]
    if (length(missing_x) != 0) {
      return(glue::glue("These values don't exist}: {.missing_x}",
                        .missing_x = glue::glue_collapse(missing_x, sep = ", ", width = 200, last = " and ")))
    }
  }
  return(res)
}

#' Check if an argument is an object that is supported by [Transition]
#'
#' @param x an object.
#'
#' @return a logical value if TRUE, if false then returns an error message
#' @export
check_transition_supported_model <- function(x) {
  res <- checkmate::check_subset(class(x)[[1]], choices = SupportedTransitionModels())
  if (!isTRUE(res)) {
    msg <- glue::glue("'x' is class '{class(x)[[1]]}' which is not one of the \\
                      supported models in Transition")
    return(msg)
  }
  return(TRUE)
}

#' @export
#' @rdname check_transition_supported_model
assert_transition_supported_model <- checkmate::makeAssertionFunction(check_transition_supported_model)

#' @export
#' @rdname check_transition_supported_model
test_transition_supported_model <- checkmate::makeTestFunction(check_transition_supported_model)

#' @export
#' @rdname check_transition_supported_model
expect_transition_supported_model <- checkmate::makeExpectationFunction(check_transition_supported_model)

#' Check if an argument x has all the required models
#'
#' @param x a named list.
#' @param names a character vector contains names of the required models
#' @param check_supported_model Whether to check that each element in `x` are
#' supported by [Transition] using `assert_transition_supported_model()`.
#'
#' @return TRUE if all the required models exist else throws an error.
#' @export
check_required_models <- function(x, names, check_supported_model = TRUE) {
  # check if it is supported
  if (check_supported_model) {
    purrr::walk(x, assert_transition_supported_model)
  }
  # check for names
  chknms_result <- names %in% names(x)
  if (!all(chknms_result)) {
    cli::cli_alert_danger("Not all required models are present.")
    sl <- cli::cli_ol()
    for (i in seq_along(chknms_result)) {
      if (chknms_result[[i]]) {
        cli::cli_li("{symbol$tick} {names[i]}")
      } else {
        cli::cli_li("{symbol$cross} {names[i]}")
      }
    }
    cli::cli_end(id = sl)
    msg <-
      glue::glue("These required models are not present: \\
                 {paste(names[!chknms_result], collapse = ', ')}")
    return(msg)
  }
  return(TRUE)
}

#' @export
#' @rdname check_required_models
assert_required_models <- checkmate::makeAssertionFunction(check_required_models)

#' @export
#' @rdname check_required_models
test_required_models <- checkmate::makeTestFunction(check_required_models)

#' @export
#' @rdname check_required_models
expect_required_models <- checkmate::makeExpectationFunction(check_required_models)
