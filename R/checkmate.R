#' Check if ids exist in Entity.
#'
#' @param x [Entity]\cr
#' an R6 [Entity] object
#' @param ids (`logical(1)`)\cr
#' Ids of Entity to check.
#' @param include_removed_data (`any`)\cr
#' Should the removed data of `x` be checked.
#' @param informative (`logical(1)`)\cr
#' Whether to return the missing ids in error message.
#'
#' @return (`logical(1)`).
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
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#' @rdname check_entity_ids
assert_entity_ids <- checkmate::makeAssertionFunction(check_entity_ids)

#' @export
#' @rdname check_entity_ids
test_entity_ids <- checkmate::makeTestFunction(check_entity_ids)

#' @export
#' @inheritParams checkmate::makeExpectation
#' @rdname check_entity_ids
expect_entity_ids <- checkmate::makeExpectationFunction(check_entity_ids)

#' Check if an argument is from the Entity class.
#'
#' @param x (`any`)\cr
#'  An object to be checked.
#'
#' @return a logical value if TRUE or an character containing an error message.
#' @export
check_entity <- function(x) {
  checkmate::check_r6(x, classes = "Entity")
}

#' @export
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#' @rdname check_entity
assert_entity <- checkmate::makeAssertionFunction(check_entity)

#' @export
#' @rdname check_entity
test_entity <- checkmate::makeTestFunction(check_entity)

#' @export
#' @inheritParams checkmate::makeExpectation
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

#' Check if an argument is an object that is supported by [Trans]
#'
#' @param x (`any`)\cr
#' an object to check.
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
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#'
#' @rdname check_transition_supported_model
assert_transition_supported_model <- checkmate::makeAssertionFunction(check_transition_supported_model)

#' @export
#' @rdname check_transition_supported_model
test_transition_supported_model <- checkmate::makeTestFunction(check_transition_supported_model)

#' @export
#' @inheritParams checkmate::makeExpectation
#' @rdname check_transition_supported_model
expect_transition_supported_model <- checkmate::makeExpectationFunction(check_transition_supported_model)

#' Check if an argument x has all the required models
#'
#' @param x a named list.
#' @param names a character vector contains names of the required models
#' @param check_supported_model Whether to check that each element in `x` are
#' supported by [Trans] using `assert_transition_supported_model()`.
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
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#' @rdname check_required_models
assert_required_models <- checkmate::makeAssertionFunction(check_required_models)

#' @export
#' @rdname check_required_models
test_required_models <- checkmate::makeTestFunction(check_required_models)

#' @export
#' @inheritParams checkmate::makeExpectation
#' @rdname check_required_models
expect_required_models <- checkmate::makeExpectationFunction(check_required_models)

#' Check if argument is a valid target object
#'
#' A target object is either a named list that contains integer values (static target) or a
#' data.frame that contains a 'time' column and other response columns (dynamic target). The type of
#' of the target depends on its usage.
#'
#' Here is an example of a static target `list(yes=10, no=20)`. Here is an example
#' of a dynamic target `data.frame(time = c(1,2,3), yes = c(10,11,12), no = c(20,21,22)`.
#'
#' @param x any object to check
#' @param null.ok default as TRUE
#'
#' @return TRUE if `x` is a valid target object else throws an error.
#'
#' @export
check_target <- function(x, null.ok = TRUE) {

  if (is.null(x)) {
    if (null.ok) {
      return(TRUE)
    } else {
      msg = "`x` cannot be NULL."
      return(msg)
    }
  }

  checkmate::assert(
    checkmate::check_list(
      x = x,
      any.missing = FALSE,
      types = c('integerish'),
      names = 'strict',
      null.ok = FALSE
    ),
    checkmate::check_data_frame(
      x = x,
      any.missing = FALSE,
      min.cols = 2,
      col.names = "strict",
      null.ok = null.ok
    ),
    checkmate::check_r6(
      x = x,
      classes = c("Target", "Generic"),
      null.ok = null.ok
    )
  )

  if (is.data.frame(x)) {
    checkmate::assert_names(
      x = names(x),
      type = "strict",
      must.include = "time"
    )

    checkmate::assert_integerish(
      x$time,
      lower = 1,
      any.missing = FALSE,
      min.len = 1,
      null.ok = FALSE,
      unique = TRUE,
      .var.name = "`time` column"
    )

  }

  return(TRUE)
}

#' @export
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#' @rdname check_target
assert_target <- checkmate::makeAssertionFunction(check_target)

#' @export
#' @rdname check_target
test_target <- checkmate::makeTestFunction(check_target)

#' @export
#' @inheritParams checkmate::makeExpectation
#' @rdname check_target
expect_target <- checkmate::makeExpectationFunction(check_target)


#' Check subset2
#'
#' The only different from `checkmate::checkSubset` is that this only print
#' mismatches in `x`.
#'
#' @inheritParams checkmate::checkSubset
#' @return (`logical(1)`).
#' @export
check_subset2 <- function(x, choices, empty.ok = TRUE, fmatch = FALSE) {
  res <- checkmate::test_subset(x = x, choices = choices, empty.ok = empty.ok, fmatch = fmatch)
  if (!isTRUE(res)) {
    x_not_in_choices <- x[!x %in% choices]
    if (length(x_not_in_choices) != 0) {
      return(glue::glue("These element in `x` don't exist in : {.x_not_in_choices}",
                        .x_not_in_choices = glue::glue_collapse(x_not_in_choices,
                                                 sep = ", ",
                                                 width = 200)))
    }
  }
  return(res)
}

#' @export
#' @param add [checkmate::AssertCollection]\cr
#'  Collection to store assertions. See [checkmate::AssertCollection].
#' @inheritParams checkmate::makeAssertion
#' @rdname check_subset2
assert_subset2 <- checkmate::makeAssertionFunction(check_subset2)

#' @export
#' @rdname check_subset2
test_subset2 <- checkmate::makeTestFunction(check_subset2)

#' @export
#' @inheritParams checkmate::makeExpectation
#' @rdname check_subset2
expect_subset2 <- checkmate::makeExpectationFunction(check_subset2)
