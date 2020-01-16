#' Check if ids exist in Entity.
#'
#' @param x an R6 [Entity] object
#' @param ids [integer()]\cr
#'  Ids of Entity to check.
#' @param informative [logical(1)]\cr
#'  Whether to return the missing ids in error message.
#'
#' @return a logical value.
#' @export
check_entity_ids <- function(x, ids, informative = TRUE) {
  assert_entity(x)
  checkmate::assert_integerish(ids, any.missing = FALSE, lower = 1)
  res <- checkmate::check_subset(x = ids, choices = x$get_ids(), fmatch = TRUE)
  if (!isTRUE(res) && informative) {
    missing_ids <- ids[!ids %in% x$get_ids()]
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
