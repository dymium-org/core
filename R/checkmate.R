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
