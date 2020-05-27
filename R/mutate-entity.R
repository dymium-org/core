#' @title Mutate entities
#'
#' @description
#'
#' This function allows the attribute data of any [Entity] instance and its
#' inheritances stored in [World] to be mutated.
#'
#' It is useful for updating attribute variables, such as age of entities in each
#' iteration etc. Note that, only one or none of the filter strategies
#' {ids, subset, preprocessing_fn} should be used at a time.
#'
#' @param world a [World] object.
#' @param entity a character denoting an entity classname.
#' @param ... a mutate expression using [data.table] syntax, e.g: age := age +1
#' @param ids a integer vector containing ids of `entity` to be mutated.
#' @param subset a subset expression, e.g: age > 30.
#' @param preprocessing_fn a filter function that accepts a data.table object.
#'
#' @return a [World] object.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#'
#' # increase age of all individual agents by 1 year.
#' world %>% mutate_entity(entity = "Individual", age := age + 1L)
#'
#' # increase age of all female individual agents by 1 year.
#' world %>% mutate_entity(entity = "Individual", age := age + 1L, subset = sex == "female")
#'
#' # incrementally increase age of all individual agents by 1 year in each iteration
#'
#' for (i in 1:10) {
#'   world %>% mutate_entity(entity = "Individual", age := age + 1L, subset = sex == "female")
#' }
mutate_entity <- function(world, entity, ..., ids = NULL, subset, preprocessing_fn) {
  checkmate::assert_r6(world, classes = "World")
  if (!checkmate::test_choice(entity, names(world$entities))) {
    stop("'", entity, "' not found in `world`.")
  }
  if (sum(c(!is.null(ids), !missing(subset), !missing(preprocessing_fn))) > 1) {
    stop("Only one or none of the filter parameters {ids, subset, and propressing_fn} should be used.")
  }
  e <- world$get(entity)
  if (is.null(ids) & missing(subset) & missing(preprocessing_fn)) {
    e$get_data(copy = FALSE)[, ...]
  }
  if (!missing(preprocessing_fn)) {
    checkmate::assert_function(preprocessing_fn, nargs = 1, null.ok = FALSE)
    ids <- preprocessing_fn(e$get_data(copy = TRUE))[[e$get_id_col()]]
  }
  if (!is.null(ids)) {
    assert_entity_ids(e, ids, informative = T)
    e$get_data(copy = FALSE)[get(e$get_id_col()) %in% ids, ...]
  }
  if (!missing(subset)) {
    .subset <- rlang::enexpr(subset)
    e$get_data(copy = FALSE)[eval(.subset), ...]
  }
  invisible(world)
}

