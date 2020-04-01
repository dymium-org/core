#' @title Mutate entities
#'
#' @description
#'
#' This function allows the attribute data of any [Entity] instance and its
#' inheritances stored in [World] to be mutated.
#'
#' It is useful for updating attribute variables, such as age of entities in each
#' iteration etc.
#'
#' @param world a [World] object.
#' @param entity a character denoting an entity classname.
#' @param ... a mutate expression using [data.table] syntax, e.g: age := age +1
#' @param ids a integer vector containing ids of `entity` to be mutated.
#' @param subset a subset expression, e.g: age > 30.
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
mutate_entity <- function(world, entity, ..., ids = NULL, subset) {
  checkmate::assert_r6(world, classes = "World")
  if (!checkmate::test_choice(entity, names(world$entities))) {
    stop("'", entity, "' not found in `world`.")
  }
  if ((!is.null(ids) & !missing(subset))) {
    stop("Only ids or subset should be given but never both!")
  }
  e <- world$get(entity)
  if (is.null(ids) & missing(subset)) {
    e$get_data(copy = FALSE)[, ...]
  }
  if (!is.null(ids)) {
    assert_entity_ids(e, ids, informative = T)
    e$get_data(copy = FALSE)[get(e$get_id_col()) %in% ids, ...]
  }
  if (!missing(subset)) {
    .subset <- rlang::enexpr(subset)
    e$get_data(copy = FALSE)[eval(.subset), ...]
  }
  return(world)
}

