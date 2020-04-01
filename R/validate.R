#' Validate the linkages between Entities in World
#'
#' @param world a [World] object.
#'
#' @return a logical value
#' @export
#'
#' @examples
#'
#' create_toy_world()
#'
#' validate_linkages(world)
validate_linkages <- function(world) {
  for (E in world$entities) {
    E_id_col <- E$get_id_col()
    for (e in world$entities) {
      if (E$class() == e$class()) {
        next()
      }
      if (E_id_col %in% names(e$get_data())) {
        if (!checkmate::test_subset(x = e$get_attr(x = E_id_col), choices = E$get_attr(x = E_id_col))) {
          stop(glue::glue("Not all entries in {E$class()} exists in {e$class()}"))
        }
      }
    }
  }
  return(TRUE)
}
