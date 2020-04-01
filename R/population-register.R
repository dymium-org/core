#' Population register
#'
#' Assign new ids to new population data.
#'
#' @param x an R6 [Population] object.
#' @param ind_data a data.table that contains individual data
#' @param hh_data a data.table that contains household data
#'
#' @note
#'
#' It is expected that the provided data in `ind_data` and `hh_data` have the same
#' structure or columns as the individual and household data in the [Population] object
#' in `x`. Also, it is crucial that all ids in the relationship columns do exist in
#' the data of the new population data.
#'
#' @return a list contains ind_data and hh_data as data.tables.
#'
#' @examples
#'
#' create_toy_world()
#'
#' pop <- world$get("Population")
#'
#' new_population_data <- pop_register(pop, toy_individuals, toy_households)
#'
#' @export
pop_register <- function(x, ind_data, hh_data = NULL) {

  checkmate::assert_r6(x, "Population", null.ok = FALSE)
  checkmate::assert_data_table(ind_data)
  checkmate::assert_data_table(hh_data, null.ok = TRUE)

  Ind <- x$get("Individual")
  Hh <- x$get("Household")

  # create lookup tables
  pop_data_lst <- register(Ind, ind_data)
  if (!is.null(hh_data)) {
    pop_data_lst <- register(Hh, pop_data_lst$ind_data, hh_data)
  }


  return(list(ind_data = pop_data_lst$ind_data, hh_data = pop_data_lst$hh_data))
}
