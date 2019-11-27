#' Population register
#'
#' Assign new ids to the new population using the ids reserved from the input
#' population object.
#'
#' @param x An Dymium's Agent object that contains Population object
#' @param ind_data a data.table contains individual data
#' @param hh_data a data.table contains household data
#'
#' @return a list contains ind_data and hh_data
#' @export
pop_register <- function(x, ind_data, hh_data) {

  checkmate::assert_r6(x, "Population", null.ok = FALSE)

  PopObj <- x

  assert_that(is.data.table(ind_data))
  # 1) register new ids
  new_ind_ids <-
    PopObj$get('Individual')$generate_new_ids(
      n = ind_data[, uniqueN(get(PopObj$get('Individual')$get_id_col()))]
    )

  # 2) craete lookup tables
  ind_lt <-
    data.frame(
      key = ind_data[, get(PopObj$get('Individual')$get_id_col())],
      value = new_ind_ids
    )

  # 3) assign ids
  ind_data <-
    lookup_and_replace(
      data = ind_data,
      lookup_table = ind_lt,
      cols = c(PopObj$get('Individual')$get_id_col(), IND$ID_COLS),
      id_col = PopObj$get('Individual')$get_id_col()
    )

  # only replace hid col in ind_data when hh_data is given
  # this is to allow the case when one want to register and
  # replace individual ids only. Such as immigrants joining
  # existing households, where immigrants' household id column
  # will be assigned with their new households.
  if (!missing(hh_data)) {
    assert_that(is.data.table(hh_data))

    # 1) register new ids
    new_hh_ids <-
      PopObj$get('Household')$generate_new_ids(
        n = hh_data[, uniqueN(get(PopObj$get('Household')$get_id_col()))]
      )

    # 2) craete lookup tables
    hh_lt <-
      data.frame(
        key = hh_data[, get(PopObj$get('Household')$get_id_col())],
        value = new_hh_ids
      )

    # 3) assign ids
    ind_data <-
      lookup_and_replace(
        data = ind_data,
        lookup_table = hh_lt,
        cols = PopObj$get('Individual')$get_hid_col()
      )

    hh_data <-
      lookup_and_replace(
        data = hh_data,
        lookup_table = hh_lt,
        cols = PopObj$get('Household')$get_id_col()
      )
  } else {
    hh_data <- NULL
  }

  return(list(ind_data = ind_data, hh_data = hh_data))
}
