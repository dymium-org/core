#' @title household formation functions
#'
#' @description
#'
#' This family of functions change the state of individual data and
#' household data of the input Pop-class object.
#'
#' @param Pop [Population].
#' @param model a named list that contains a distribution of household sizes that
#' the new individuals/households are going to join. For example, `list(1 = 0.5, 2 = 0.5)`.
#' @param mapping a [data.table::data.table()] that contains two columns: `ind_id`
#' and `hh_id`.
#' @param type one of 'join', 'randomjoin' or 'new'.
#'
#' @return `NULL`
#' @export
#' @rdname household_formation
household_formation <- function(Pop, model = NULL, mapping, type) {
  checkmate::assert_data_table(mapping)
  checkmate::assert_names(names(mapping), subset.of = c("ind_id", "hh_id"))

  switch(
    type,
    "join" = {
      stopifnot(mapping[, all(is.numeric(hh_id)) == TRUE])
      hf_join(Pop, mapping)
    },
    "randomjoin" = {
      stopifnot(mapping[, all(is.na(hh_id)) == TRUE])
      hf_random_join(Pop, model, mapping)
    },
    "new" = {
      stopifnot(mapping[, all(is.na(hh_id)) == TRUE])
      hf_new(Pop, mapping)
    },
    stop(paste0(
      type, " does not match to one of {new, join, randomjoin}."
    ))
  )

  return(invisible())
}

hf_join <- function(Pop, mapping) {

  # expand mapping
  mapping_expanded <-
    element_wise_expand_lists(mapping$ind_id, mapping$hh_id) %>%
    magrittr::set_colnames(., names(mapping))

  # join household
  Pop$join_household(
    ind_ids = mapping_expanded$ind_id,
    hh_ids = mapping_expanded$hh_id
  )

}

hf_new <- function(Pop, mapping) {

  HhObj <- Pop$get("Household")
  # create new households
  # each row of `mapping` represent one household
  HhObj$add_new_agents(n = nrow(mapping))

  # assign households to groups of individuals
  mapping[, hh_id := HhObj$get_new_agent_ids()]
  mapping_expanded <-
    element_wise_expand_lists(l1 = mapping$ind_id, l2 = mapping$hh_id) %>%
    magrittr::set_colnames(., names(mapping))

  # join new households
  Pop$join_household(
    ind_ids = mapping_expanded$ind_id,
    hh_ids = mapping_expanded$hh_id
  )

}


hf_random_join <- function(Pop, model, mapping) {

  HhObj <- Pop$get("Household")

  if (!is.null(model)) {
    sampling_weights <-
      model # hhsize = 1, 2, 3, 4

    non_emptied_hhids <-
      HhObj$get_data()[hhsize != 0 & hhsize <= 4,
                           .(hid = get(HhObj$get_id_col()), hhsize)]

    stopifnot(length(sampling_weights) == non_emptied_hhids[, uniqueN(hhsize)])

    randomly_selected_hhids <-
      sample(
        x = non_emptied_hhids[, hid],
        size = nrow(mapping),
        replace = FALSE,
        prob = sampling_weights[non_emptied_hhids[, hhsize]]
      )

    # assign randomly drawn hhids to movers
    mapping[, hh_id := randomly_selected_hhids]

    # join households
    hf_join(Pop, mapping)
  } else {
    non_emptied_hhids <-
      HhObj$get_data()[hhsize != 0, .(hid = get(HhObj$get_id_col()), hhsize)]

    randomly_selected_hhids <-
      sample(x = non_emptied_hhids[, hid],
             size = nrow(mapping),
             replace = FALSE)

    # assign randomly drawn hhids to movers
    mapping[, hh_id := randomly_selected_hhids]

    # join households
    hf_join(Pop, mapping)
  }
}
