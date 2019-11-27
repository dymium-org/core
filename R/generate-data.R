#' @title Randomly generate households and individuals
#'
#' @description
#' This function returns a list of randomly generated housheold and individual
#' data.tables which comprise of these columns for individuals: pid, hid, age, sex.
#' And these following columns for households: hid, hhsize. Note that, the total
#' number of individuals cannot be garantee to be the same in every run.
#'
#' @param n_households (`integer(1)`) number of households to be generated.
#' @param max_hhsize (`integer(1)`) the maximum size of households that can be generated.
#'
#' @return a list of data.tables
#' @export
#'
#' @examples
#'
#' synth_pop <- generate_population()
generate_population <- function(n_households = 10, max_hhsize = 6) {
  checkmate::assert_count(n_households, positive = T, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_count(max_hhsize, positive = T, na.ok = FALSE, null.ok = FALSE)
  # TODO: synthesise relationships: partner_id, father_id, mother_id
  synth_data <- data.table(hid = 1:n_households) %>%
    .[, hhsize := sample(1:max_hhsize, size = .N, replace = TRUE)] %>%
    .[rep(hid, hhsize)] %>%
    .[, `:=`(
      pid = 1:.N,
      sex = sample(unlist(IND$SEX), .N, replace = TRUE),
      age = rbinom(.N, size = 100, prob = runif(.N, max = 0.8)),
      marital_status = sample(unlist(IND$MARITAL_STATUS), size = .N, replace = TRUE)
    )] %>%
    # fix bad marital statuses for kids
    .[age <= 15, marital_status := IND$MARITAL_STATUS$NOT_APPLICABLE]

  list(ind_data = synth_data[, .(pid, hid, sex, age, marital_status)],
       hh_data = synth_data[, .(hid, hhsize)] %>% unique())
}
