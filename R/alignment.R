#' Alignment
#'
#' @description
#' Microsimulation alignment ensures that the simulation outcome matches its given alignment
#' target while the selection probabilities are based on each individual's likelihood.
#'
#' @param prediction a data.table with columns that contain predicted probabilities
#' @param target a named list that contains name-number value pairs that represent
#'   the number of agents to be aligned to each choice.
#'
#' @return a character vector with the same length as the nubmer of rows of
#' `prediction`. Where the index of the vector correspond to the index of the rows
#' of `prediction`,
#' @export
alignment <- function(prediction, target) {

  checkmate::assert_list(
    target,
    types = 'integerish',
    min.len = 1,
    names = 'strict',
    null.ok = FALSE
  )

  checkmate::assert_integerish(
    as.numeric(target),
    lower = 1,
    min.len = 1,
    any.missing = FALSE,
    null.ok = FALSE
  )

  checkmate::assert_data_table(prediction, any.missing = FALSE, null.ok = FALSE)

  # this is needed since `prediction` will be mutated below and we do not want to
  # accidentally change the original prediction.
  p <- data.table::copy(prediction)

  checkmate::assert_names(names(target), subset.of = names(prediction))

  if (nrow(p) < Reduce(`+`, target)) {
    stop("The sum of targets cannot exceed the number of agents that are undergoing this transition.")
  }

  # place holder column for select choices
  p[, .choice := NA_character_]

  # pick n agents based on the predicted probabilities in prediction
  for (i in seq_along(target)) {
    # ac: indices of remaining undecided agents
    # sc: indices of agents who selected choice 't'
    t <- target[i]
    ac <- p[is.na(.choice),  which = TRUE]
    sc <- sample_choice(
      x = ac,
      size = as.numeric(t),
      replace = FALSE,
      prob = p[[names(t)]][ac]
    )
    p[sc, .choice := names(t)]
  }

  p[['.choice']]
}
