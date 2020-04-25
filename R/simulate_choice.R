#' Simulate a choice situation
#'
#' @description
#' This function simulates a choice situation using a model object and data to
#' predict probabilities.
#'
#' @param prediction a tidy_prediction object created by [tidy_prediction].
#' @param target a [Target] object or a named list this is for aligning the simulation
#' outcome to an external target.
#' @return a character vector
#' @export
simulate_choice <- function(prediction, target = NULL) {

  checkmate::assert_class(prediction, classes = "tidy_prediction")
  checkmate::assert_data_frame(
    prediction,
    types = 'double',
    min.cols = 2,
    any.missing = FALSE,
    null.ok = FALSE,
    col.names = 'unique'
  )
  if (!is.data.table(prediction)) {
    setDT(prediction)
  }

  if (all(names(prediction) %in% c('choices', 'probs'))) {
    stop("not implemented yet.")
  }

  choices <- names(prediction)
  # random draw choices
  if (!is.null(target)) {
    alignment(prediction, target)
  } else {
    purrr::pmap_chr(prediction, ~ sample_choice(choices, 1, prob = (list(...))))
  }
}

