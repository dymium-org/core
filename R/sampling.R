#' @title Modified version of base::sample for programming in a Monte Carlo simulation
#'
#' @description
#' This version of the sample function removes the feature where x is an integer of length 1
#' sampling via sample takes place from 1:x. This can bring unexpected behaviour
#' as mentioned in the documentation of `base::sample()`.
#'
#' @param x a vector that contains value(s) that represents a choiceset.
#' @inheritParams base::sample
#' @export
#'
#' @examples
#'
#' sample_choice(7, 10) # equipvalent to rep(7, 10)
#' sample_choice(7, 1)
#' sample_choice(7) # which is equipvalent to the above
sample_choice <- function(x, size = 1, replace = FALSE, prob = NULL) {
  if (length(x) != 1) {
    sample(x = x, size = size, replace = replace, prob = prob)
  } else {
    rep(x, size)
  }
}
