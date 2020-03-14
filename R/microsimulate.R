#' Microsimulate
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
"microsimulate" <-
  function(x, ...){
    UseMethod("microsimulate")
  }

microsimulate.train <- function(model, newdata, target) {
  predicted_probs <- predict(model, newdata, type = "prob")

  predicted_probs
}

microsimulate.glm <- function(model, newdata, target) {
  predicted_probs <- predict(model, newdata, type = "response")

  predicted_probs
}

microsimulate.data.table <- function(model, newdata, target) {
  browser()
}


#' Simulate a choice situation
#'
#' @param prediction a data.table where the column names are choices and their values
#'  are probabilities correspoding fors the choices. Each row represent the choice
#'  probabilities of an agent.
#'
#' @return a character vector of the same length as `prediction`.
#' @export
simulate_choice <- function(prediction, target) {
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

  choices <- names(prediction)

  # random draw choices
  purrr::pmap_chr(prediction, ~ sample_choice(choices, 1, prob = (list(...))))
}
