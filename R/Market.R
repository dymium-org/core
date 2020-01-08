#' Market class
#'
#' @description
#' A market class.
#'
#' @include Generic.R
#' @export
Market <- R6Class(
  classname = "Market",
  inherit = Generic,
  public = list(
    #' @description
    #' Create a new market object.
    #'
    #' @param ... dots
    #'
    #' @return `NULL`
    #'
    #' @examples
    #'
    #' Mrkt <- Market$new()
    initialize = function(...) {

    },


    #' @description
    #' Match agents with choices
    #'
    #' @param agents a vector containing agent ids
    #' @param choices a vector containing choice ids
    #' @param probabilities a vector containing numerical probability values
    #'
    #' @return a [data.table::data.table] with two columns: `agent_id` and `choice_id`.
    match = function(agents, choices, probabilities) {

    }
  ),

  private = list()
)
