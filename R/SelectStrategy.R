#' R6 Class Containing a select strategy for agents.
#'
#' @description
#' This is the base class for implementing a select strategy.
#'
#' @details
#' A select strategy to be adopted by agents when they are in a market.
#'
#' @include Generic.R
#' @export
SelectStrategy <- R6Class(
  classname = "SelectStrategy",
  inherit = dymiumCore::Generic,
  public = list(

    #' @description
    #' Create a select strategy object.
    #'
    #' @param ... dots
    #'
    #' @return `NULL`
    #' @export
    #'
    #' @examples
    #'
    #' SS <- SearchStrategy$new()
    initialize = function(...){

    },

    set = function() {

    }
  ),

  private = list(

  )
)
