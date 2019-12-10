#' R6 Class Containing a search strategy for agents.
#'
#' @description
#' This is the base class for implementing a search strategy.
#'
#' @details
#' A search strategy to be adopted by agents when they are in a market.
#'
#' @include Generic.R
#' @export
SearchStrategy <- R6Class(
  classname = "SearchStrategy",
  inherit = dymiumCore::Generic,
  public = list(

    #' @description
    #' Create a search strategy object.
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
      self$set()
    },

    set = function() {
      private$abstract()
    },

    simulate = function() {
      private$abstract()
    },

    filter = function() {
      private$abstract()
    },

    evaluate = function() {
      private$abstract()
    }
  ),

  private = list(

  )
)
