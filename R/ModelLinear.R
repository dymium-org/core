#' @title ModelLinear
#'
#' @export
ModelLinear <- R6::R6Class(
  classname = "ModelLinear",
  inherit = ModelCustom,
  public = list(

    #' @description
    #'
    #' Initialisation function
    #'
    #' @param params a `data.frame` object.
    #' @param formula a `formula` object.
    #'
    #' @return NULL
    initialize = function(params, formula) {
      super$initialize(params, formula, type = "linear")
      invisible(NULL)
    },

    predict = function(newdata) {
      private$.compute_linear_combination(newdata)
    }
  )
)
