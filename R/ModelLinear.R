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
    #' @param preprocessing_fn a pre-processing function that gets applied to the
    #'  data given to the `predict` method before making the prediction.
    #'
    #' @return NULL
    initialize = function(params, formula, preprocessing_fn) {
      super$initialize(params = params,
                       formula = formula,
                       type = "linear",
                       preprocessing_fn = preprocessing_fn)
      invisible(NULL)
    },

    predict = function(newdata) {
      private$.compute_linear_combination(newdata)
    }
  )
)
