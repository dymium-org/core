#' @title ModelBinaryChoice
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting [ModelCustom].
#'
#' @export
ModelBinaryChoice <- R6::R6Class(
  classname = "ModelBinaryChoice",
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
    initialize = function(params, formula, preprocessing_fn = NULL) {
      super$initialize(
        params = params,
        formula = formula,
        type = "binary_choice",
        preprocessing_fn = preprocessing_fn
      )
      invisible(NULL)
    },

    #' @description
    #'
    #' This predict method returns probabilities generated from the parameters
    #' of this [Model] object.
    #'
    #' @param newdata a `data.frame` object.
    #' @param link_function :: `character(1)`\cr
    #'  default as 'logit' using `stats::binomial(link = "logit")`. Choice of
    #'  'logit' and 'probit'. TODO: implement 'probit' option.
    #'
    #' @return
    #' @export
    predict = function(newdata, link_function = c("logit")) {
      link_function <- match.arg(link_function)
      linear_comb <- private$.compute_linear_combination(newdata)
      1 / (1 + exp(-linear_comb))
    }
  )
)
