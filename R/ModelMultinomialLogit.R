#' ModelMultinomialLogit
#'
#' @description
#' A multinomial logit model
#'
#' @details
#' This model object is use to create a multinomial model using already estimated
#' parameters.
#'
#' @export
ModelMultinomialLogit <- R6::R6Class(
  classname = "ModelMultinomialLogit",
  inherit = ModelCustom,
  public = list(

    #' @description
    #'
    #' Initialisation function
    #'
    #' @param params
    #' @param formula a `formula` or [mlogit::mFormula()] object.
    #' @param preprocessing_fn a pre-processing function that gets applied to the
    #'  data given to the `predict` method before making the prediction.
    #'
    #' @return NULL
    initialize = function(params, formula, preprocessing_fn) {
      super$initialize(params = params,
                       formula = formula,
                       type = "multinomial",
                       preprocessing_fn = preprocessing_fn)
      invisible(NULL)
    },

    #' @description
    #'
    #' This predict method returns probabilities generated from the parameters
    #' of this [Model] object.
    #'
    #' @param newdata (`data.frame()`) \cr
    #'  new data to generate probabilities conditioned on its explanatory variables.
    #' @param chooser_id_col (`character(1)`)\cr
    #'  column name of the chooser id
    #' @param choice_id_col (`character(1)`)\cr
    #'  column name of the choice id
    #'
    #' @return a `data.frame` object with three columns: chooser_id (`integer()`),
    #'  choice_id (`integer()`), linear_comb (`numeric()`), prob (`numeric()`). Note
    #'  that, 'linear_comb' stands for linear combination (i.e. $$B1 * x1 + B2 * x2$$).
    predict = function(newdata, chooser_id_col, choice_id_col) {
      data.table(chooser_id = newdata[[chooser_id_col]],
                 choice_id = newdata[[choice_id_col]],
                 linear_comb = private$.compute_linear_combination(newdata)) %>%
        .[, prob := exp(linear_comb)/sum(exp(linear_comb)), by = chooser_id]
    }
  )
)
