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
#'
#' @examples
#'
#' if (requireNamespace('mlogit')) {
#'
#'
#' }
ModelMultinomialLogit <- R6::R6Class(
  classname = "ModelMultinomialLogit",
  inherit = ModelCustom,
  public = list(

    #' @description
    #'
    #' Initialisation function
    #'
    #' @param params a named numeric vector.
    #' @param formula a `formula` object of class `mlogit::mFormula`, [Formula::Formula], or `formula`.
    #' @param preprocessing_fn a pre-processing function that gets applied to the
    #'  data given to the `predict` method before making the prediction.
    #'
    #' @return NULL
    initialize = function(params, formula, preprocessing_fn = NULL) {

      required_pkgs <- c("mlogit")
      # required_versions <- c("1.1.0")

      for (i in seq_along(required_pkgs)) {
        if (!requireNamespace(required_pkgs[[i]])) {
          stop("Required ", required_pkgs[[i]], " to be installed.")
        }
      }

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
      checkmate::expect_data_frame(newdata)
      data.table(chooser_id = newdata[[chooser_id_col]],
                 choice_id = newdata[[choice_id_col]],
                 linear_comb = private$.compute_linear_combination(newdata, chooser_id_col, choice_id_col)) %>%
        .[, prob := exp(linear_comb)/sum(exp(linear_comb)), by = chooser_id]
    }
  ),

  private = list(
    .compute_linear_combination = function(newdata, chooser_id_col, choice_id_col) {
      if (inherits(newdata, "dfidx")) {
        checkmate::expect_names(x = names(newdata$idx),
                                identical.to = c(chooser_id_col, choice_id_col))
      } else {
        newdata <-
          dfidx::dfidx(newdata, idx = c(chooser_id_col, choice_id_col))
      }
      mf <- model.frame(newdata, self$formula)
      # see https://github.com/dymium-org/dymiumCore/issues/84
      mm <- mlogit:::model.matrix.dfidx_mlogit(mf)
      return(as.numeric(self$params %*% t(mm)))
    }
  )
)

#' @param x  a [ModelMultinomialLogit] object
#' @param newdata a data.frame object
#' @param chooser_id_col a character that indicates the id column of the choosers in `newdata`.
#' @param choice_id_col a character that indicates the id column of the choices in `newdata`.
#'
#' @return a numeric vector
#' @export
predict.ModelMultinomialLogit = function(x, newdata, chooser_id_col, choice_id_col) {
  x$predict(newdata, chooser_id_col, choice_id_col)
}
