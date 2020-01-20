#' @title Transition for regression model
#'
#' @description
#'
#' Note that, TransitionRegression only returns a raw output of the simulation result.
#'
#'
#' @format [R6::R6Class] object inheriting from [Transition]
#' @section Construction:
#'
#' ```
#' Transition$new(x, model, target = NULL, targeted_agents = NULL)
#' ```
#' See [Transition], for a description of the arguments.
#'
#' @section Fields
#'
#' See [Transition].
#'
#' @section Methods
#'
#' See [Transition].
#'
#' @seealso [TransitionClassification]
#'
#' @include Transition.R
#' @export
TransitionRegression <- R6Class(
  classname = "TransitionRegression",
  inherit = Transition,
  public = list(
    initialize = function(x, model, target = NULL, targeted_agents = NULL) {
      super$initialize(x, model, target = target, targeted_agents = targeted_agents)
    }
  ),

  private = list(
    .AgtObj = R6Class(), # use as a reference holder
    .sim_data = data.table(),
    .model = NULL, # model object or data.table
    .sim_result = data.table(), # two columns: id, response
    .target = integer(),
    .targeted_agents = integer(), # a vector containing agent ids

    simulate = function() {

      # response is a vector of simulated choices that maybe of type double,
      # integer, logical or character.
      response <- switch(
        EXPR = class(private$.model)[[1]],
        "train" = simulate_regression_train(self, private),
        "glm" = simulate_regression_glm(self, private),
        "lm" = simulate_regression_glm(self, private),
        stop(
          glue::glue(
            "Transition class doesn't have an implementation of {class(private$.model)} \\
            class. Please kindly request this in dymiumCore's Github issue or send in a PR! :)"
          )
        )
      )

      response
    }
 )
)

simulate_regression_train <- function(self, private) {
  lg$trace('simulate_regression_train')
  checkmate::assert_class(private$.model, classes = 'train')
  checkmate::assert_true(private$.model$modelType == "Regression")
  # make prediction
  predict(object = private$.model, newdata = private$.sim_data, type = "raw")
}

simulate_regression_glm <- function(self, private) {
  lg$trace('simulate_regression_glm')
  # make prediction
  predict(object = private$.model, newdata = private$.sim_data, type = "response")
}

