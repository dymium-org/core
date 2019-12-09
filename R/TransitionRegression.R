#' @title Transition for regression model
#' @inherit Transition
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
  lg$trace('simulate_classification_numeric')
  checkmate::assert_class(private$.model, classes = 'train')
  checkmate::assert_true(private$.model$modelType == "Regression")
  # make prediction
  predict(object = private$.model, newdata = private$.sim_data, type = "raw")
}

