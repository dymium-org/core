#' @title Transition for regression model
#'
#' @description
#' This is particularly useful for updating agents' attributes that can be modelled
#' using a regression model.
#'
#' By calling the constructor method of [TransitionClassification] this will
#' initiate the following steps:
#'
#' 1. `initialise(x, model, target, targeted_agents)` ->
#' 2. `filter(.data)`: filter agents to apply the transition to.
#' 3. `mutate(.data)`: add variables to the data of the filtered agents.
#' 4. `simulate()`: simulate the transition outcome using the probabilistic model
#' 5. `postprocess(.sim_result)`: post-processing the simulation result.
#'
#' Note that, the order of filter and mutate can be swap by overwriting the `mutate_first`
#' public field to `TRUE`. This may be useful in cases where agent selection for
#' the transition depends on one or more derived variables.
#'
#' To get the simulation result use `$get_result()`.
#'
#' @note
#' TransitionRegression only returns a raw output of the simulation result.
#' In a future update, there will be an option which allows the prediction result
#' to be drawn from a distribution to add randomness to the result.
#'
#' @format [R6::R6Class] object inheriting from [Trans]
#' @section Construction:
#'
#' ```
#' TransitionRegression$new(x, model, targeted_agents = NULL)
#' ```
#'
#' * `x` :: [R6::R6Class]\cr
#'   A Agent class inheritance object.
#'
#' * `model` :: `any object` in [SupportedTransitionModels]\cr
#'  A model object to be used to simulate transition.
#'
#' * `targeted_agent` :: [`integer()`]\cr
#'  (Default as NULL)
#'  A integer vectors that contains ids of agents in `x` to undergo the event.
#'
#' @section Fields:
#'
#'  * `mutate_first`:: `logical(1)`\cr
#'  Default as FALSE, this flag is used to indicate whether the attribute data from
#'  the Agent in `x` should be mutated (`$mutate(.data)`) before filtered (`$filter(.data)`).
#'
#' @section Methods:
#'
#' * `filter(.data)`\cr
#'  ([data.table::data.table()]) -> `[data.table::data.table()]`\cr
#'  **(By default, first of the preprocessing steps)**\cr
#'  By default this method returns the input `.data`. This method can be overwrite
#'  to give the user the flexibility to 'filter' the data prior to making prediction
#'  by the given model. Filtering for eligible agents for this transition can be done in this step.
#'
#' * `mutate(.data)`\cr
#'  ([data.table::data.table()]) -> `[data.table::data.table()]`\cr
#'  **(By default, second of the preprocessing steps)**\cr
#'  By default this method returns the input `.data`. This method can be overwrite
#'  to give the user the flexibility to 'mutate' the data prior to making prediction
#'  by the given model. Adding derived variables and historical life course of the agents
#'  can be done in this step.
#'
#' * `update_agents(attr)`\cr
#'  (`character(1)`)\cr
#'  Update the attribute data of the agents that undergo the transition event.
#'
#' * `get_result(ids)`\cr
#'  (`integer()`) -> [data.table::data.table]\cr
#'  Returns the simulation result in a [data.table::data.table] format with two
#'  columns `id` and `response`.
#'
#' * `get_nrow_result()`\cr
#'  Returns the number of rows in the simulation result.
#'
#' * `get_decision_maker_ids(response_filter = NULL)`\cr
#'  (`character()`) -> (`integer()`)\cr
#'  Returns ids of the agents that have their response equal to `response_filter`.
#'
#' @seealso [TransitionClassification] and [Trans].
#'
#' @include Transition.R
#' @export
#'
#' @example
#' # load toy data
#' create_toy_population()
#' Ind <- pop$get("Individual")
#'
#' # fit a OLS regression model
#' model_lm <- glm(age ~ sex + marital_status,
#'                 data = Ind$get_data(),
#'                 family = "gaussian")
#' summary(model_lm)
#'
#' TransAge <- TransitionRegression$new(Ind, model = model_lm)
#' # see the simulation result
#' TransAge
#'
#' # update the individual agents' 'age' field using their simulated age
#' TransAge$update_agents(attr = "age")
TransitionRegression <- R6Class(
  classname = "TransitionRegression",
  inherit = Trans,
  public = list(
    initialize = function(x, model, targeted_agents = NULL) {
      super$initialize(x, model, target = NULL, targeted_agents = targeted_agents)
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

