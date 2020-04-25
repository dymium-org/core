#' Transition for a classification model
#'
#' @description
#'
#' [TransitionClassification] performs Monte Carlo simulation on a probabilistic
#' model. In a simpler term, a psuedo random number generator is used to simulate
#' the outcome based on the probability from the model.
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
#'
#' `target` is used ensures that the aggregate outcome of the transition matches
#'  a macro-level outcome as defined in `target`. This is known as 'alignment' see,
#'  Li, J., & O'Donoghue, C. (2012). Evaluating binary alignment
#'  methods in microsimulation models. For example, in a transition where the probabilistic
#'  model predicts only two outcomes, a binary model, "yes" and "no". If the target
#'  is a list of yes = 10 and no = 20 (i.e. `r list(yes = 10, no = 20)`), this will
#'  ensure that there will be 10 decision makers whom select 'yes' and 20 decision makers
#'  that select 'no'. However, this doesn't mean that all decision makers have
#'  an equal odd of select 'yes' or 'no', the odd is still to be determined by the given
#'  probalistic model. See [alignment] for more detail.
#'
#' @section Construction:
#'
#' ```
#' TransitionClassification$new(x, model, target = NULL, targeted_agents = NULL)
#' ```
#'
#' * `x` :: [R6::R6Class]\cr
#'   An [Entity] object or its inheritances.
#'
#' * `model` :: `any object` in [SupportedTransitionModels]\cr
#'  A model object to be used to simulate transition.
#'
#' * `target` :: a named `list()`\cr
#'  (Default as NULL).
#'  `Target` or A named list where its names is a subset of to the choices in `model`
#'  to be selected and its values are the number of agents to choose those choices.
#'  See the note section for more details.
#'
#' * `targeted_agent` :: `integer()`\cr
#'  (Default as NULL)
#'  An integer vector that contains agents' ids of the [Entity] in `x` to undergo
#'  the transition. If this is given then `target` will be ignored.
#'
#' * `model_by_id` :: `logical(1)`\cr
#' This flag is to indicate whether the `model` object is meant to be matched
#' by the id column of the entity object in `x` or not. It should be noted that
#' this flag only matters if the `model` object is of type [data.table::data.table()]
#' where it must contains a numeric column called `prob` or list columns of type
#' numeric and character called `probs` and `choices`. The model object must have
#' a column which its name matches with the id column of the entity object in `x`.
#'
#' @section Fields:
#'
#'  * `model_by_id` :: (`logical(1)`)\cr
#'  See argument in the construction section.
#'
#'  * `mutate_first`:: `logical(1)`\cr
#'  Default as FALSE, this flag is used to indicate whether the attribute data from
#'  the Agent in `x` should be mutated (`$mutate(.data)`) before filtered (`$filter(.data)`).
#'
#' @section Methods:
#'
#'  * `filter(.data)`\cr
#'  ([data.table::data.table()]) -> `[data.table::data.table()]`\cr
#'  **(By default, first of the preprocessing steps)**\cr
#'  By default this method returns the input `.data`. This method can be overwrite
#'  to give the user the flexibility to 'filter' the data prior to making prediction
#'  by the given model. Filtering for eligible agents for this transition can be done in this step.
#'
#'  * `mutate(.data)`\cr
#'  ([data.table::data.table()]) -> `[data.table::data.table()]`\cr
#'  **(By default, second of the preprocessing steps)**\cr
#'  By default this method returns the input `.data`. This method can be overwrite
#'  to give the user the flexibility to 'mutate' the data prior to making prediction
#'  by the given model. Adding derived variables and historical life course of the agents
#'  can be done in this step.
#'
#' * `update_agents(attr)`\cr
#' (`character(1)`)\cr
#' Update the attribute data of the agents that undergo the transition event.
#'
#' * `get_result(ids)`\cr
#' (`integer()`) -> [data.table::data.table]\cr
#' Returns the simulation result in a [data.table::data.table] format with two
#' columns `id` and `response`.
#'
#' * `get_nrow_result()`\cr
#' Returns the number of rows in the simulation result.
#'
#' * `get_decision_maker_ids(response_filter = NULL)`\cr
#' (`character()`) -> (`integer()`)\cr
#' Returns ids of the agents that have their response equal to `response_filter`.
#'
#' * `draw(n)`\cr
#' (`interger(1)`) -> (`integer()`)\cr
#' Draw ids from the ids of eligible entities where the selection probabilty is
#' determined by the given `model` object when the object was constructed.
#'
#'
#' @seealso [TransitionRegression] and [Trans].
#' @include Transition.R
#' @export
#'
#' @examples
#'
#' # create a Individual agent object
#' Ind <- Individual$new(.data = toy_individuals, id_col = "pid")
#'
#' # create a probabilistic model
#' driver_status_rate <- data.table::data.table(
#'   sex = c('male', 'female'),
#'   probs = list(c(0.3,0.7), c(0.4,0.6)),
#'   choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
#' )
#'
#' # create a Transition for driver status
#' TransitionCandrive <- R6::R6Class(
#'   classname = "TransitionCandrive",
#'   inherit = TransitionClassification
#' )
#'
#' TransCanDrive <- TransitionCandrive$new(x = Ind, model = driver_status_rate)
#'
#' barplot(
#'   table(TransCanDrive$get_result()[['response']]),
#'   main = "Transition result: driver status",
#'   col = c('steelblue', 'salmon')
#' )
TransitionClassification <- R6Class(
  classname = "TransitionClassification",
  inherit = Trans,

# Public ------------------------------------------------------------------


  public = list(

    model_by_id = NULL,

    #' @description
    #' Create a [TransitionClassification] object.
    #'
    #' @param x an [Entity] object
    #' @param model any objects of type in [SupportedTransitionModels].
    #' @param target a named list where the names corresponds to the choices and the values
    #'  are the number of agents to choose those choices. This imposes an alignment of
    #'  the outcomes to an external constraint.
    #' @param targeted_agents a integer vector that contains ids of the entities in `x`
    #'  to undergo this
    #' @param model_by_id see in the public field section.
    #'
    #' @return an [R6::R6Class] object
    #'

    initialize = function(x, model, target = NULL, targeted_agents = NULL, model_by_id = FALSE) {
      self$model_by_id <- model_by_id
      super$initialize(x, model, target = target, targeted_agents = targeted_agents)
    },

    draw = function(n) {
      checkmate::assert_count(n, na.ok = FALSE, positive = T, null.ok = FALSE)
      if (n > self$get_nrow_result()) {
        stop('`n` is greater than the number of eligible agents for this transition.')
      }

    }
  ),
  private = list(

# Private -----------------------------------------------------------------

    simulate = function() {
      # expect a vector
      private$.prediction <- tidy_prediction(private$.model, private$.sim_data)
      simulate_choice(private$.prediction, private$.target)
    }
 )
)

