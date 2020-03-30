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
#'
#' @seealso [TransitionRegression] and [trans].
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
    }
  ),
  private = list(

# Private -----------------------------------------------------------------

    .AgtObj = R6Class(), # use as a reference holder
    .sim_data = data.table(),
    .model = NULL, # model object or data.table
    .sim_result = data.table(), # two columns: id, response
    .target = integer(),
    .targeted_agents = integer(), # a vector containing agent ids
    .allowed_classes = c('train', 'data.table', 'list', "numeric"),

    simulate = function() {
      # expect a vector
      response <- switch(
        EXPR = class(private$.model)[[1]],
        "train" = simulate_classification_train(self, private),
        "data.table" = simulate_classification_datatable(self, private),
        "list" = simulate_classification_list(self, private),
        "numeric" = simulate_classification_numeric(self, private),
        "glm" = simulate_classification_glm(self, private),
        stop(
          glue::glue(
            "{class(self)[[1]]} class doesn't know how to deal with a {class(private$.model)} \\
            object. Please kindly request this in dymiumCore's Github issue or send in a PR! :)"
          )
        )
      )
      return(response)
    }
 )
)


# Simulate classification functions ----------------------------------------------------------
simulate_classification_train <- function(self, private) {
  lg$trace('simulate_classification_numeric')
  checkmate::assert_class(private$.model, classes = 'train')
  checkmate::assert_true(private$.model$modelType == "Classification")
  # get predicted probabilities
  prediction <- predict(object = private$.model, newdata = private$.sim_data, type = "prob")
  # random draw choices
  monte_carlo_sim(prediction, target = private$.target)
}

is_dynamic_rate_datatable_model <- function(x, .data) {
  checkmate::assert_data_table(x, min.rows = 1, col.names = 'strict')
  time_cols <- is_dynamic_rate_col(names(x))
  matching_var_cols <- names(x)[!time_cols][names(x)[!time_cols] %in% names(.data)]
  if (length(matching_var_cols) != 0 & any(time_cols)) {
    return(TRUE)
  }
  return(FALSE)
}

is_dynamic_rate_col <- function(x) {
  grepl("^t_[0-9]+$", x)
}

simulate_classification_datatable <- function(self, private) {
  # save some typing, this is not creating a copy of the model data.table but a reference semetic
  model <- private$.model
  sim_data <- private$.sim_data
  id_col <- private$.AgtObj$get_id_col()
  .reserved_colnames <- c('prob', 'probs', 'choices')
  matching_vars <- names(model)[!names(model) %in% .reserved_colnames & !is_dynamic_rate_col(names(model))]

  # checks

  # check if it is a dynamic rate model
  dynamic_rate_model_flag <- is_dynamic_rate_datatable_model(model, sim_data)

  if (!dynamic_rate_model_flag) {
    checkmate::assert_data_table(model, any.missing = FALSE, min.rows = 1, col.names = 'strict', null.ok = FALSE)
    checkmate::assert_names(names(model), subset.of = c(names(private$.sim_data), .reserved_colnames))
  }

  # two ways that data.table can be used in Trans
  # 1) as an enumerated table of a binary model
  # 2) as a classification model

  # classify which of the ways is used here

  # (1)
  if (checkmate::test_names(names(model), must.include = 'prob', disjunct.from = c('choices', 'probs'))) {

    checkmate::assert_double(model[['prob']], lower = 0, upper = 1, any.missing = FALSE, null.ok = FALSE)
    prediction <-
      merge(x = private$.sim_data[, .SD, .SDcols = c(id_col, matching_vars)],
            y = model,
            by = matching_vars,
            all.x = T
      ) %>%
      # dropping matching variables
      .[, .SD, .SDcols = names(.)[!names(.) %in% matching_vars]] %>%
      # merge to prob to the original ordering of private$.sim_data
      .[private$.sim_data[, .SD, .SDcols = id_col], on = "pid"] %>%
      # create a data.frame that contains 'no' and 'yes' columns
      .[, .(yes = prob, no = 1 - prob)]
  }

  # (2)
  if (checkmate::test_names(names(model), must.include = c('choices', 'probs'), disjunct.from = 'prob') &
      !exists('prediction')) {
    # checks
    checkmate::assert_list(model[['probs']], types = c('numeric'), any.missing = FALSE, null.ok = FALSE)
    checkmate::assert_list(model[['choices']], types = c('numeric', 'character'), any.missing = FALSE, null.ok = FALSE)
    if (anyDuplicated(private$.model[, ..matching_vars]) != 0) {
      stop(glue::glue("`model` contains duplicated rows. This will \\
                        cause some agents to have more than one choiceset \\
                        which is not allowed."))
    }

    .choices_and_probs_are_valid <-
      all(purrr::map2_lgl(
        .x = private$.model[['probs']],
        .y = private$.model[['choices']],
        .f = ~ {
          (length(.x) == length(.y)) & # same correspoding number of prob to choice
            # (all(.x %between% c(0, 1))) & # probability value
            (all(sum(.x) != 0)) # sum to 1
        }))

    if (.choices_and_probs_are_valid == FALSE) {
      stop(glue::glue("the model's probability and choice columns failed \\
                              to pass the sanity checks of \\
                              `private$simulate.data.table.choices()`. Please debug \\
                              to see."))
    }

    if (!is.null(private$.target)) {
      stop(
        glue::glue(
          "Currently, `target` cannot be done in TransitionClassification with a \\
          'choice data.table' model."
      ))
    }

    if (isTRUE(self$model_by_id)) {
      # Force to match by `id_col` only
      sim_data_matching_vars <- id_col
    } else {
      sim_data_matching_vars <- c(id_col, matching_vars)
    }

    # simulate choice
    response <-
      merge(x = private$.sim_data[, .SD, .SDcols = sim_data_matching_vars],
            y = private$.model,
            by = matching_vars,
            sort = FALSE) %>%
      .[, .SD, .SDcols = names(.)[names(.) %in% c(id_col, .reserved_colnames)]] %>%
      # agent draws a choice from its choiceset (the lengths of the choicesets may vary)
      .[, response := purrr::map2_chr(probs, choices, ~ {sample_choice(.y, 1, replace = FALSE, prob = .x)})] %>%
      .[['response']]

    return(response)
  }

  # (3) dynamic rate model
  if (dynamic_rate_model_flag) {
    current_sim_time <- .get_sim_time()
    # flag the columns that are a rate column i.e. t_2011, t_2012
    rate_col_indexes <- is_dynamic_rate_col(names(model))
    # extract just the time numeric component of the rate column names
    times <- names(model)[rate_col_indexes] %>%
      gsub("t_", "", .) %>%
      as.integer()
    # find the closest rate column to the current simulation time. i.e. if
    # there are rate columns for 10 years and the current simulation time is 11
    # the rate column of year 10 will be used.
    index_closest_time <- which.min(abs(times - current_sim_time))
    colname_with_closest_time <- grep(paste0("t_",times[index_closest_time],"$"), names(model), value = T)
    matchin_var_flags <- !rate_col_indexes
    # turn the rate colunm with the closest time to the current simulation time as `FALSE`
    rate_col_indexes[which(names(model) == colname_with_closest_time)] <- FALSE
    # filter the dynamic rate model with just the matching variable and the current time rate column
    current_rate_model <- model[, .SD, .SDcols = names(model)[!rate_col_indexes]]
    # create a prediction table
    prediction <-
      merge(x = private$.sim_data[, .SD, .SDcols = c(id_col, matching_vars)],
            y = current_rate_model,
            by = matching_vars,
            all.x = T
      ) %>%
      # dropping matching variables
      .[, .SD, .SDcols = names(.)[!names(.) %in% matching_vars]] %>%
      # merge to prob to the original ordering of private$.sim_data
      .[private$.sim_data[, .SD, .SDcols = id_col], on = "pid"] %>%
      # rename the rate column to prob
      data.table::setnames(., old = colname_with_closest_time, new = "prob") %>%
      # create a data.frame that contains 'no' and 'yes' columns
      .[, .(yes = prob, no = 1 - prob)]
  }

  # randomly draw choices
  monte_carlo_sim(prediction, target = private$.target)
}

simulate_classification_numeric <- function(self, private) {
  lg$trace("simulate_classification_numeric")

  # checks
  checkmate::assert_numeric(private$.model, lower = 0, finite = TRUE, any.missing = FALSE, null.ok = FALSE, names = 'strict')
  simulate_classification_list(self, private)
}

simulate_classification_glm <- function(self, private) {
  lg$trace("simulate_classification_binomial")

  stopifnot(!is_regression(private$.model))

  prediction <-
    data.table(yes = predict(
      private$.model,
      newdata = private$.sim_data,
      type = "response"
    ))[, no := 1 - yes]

  monte_carlo_sim(prediction, private$.target)
}

simulate_classification_list <- function(self, private) {
  lg$trace("simulate_classification_list")
  # save some typing
  model <- private$.model
  sim_data <- private$.sim_data
  id_col <- private$.AgtObj$get_id_col()

  # assuming this function is only to be calle from TransitionClassification$simulate()
  if (!is.list(model)) {
    model <- as.list(model)
  }

  # checks
  checkmate::assert_list(model, types = 'numeric', any.missing = FALSE, names = 'strict', min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(as.numeric(model), lower = 0, finite = TRUE, any.missing = FALSE, null.ok = FALSE)

  # make prediction
  prediction <- as.data.table(as.list(model)) %>%
    .[rep(1, nrow(sim_data))]

  # simulate choices
  monte_carlo_sim(prediction, private$.target)
}


