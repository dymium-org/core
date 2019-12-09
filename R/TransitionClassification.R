#' Transition for classification model
#'
#' @description
#' TransitionClassification
#'
#' @include Transition.R
#' @export
TransitionClassification <- R6Class(
  classname = "TransitionClassification",
  inherit = Transition,

# Public ------------------------------------------------------------------


  public = list(

    #' @field model_by_id (`logical(1)`) is default as `FALSE`. This flag is to indicate
    #'  whether the `model` object is meant to be matched by the id column of the entity object
    #'  in `x` or not. It should be noted that this flag only matters if the `model` object is
    #'  of type [data.table::data.table()] where it must contains a numeric column called `prob`
    #'  or list columns of type numeric and character called `probs` and `choices`. The model
    #'  object must have a column which its name matches with the id column of the entity object in `x`.
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
        stop(
          glue::glue(
            "{class(self)[[1]]} class doesn't have an implementation of {class(private$.model)} \\
            class. Please kindly request this in dymiumCore's Github issue or send in a PR! :)"
          )
        )
      )

      if (!is.null(private$.target)) {
        # remove .sim_data that were used in target
        private$.sim_data <- private$.sim_data[-which(is.na(response))]
        # filter all NAs from response
        response <- response[!is.na(response)]
      }

      response
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

simulate_classification_datatable <- function(self, private) {
  # save some typing, this is not creating a copy of the model data.table but a reference semetic
  model <- private$.model
  sim_data <- private$.sim_data
  id_col <- private$.AgtObj$get_id_col()
  .reserved_colnames <- c('prob', 'probs', 'choices')
  matching_vars <- names(model)[!names(model) %in% .reserved_colnames]

  # checks
  checkmate::assert_data_table(model, any.missing = FALSE, min.rows = 1, col.names = 'strict', null.ok = FALSE)
  checkmate::assert_names(names(model), subset.of = c(names(private$.sim_data), .reserved_colnames))

  # two ways that data.table can be used in Transition
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
            (all(.x %between% c(0, 1))) & # probability value
            (all(sum(.x) == 1)) # sum to 1
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

    # simulate choice
    response <-
      merge(x = private$.sim_data[, .SD, .SDcols = c(id_col, matching_vars)],
            y = private$.model,
            by = matching_vars,
            sort = FALSE) %>%
      .[, .SD, .SDcols = names(.)[names(.) %in% c(id_col, .reserved_colnames)]] %>%
      # agent draws a choice from its choiceset (the lengths of the choicesets may vary)
      .[, response := purrr::map2_chr(probs, choices, ~ {sample(.y, 1, replace = FALSE, prob = .x)})] %>%
      .[['response']]

    return(response)
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


