#' @title Transition class
#'
#' @description
#'
#' A class that perform Monte Carlo simulation on agents using a probabilistic model.
#' Work flow: `initialise()` -> `filter()` -> `mutate()` -> `simulate()` -> `postprocess()`.
#' Note that, the order of filter and mutate can be swap by overwriting the `preprocess()` method.
#' The default order as speficied in the `preprocess` method is:
#'
#' ```r
#' filter(.data) %>%
#'  mutate(.)
#' ```
#'
#' @section Construction:
#'
#' ```
#' Transition$new(x, model, target = NULL, targeted_agents = NULL)
#' ```
#'
#' * `x` :: [`R6`]\cr
#'   A Agent class inheritance object.
#'
#' * `model` :: `object`\cr
#'  A model
#'
#' * `target` :: [`integer()`]\cr
#'  (Default as NULL). A number that forces the number of micro events to occur. For example, if
#'  `10`` is speficied, there will be 10 agents that under go the event. However,
#'  if a integer vector is given it must be the same length as the classes in the model.
#'  This only works for classification models.
#'
#' * `targeted_agent` :: [`integer()`]\cr
#'  (Default as NULL) A integer vectors that contains ids of agents in `x` to undergo the event.
#'
#' @section Fields:
#'
#'  * `NULL`\cr
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
#' * `preprocess(.data)`\cr
#' ([data.table::data.table()]) -> `[data.table::data.table()]`\cr
#' By default, preprocess runs `filter()` then `mutate()` as described in the description section.
#' This can be overwritten to change the order and add extra steps.
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
#' @param x a Agent class inheritance object
#' @param model a model object
#' @param target a integer
#'
#' @export
# TransitionClass ---------------------------------------------------------
Transition <- R6Class(
  classname = "Transition",
  inherit = Generic,
  public = list(
    # Public ----------------------------------------------------------
    # load data, filter, and other settings
    initialize = function(x, model, target = NULL, targeted_agents = NULL) {
      # checks
      checkmate::assert_class(x, c("Agent"))
      checkmate::assert_subset(class(model)[[1]], choices = SupportedTransitionModels())
      checkmate::assert_list(target, any.missing = FALSE, types = 'integerish', names = 'strict', null.ok = TRUE)
      checkmate::assert_integerish(targeted_agents, lower = 1, any.missing = FALSE, null.ok = TRUE)

      # store inputs
      private$.AgtObj <- x
      private$.model <- model
      private$.target <- target
      private$.targeted_agents <- targeted_agents

      # run the steps
      private$run_preprocessing_steps()
      private$run_simulation()
      private$run_postprocessing_steps()

      invisible(self)
    },

    get_decision_maker_ids = function(response_filter = NULL) {
      # TODO: how to allow decision_filter for other logical operators
      # (eg: <=, =>, !=)?
      if (is.null(response_filter)) {
        return(private$.sim_result[, (id)])
      }
      private$.sim_result %>%
        # filter
        .[response %in% response_filter, (id)]
    },

    get_data = function() {
      copy(private$.sim_data)
    },

    get_result = function(ids) {
      if (missing(ids)) {
        return(copy(private$.sim_result))
      } else {
        # return response according to the order of the given `ids`
        checkmate::assert_integerish(ids, any.missing = FALSE, lower = 1, null.ok = FALSE)
        if (!checkmate::test_integerish(ids, unique = TRUE)) {
          lg$warn('`ids` are not unique! if you are certain that is what you want \\
                  you may ignore this warning message.')
        }
        checkmate::assert_subset(ids, choices = private$.sim_result[['id']], fmatch = TRUE, empty.ok = FALSE)
        return(merge(data.table(id = ids), private$.sim_result, by = 'id', sort = FALSE))
      }
    },

    get_nrow_result = function() {
      private$.sim_result[, .N]
    },

    filter = function(.data) {
      .data
    },

    mutate = function(.data) {
      .data
    },

    preprocess = function(.data) {
      self$filter(.data) %>%
        self$mutate(.)
    },

    postprocess = function(.data) {
      .data
    },

    #' @details
    #' Update the attribute data of the agents that undergo the transition event.
    #'
    #' @param attr the column name in agents' attribute data to be updated using the
    #'  response result from the transition event.
    #'
    #' @return NULL
    update_agents = function(attr) {
      private$update(attr)
    },

    print = function() {

      if (is.numeric(private$.sim_result[['response']])) {
        rs <- summary(private$.sim_result[['response']])
        .value <- paste(names(rs),
                        round(rs, 2),
                        collapse = " | ")
      }

      if (is.character(private$.sim_result[['response']])) {
        .value <-
          glue::glue_collapse(unique(private$.sim_result[['response']]),
                              sep = ", ",
                              width = 100)
      }

      msg <- glue::glue(
        "There are {private$.sim_result[, uniqueN(id)]} {class(private$.AgtObj)[[1]]} \\
        agents with {private$.sim_result[, uniqueN(response)]} unique responses \\
        of type {private$.sim_result[, typeof(response)]} \\
        {{{.value}}}
        "
      )

      message(msg)
    }
  ),

  private = list(
    # Private ----------------------------------------------------------
    .model = NULL, # model object or data.table
    .AgtObj = R6::R6Class(), # use as a reference holder
    .sim_data = data.table(), # preprocessed simulation data
    .sim_result = data.table(), # two columns: id, response
    .target = integer(),
    .targeted_agents = integer(), # a vector containing agent ids of .AgtObj

    run_preprocessing_steps = function() {

      # save some typing
      AgtObj <- private$.AgtObj

      # extract data from agent
      if (!is.null(private$.targeted_agents)) {
        raw_data <- AgtObj$get_data(ids = private$.targeted_agents)
      } else {
        raw_data <- AgtObj$get_data()
      }

      checkmate::assert_data_table(raw_data, min.rows = 1, null.ok = FALSE, .var.name = "Agent's data")

      preprocessed_data <- self$preprocess(raw_data)

      if (!is.data.table(preprocessed_data)) {
        data.table::setDT(preprocessed_data)
      }

      # sanity checks
      checkmate::assert_data_table(preprocessed_data, min.rows = 1, null.ok = FALSE)
      checkmate::assert_names(names(preprocessed_data), must.include = AgtObj$get_id_col())

      private$.sim_data <- preprocessed_data

      invisible(TRUE)
    },

    run_postprocessing_steps = function() {
      private$.sim_result <- self$postprocess(private$.sim_result)
      invisible(TRUE)
    },

    simulate = function() {

      # expect a vector
      response <- rep(1, nrow(private$.sim_data)) # dummy

      # response <- switch(
      #   EXPR = class(private$.model)[[1]],
      #   "train" = simulate_train(self, private),
      #   "data.table" = simulate_datatable(self, private),
      #   "list" = simulate_list(self, private),
      #   "NULL" = simulate_numeric(self, private),
      #   stop(
      #     glue::glue(
      #       "{class(self)[[1]]} class doesn't have an implementation of {class(private$.model)} \\
      #       class. Please kindly request this in dymiumCore's Github issue or send in a PR! :)"
      #     )
      #   )
      # )

      response
    },

    run_simulation = function() {
      response <- private$simulate()

      # validity checks
      if (length(response) != nrow(private$.sim_data)) {
        stop(glue::glue("The number of predictions from the model doesn't \\
                                    equal to the number of row of the data used \\
                                    to simulate it."))
      }
      checkmate::assert(
        checkmate::check_character(response, any.missing = FALSE, null.ok = FALSE),
        checkmate::check_numeric(response, finite = TRUE, any.missing = FALSE, null.ok = FALSE),
        combine = 'or'
      )

      # construct simulation result
      sim_result <-
        data.table::data.table(
          id = private$.sim_data[[private$.AgtObj$get_id_col()]],
          response = response
        )

      checkmate::assert(
        checkmate::check_integerish(sim_result[['id']], unique = TRUE),
        checkmate::check_data_table(sim_result, any.missing = FALSE, null.ok = FALSE),
        checkmate::check_names(names(sim_result), identical.to = c("id", "response")),
        combine = 'and'
      )

      private$.sim_result <- sim_result
      invisible(TRUE)
    },

    update = function(attr) {

      # prepare agents' data to update
      Agt <- private$.AgtObj
      .data <- Agt$get_data(copy = FALSE)
      id_col <- Agt$get_id_col()

      if (!checkmate::test_names(names(.data), must.include = attr)) {
        lg$warn("{attr} is being added to the attribute data of {class(Agt)[[1]]}
                  as it is not an original attribute of {class(Agt)[[1]]}.")
      }

      # responses
      ids <- self$get_result()[["id"]]
      responses <- self$get_result()[["response"]]

      # get index of ids
      idx_unordered <- .data[get(id_col) %in% ids, which = TRUE]
      idx_dt <- .data[idx_unordered, ..id_col][, idx := idx_unordered]
      idx <- merge(x = data.table(id = ids),
                   y = idx_dt,
                   by.x = "id", by.y = id_col, sort = FALSE) %>%
        .[["idx"]]

      # update by reference
      for (i in seq_along(idx)) {
        data.table::set(.data, i = idx[i], j = attr, value = responses[i])
      }

      invisible()
    }
 )
)


# Functions ---------------------------------------------------------------


#' Classes of supported objects to be use in Transition.
#'
#' @description
#' Currently, these classes are supported in the `model` argument of the Transition's
#' constructor: [caret::train], a named `list`, and [data.table::data.table].
#'
#' @return a character vector
#' @export
SupportedTransitionModels <- function() {
  return(c("train", "list", "data.table", "numeric", "glm", "lm"))
}

monte_carlo_sim <- function(prediction, target) {
  checkmate::assert_data_frame(
    prediction,
    types = 'double',
    min.cols = 2,
    any.missing = FALSE,
    null.ok = FALSE,
    col.names = 'unique'
  )
  choices <- names(prediction)

  if (!is.null(target)) {
    return(alignment(prediction, target))
  } else {
    # random draw choices
    return(purrr::pmap_chr(prediction, ~ sample_choice(choices, 1, prob = (list(...)))))
  }
}

#' Simulate a transition of entities
#'
#' @param entity an [Entity] object
#' @param model a model object that belongs to the classes in [SupportedTransitionModels].
#' @param target a named list that is the target for alignment.
#' @param update default as NULL. This indicates whether `entity` should be updated
#' using the outcomes from the transtion. To update an attribute of `entity` the name
#' of the attribute to be updated must be specified as a character value.
#'
#' @return a data.table with two columns: id and response.
#' @export
#'
#' @examples
#'
#' # load data
#' create_toy_population()
#' Ind <- pop$get("Individual")
#'
#' # create model
#' model_lm <- glm(age ~ sex + marital_status, data = Ind$get_data(), family = "gaussian")
#' model_glm <- glm(I(sex == "male") ~ age + marital_status, data = Ind$get_data(), family = "binomial")
#'
#' # simulation transition
#' transition(Ind, model_lm)
#' transition(Ind, model_glm)
transition <- function(entity, model, target = NULL, targeted_agents = NULL, update_attr = NULL) {
  if (is_regression(model)) {
    trans <- TransitionRegression$new(entity, model, target, targeted_agents)
  } else {
    trans <- TransitionClassification$new(entity, model, target, targeted_agents)
  }
  if (!is.null(update_attr)) {
    trans$update_agents(update_attr)
  }
  trans$get_result()
}

is_regression <- function(x) {
  if (inherits(x, "train")) {
    if (x$modelType == "Regression")
      return(TRUE)
    if (x$modelType == "Classification")
      return(FALSE)
    stop("The model is neither regression or classification.")
  }
  if (inherits(x, "lm")) {
    if(family(x)$link %in% c("identity", "log")) {
      return(TRUE)
    }
    if(family(x)$link %in% c("logit", "probit")) {
      return(FALSE)
    }
    stop("The model is neither regression or classification.")
  }
}
