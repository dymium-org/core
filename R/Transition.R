#' @title Transition class
#'
#' @description
#'
#' A class that perform Monte Carlo simulation on agents using a probabilistic model.
#' Work flow: `initialise()` -> `filter()` -> `mutate()` -> `simulate()` -> `postprocess()`.
#' Note that, to swap the run order of `filter()` and `mutate()` you need to change the
#' `mutate_first` public field to `TRUE`.
#'
#' @note
#'
#' `target` can be static or dynamic depending on the data structure of it. A static
#' target can be a named list or an integer value depending its usage in each
#' event function.
#'
#' @section Construction:
#'
#' ```
#' Trans$new(x, model, target = NULL, targeted_agents = NULL)
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
#'  * `mutate_first`:: `logical(1)`\cr
#'  Default as FALSE, this flag is used to indicate whether the attribute data from
#'  the Agent in `x` should be mutated (`$mutate(.data)`) before filtered (`$filter(.data)`).
#'  See the description section for more details about the processing steps of [Trans].
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
#' @export
# TransitionClass ---------------------------------------------------------
Trans <- R6Class(
  classname = "Trans",
  inherit = Generic,
  public = list(
    # Public ----------------------------------------------------------
    mutate_first = FALSE,

    # load data, filter, and other settings
    initialize = function(x, model, target = NULL, targeted_agents = NULL) {
      # checks
      checkmate::assert_class(x, c("Agent"))
      checkmate::assert(
        checkmate::check_subset(class(model)[[1]], choices = SupportedTransitionModels()),
        checkmate::check_r6(model, classes = "Model"),
        combine = "or"
      )
      dymiumCore::assert_target(target, null.ok = TRUE)
      checkmate::assert_integerish(targeted_agents, lower = 1, any.missing = FALSE, null.ok = TRUE)


      # store inputs
      private$.AgtObj <- x
      if (checkmate::test_r6(model, classes = "Model")) {
        private$.model <- model$model
        private$.model_preprocessing_fn <- model$preprocessing_fn
      } else {
        private$.model <- model
      }
      private$.target <- Target$new(target)$get()
      private$.targeted_agents <- targeted_agents

      # run the steps ------
      # catch the case when an empited `targeted_agents` is given.
      if (is.null(targeted_agents) | length(targeted_agents) != 0) {
        private$run_preprocessing_steps()
      } else {
        private$.sim_data <- NULL
      }

      private$run_simulation()
      private$run_postprocessing_steps()

      invisible(self)
    },

    get_decision_maker_ids = function(response_filter = NULL) {
      # TODO: how to allow decision_filter for other logical operators
      # (eg: <=, =>, !=)?
      if (is.null(response_filter)) {
        return(private$.sim_result[["id"]])
      }
      private$.sim_result %>%
        # filter
        .[response %in% response_filter, id]
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

    postprocess = function(.sim_result) {
      .sim_result
    },

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
        rs <- summary(as.factor(private$.sim_result[['response']]))
        .value <- paste0(names(rs), ": ",
                        round(rs, 2),
                        collapse = " | ")
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
    .model_preprocessing_fn = NULL,
    .AgtObj = R6::R6Class(), # use as a reference holder
    .sim_data = data.table(), # preprocessed simulation data
    .sim_result = data.table(), # two columns: id, response
    .target = integer(),
    .targeted_agents = integer(), # a vector containing agent ids of .AgtObj
    .prediction = NULL,

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

      # Model's preprocessing function
      if (!is.null(private$.model_preprocessing_fn)) {
        raw_data <- private$.model_preprocessing_fn(raw_data)
      }

      # preprocess data
      if (self$mutate_first) {
        preprocessed_data <-
          raw_data %>%
          self$mutate(.) %>%
          self$filter(.)
      } else {
        preprocessed_data <- self$filter(raw_data)
      }

      # check if after `filter` there are still data
      if (nrow(preprocessed_data) > 0) {
        if (!self$mutate_first) {
          preprocessed_data <- self$mutate(preprocessed_data)
        }
        # sanity checks
        checkmate::assert_data_frame(preprocessed_data, min.rows = 1, null.ok = FALSE)
        checkmate::assert_names(names(preprocessed_data), must.include = AgtObj$get_id_col())
        if (!is.data.table(preprocessed_data)) {
          data.table::setDT(preprocessed_data)
        }
      } else {
        # simulation won't be run if this is NULL
        preprocessed_data <- NULL
      }

      private$.sim_data <- preprocessed_data

      invisible(TRUE)
    },

    run_postprocessing_steps = function() {
      private$.sim_result <- self$postprocess(private$.sim_result)
      invisible(TRUE)
    },

    simulate = function() {

      # expect a vector
      lg$warn("Transition is not meant not be used directly! It only gives an incorrect \\
               simulation result for internal testing purposes! Please use \\
               TransitionClassification or TransitonRegression instead.")
      response <- rep(1, nrow(private$.sim_data)) # dummy

      response
    },

    run_simulation = function() {
      if (!is.null(private$.sim_data)) {
        response <- private$simulate()

        # validity checks
        if (length(response) != nrow(private$.sim_data)) {
          stop(glue::glue("The number of predictions from the model doesn't \\
                                    equal to the number of row of the data used \\
                                    to simulate it."))
        }

        # construct simulation result
        sim_result <-
          data.table::data.table(id = private$.sim_data[[private$.AgtObj$get_id_col()]],
                                 response = response)

        if (is.null(private$.target)) {
          checkmate::assert(
            checkmate::check_integerish(sim_result[['id']], unique = TRUE),
            checkmate::check_data_table(sim_result, any.missing = FALSE, null.ok = FALSE),
            checkmate::check_names(names(sim_result), identical.to = c("id", "response")),
            combine = 'and'
          )
        } else {
          checkmate::assert(
            checkmate::check_integerish(sim_result[['id']], any.missing = FALSE, unique = TRUE),
            checkmate::check_data_table(sim_result, any.missing = TRUE, null.ok = FALSE),
            checkmate::check_names(names(sim_result), identical.to = c("id", "response")),
            combine = 'and'
          )
        }
      } else {
        sim_result <- data.table(id = integer(), response = character())
      }

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

.pick_target <- function(target) {
  if (!is.data.frame(target)) {
    return(target)
  }
  if (!is.data.table(target)) {
    target <- as.data.table(target)
  }
  current_sim_time <- .get_sim_time()

  index_closest_time <- which.min(abs(target[['time']] - current_sim_time))

  return(as.list(target[index_closest_time, -c("time")]))
}

#' Get all object classes that are supported by Transition
#'
#' @description
#' Currently, these classes are supported in the `model` argument of the Transition's
#' constructor:
#' - [caret::train],
#' - [mlr::train]
#' - [stats::lm],
#' - [stats::glm],
#' - a numeric vector,
#' - a named `list`, and
#' - [data.table::data.table].
#'
#' @note See the 'Transition' section of the
#' [dymiumCore's introduction](https://core.dymium.org/articles/dymium-intro.html)
#' webpage for more detail.
#'
#' Also note that, all models that can return predicted probabilities estimated using
#' [caret::train] and [mlr::train] should work in `transition()` and `TransitionClassification`.
#' However, currently, regression models only work in `TransitionRegression`.
#'
#' @return a character vector
#' @export
SupportedTransitionModels <- function() {
  get_supported_models()
}

#' @rdname SupportedTransitionModels
#' @export
get_supported_models <- function() {
  return(c("train", "list", "data.table", "numeric", "glm", "lm", "WrappedModel"))
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

  if (!is.data.table(prediction)) {
    setDT(prediction)
  }

  choices <- names(prediction)

  if (!is.null(target)) {
    return(alignment(prediction, target))
  } else {
    # random draw choices
    return(purrr::pmap_chr(prediction, ~ sample_choice(choices, 1, prob = (list(...)))))
  }
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
  return(FALSE)
}
