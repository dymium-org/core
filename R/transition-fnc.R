#' Simulate state transition (Markov chain)
#'
#' @description
#' The `transition` function can be used to evaluate a model againts the attribute
#' data of an [Entity] object stored inside the input [World] object and update
#' an attribute of that [Entity] using random draws from the prediction result.
#' It allows simple state transitions to be simulated directly inside a
#' microsimulation pipeline, as it always returns the input `World` object.
#'
#' @param world a [World] object
#' @param entity a character indicating the entity class to apply the transition to.
#' @param model a [Model] object or an object in [SupportedTransitionModels()].
#' @param target a [Target] object or a named list or `NULL`.
#' @param targeted_ids a integer vector containing ids of entities in `entity`
#'  to undergo the transition or `NULL`.
#' @param preprocessing_fn a function that accepts one argument or `NULL`. This allows preprocessing of
#' `entity`'s attribute data, for example, if only records with certain conditions
#' should undergo the transition or a new variable should be added to the data. See
#' the Note section below for how to create a preprocessing function.
#' @param attr a character denoting which of the attribute if `entity` should be
#'  updated as the result of the transition or `NULL`.
#' @param verbose `logical()`\cr
#'  Default as `FALSE`.
#' @param values named `vector()`\cr
#'  A named vector that is used to replace the outcomes of the model in the field
#'  specified in `attr`. See the example section.
#'
#' @note
#'
#' In general, dymiumCore detects variables in entity data of an [Entity] object that
#' has a dot prefix as derived variables. Meaning, those derived variables are not
#' check against new entity data that are getting added to the [Entity] object. But
#' when entity data are used in `get_transition`, the dot prefix of the derived variables
#' will be removed. This is to make it convenient when naming variables during
#' the model estimation step.
#'
#' To create a pre-processing function you can use `dplyr` or `data.table`. You can
#' even combine multiple functions with `magrittr::%>%`. As an example, if you only
#' want to filter just the male population then you can choose one of the
#' following options to create your preprocessing function.
#'
#' ```
#' # as a function using base R
#' filter_male <- function(.data) {
#'   .data[.data$sex == "male"]
#' }
#'
#' # dplyr's way
#' filter_male <- function(.data) {
#'   dplyr::filter(.data, sex == "male")
#' }
#'
#' # data.table's way
#' filter_male <- function(.data) {
#'   .data[sex == "male", ]
#' }
#'
#' # magrittr's way + dplyr's way
#' filter_male <-
#'   . %>%
#'   dplyr::filter(., sex == "male")
#'
#' # magrittr's way + data.table's way + additional conditions
#' filter_male <-
#'   . %>%
#'   .[sex == "male", ] %>%
#'   .[age >= 50, ]
#'
#' ```
#'
#' New variables can also be added to the entity data to be used in predicting
#' their transition probability. Again we can use `dplyr`'s or `data.table`'s way.
#' The examples below show how you can add a 5-year age group variable, called `age5`,
#' to the entity data.
#'
#' ```
#' # magrittr's way + data.table's way
#' filter_male <-
#'   . %>%
#'   .[sex == "male" & age >= 50, ] %>%
#'   .[, age5 := cut(age, breaks = c(seq(0,80,5), Inf), include.lowest = TRUE, right = FALSE)]
#'
#' # magrittr's way + dplyr's way
#' filter_male <-
#'   . %>%
#'   dplyr::filter(., sex == "male" & age >= 50) %>%
#'   dplyr::mutate(., age5 = cut(age, breaks = c(seq(0,80,5), Inf), include.lowest = TRUE, right = FALSE))
#' ```
#'
#' Note that, new variables added inside preprocessing_fn won't
#' change the attribute data of the `entity` object that is undergoing a transition.
#' These variables only appear temporary within the context of the transition.
#'
#' @return [transition] returns the first argument which is the [World] object, while
#' [get_transition] a data.table objec that contains the transition outcomes with
#' two columns: id and response.
#' @export
#'
#' @examples
#'
#' # create a filter function
#' library(caret)
#'
#' filter_male <-
#'   . %>%
#'   .[sex == "male", ]
#'
#' filter_not_dead <-
#'   . %>%
#'   .[age != -1]
#'
#' # create a multinomial logit model using `caret`
#' mnl <- caret::train(marital_status ~ age + sex,
#'   data = toy_individuals,
#'   method = "multinom",
#'   trace = FALSE
#' )
#' # this model denotes that there is a 10% chance that an individual will decease
#' death_model <- list(yes = 0.1, no = 0.9)
#'
#' # create a toy world
#' create_toy_world()
#'
#' # simulate marital status transition and update the attribute.
#' transition(world,
#'   entity = "Individual",
#'   model = mnl,
#'   preprocessing_fn = filter_male,
#'   attr = "marital_status"
#' )
#'
#' # get a transition result
#' get_transition(world,
#'   entity = "Individual",
#'   model = mnl,
#'   preprocessing_fn = filter_male
#' )
#'
#' # lets make a pipeline of transitions
#' world %>%
#'   transition(
#'     entity = "Individual",
#'     model = mnl,
#'     preprocessing_fn = . %>% filter_male() %>% filter_not_dead(),
#'     attr = "marital_status"
#'   ) %>%
#'   transition(
#'     entity = "Individual",
#'     model = death_model,
#'     preprocessing_fn = filter_not_dead,
#'     attr = "age",
#'     values = c(yes = -1L)
#'   )
#' # print the attributes of the individual agents
#' world$entities$Individual$get_data()
transition <-
  function(world,
           entity,
           model,
           target = NULL,
           targeted_ids = NULL,
           preprocessing_fn = NULL,
           attr = NULL,
           values = NULL,
           verbose = FALSE) {
    result <- get_transition(world, entity, model, target, targeted_ids, preprocessing_fn)
    e <- world$get(entity)
    if (verbose) {
      if (nrow(result) == 0) {
        lg$warn("No {entity} records were selected for transition.")
      } else {
        if (is.numeric(result[["response"]])) {
          rs <- summary(result[["response"]])
          .value <- paste(names(rs),
            round(rs, 2),
            collapse = " | "
          )
        }

        if (is.character(result[["response"]])) {
          rs <- summary(as.factor(result[["response"]]))
          .value <- paste0(names(rs), ": ",
            round(rs, 2),
            collapse = " | "
          )
        }

        msg <- glue::glue(
          "In this transition, there are {result[, uniqueN(id)]} {class(e)[[1]]} \\
        agents with {result[, uniqueN(response)]} unique responses \\
        of type {result[, typeof(response)]} {{{.value}}}"
        )

        message(msg)
      }
    }
    # update attr using the result
    if (!is.null(attr) & nrow(result) != 0) {
      checkmate::assert_names(x = attr, subset.of = e$database$attrs$colnames)
      if (!is.null(values)) {
        checkmate::assert_atomic(
          x = values,
          names = "strict",
          any.missing = FALSE,
          max.len = uniqueN(result[["response"]])
        )
        relabelled_responses <- values[match(result[["response"]], names(values))]
        valid_responses <- relabelled_responses[!is.na(relabelled_responses)]
        idx <- e$get_idx(ids = result[!is.na(relabelled_responses), id])
        if (class(valid_responses) != e$get_data(copy = FALSE)[, class(get(attr))]) {
          stop(
            "`values` must have the same type as the variable in `attr`. ",
            class(valid_responses), "!=", e$get_data(copy = FALSE)[, class(get(attr))]
          )
        }
        data.table::set(
          x = e$get_data(copy = FALSE),
          i = idx,
          j = attr,
          value = valid_responses
        )
      } else {
        idx <- e$get_idx(result[["id"]])
        data.table::set(e$get_data(copy = FALSE),
          i = idx,
          j = attr,
          value = result[["response"]]
        )
      }
    }
    # return world to make this function pipable.
    invisible(world)
  }

#' @rdname transition
#' @export
get_transition <- function(world, entity, model, target = NULL, targeted_ids = NULL, preprocessing_fn = NULL) {
  checkmate::assert_r6(world, classes = "World")

  if (!checkmate::test_string(entity, na.ok = FALSE)) {
    stop(
      "`entity` has to be a string indicating the name of an entity object ",
      "to undergo the transition."
    )
  }

  if (!checkmate::test_choice(entity, names(world$entities))) {
    stop("'", entity, "' not found in `world`.")
  }

  checkmate::assert(
    check_transition_supported_model(model),
    checkmate::check_r6(model, classes = "Model")
  )
  assert_target(target, null.ok = TRUE)
  e <- world$get(entity)
  e_data <- e$get_data()
  if (!is.null(targeted_ids)) {
    assert_entity_ids(e, ids = targeted_ids, informative = TRUE, .var.name = entity)
    e_data <- e_data[get(e$get_id_col()) %in% targeted_ids, ]
  }
  if (!is.null(preprocessing_fn)) {
    checkmate::assert_function(preprocessing_fn, nargs = 1)
    e_data <-
      preprocessing_fn(e_data)
  }
  if (!is.null(model$preprocessing_fn) && checkmate::test_r6(model, classes = "Model")) {
    e_data <-
      model$preprocessing_fn(e_data)
  }
  e_data <- dymiumCore::normalise_derived_vars(e_data)
  if (nrow(e_data) == 0) {
    return(data.table(id = integer(), response = character()))
  }
  result <-
    simulate_choice(
      model = model,
      newdata = e_data,
      target = target
    ) %>%
    data.table::data.table(
      id = e_data[[e$get_id_col()]],
      response = .
    )
  result
}
