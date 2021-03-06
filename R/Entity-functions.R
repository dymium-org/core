# History -----------------------------------------------------------------
#' Add history to Entity
#'
#' @description
#' Recording past events of entities allow them to have memory of their past
#' experiences and actions. This can be later use, if requires, in future decision
#' making processes. For example, the number of past fertility events may be a
#' strong predictor of the next fertility for female individuals.
#'
#' @param entity an [Entity] or its inheritances object.
#' @param ids a integer vector that contains ids of entities to record the event
#' @param event a character value that denotes the event that is being recorded
#' @param time a integer value that represent the time at which the event occured
#' for the entities.
#' @param id_col_as_list a logical value which indicates whether the id column
#' should be stored as a list column. This significantly reduces the memory
#' footprint of the history data with a higher cost for basic data manipulation
#' since the data must be unnested to filter by entity ids.
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' create_toy_population()
#' add_history(pop$get("Individual"), ids = c(1:100), event = "test_event", time = 1)
#' pop$get("Individual")$get_data("history")
add_history <- function(entity, ids, event, time = .get_sim_time(), id_col_as_list = FALSE) {
  checkmate::assert_r6(entity, classes = "Entity")
  checkmate::assert_integerish(ids, lower = 0, min.len = 1)
  checkmate::assert_string(event)
  checkmate::assert_count(time)
  if (id_col_as_list) {
    history_data <- .create_history_data(entity$get_id_col(),
      id = ids,
      time = time,
      event = event
    )
  } else {
    history_data <- .create_history_data2(entity$get_id_col(),
      id = ids,
      time = time,
      event = event
    )
  }
  if (!"history" %in% entity$get_data_names()) {
    entity$add_data(.data = history_data, name = "history")
  } else {
    entity$data("history")$add(history_data)
  }
  invisible()
}

# this shouldn't be used anywhere that is not inside `add_history()` function
.create_history_data <- function(id_col, time, event, id) {
  data.table(
    time = time,
    created_timestamp = Sys.time(),
    event = event,
    id = list(id)
  ) %>%
    data.table::setnames(., old = "id", new = id_col)
}

.create_history_data2 <- function(id_col, time, event, id) {
  data.table(
    time = as.integer(time),
    created_timestamp = as.integer(Sys.time()),
    event = as.factor(event),
    id = id
  ) %>%
    data.table::setnames(., old = "id", new = id_col)
}


#' Get Entity history
#'
#' @description
#'
#' Get the history data of a given Entity. If a [Container] object is given then
#' this will return a list of data.tables or NULL if an [Entity] in the [Container]
#' doesn't have any history data.
#'
#' @param x An R6 object that belongs to one of these classes: [Entity], [Population] and [World].
#' @param ... (not being used) dots
#'
#' @return a list of data.table or `NULL`.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#'
#' add_history(world$get("Individual"),
#'   ids = 1:100,
#'   event = "test_event1",
#'   time = 1
#' )
#'
#' get_history(world)
#' get_history(world$get("Individual"))
get_history <- function(x, ...) {
  UseMethod("get_history", x)
}

#' @rdname get_history
#' @export
get_history.Container <- function(x, ...) {
  purrr::map(
    .x = x$Cont[sapply(x$Cont, function(x) inherits(x, "Entity"))],
    .f = ~ {
      get_history.Entity(.x)
    }
  )
}

#' @rdname get_history
#' @export
get_history.Entity <- function(x, ...) {
  if (!is.null(x$database[["history"]])) {
    return(x$get_data("history"))
  } else {
    return(NULL)
  }
}


impute_history <- function(entity, ids, event = NULL) {
  checkmate::assert_r6(entity, classes = "Entity")
  checkmate::assert_integerish(ids, lower = 0)
  checkmate::assert_string(event)
}

#' Combine history data of Entities into a single data.frame.
#'
#' @param x a R6 object that inherits [Container] such as [World].
#'
#' @return a data.table with five columns: time, created_timestamp, event, id, entity
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' Ind <- world$get("Individual")
#' add_history(Ind, ids = sample(Ind$get_ids(), 10), event = "event1", time = 1)
#' combine_histories(world)
combine_histories <- function(x) {
  checkmate::expect_r6(x, classes = "Container")
  get_history(x) %>%
    purrr::keep(., ~ !is.null(.x)) %>%
    purrr::map2(
      .x = ., .y = names(.),
      .f = ~ {
        .x %>%
          data.table::setnames(., old = 4, new = "id") %>%
          data.table::set(., j = "entity", value = .y)
      }
    ) %>%
    rbindlist(.)
}

merge_entities <- function(entity_x, entity_y, x_dataname, y_dataname) {

}

#' Inspect Entity
#'
#' @description
#' Return and print to console the attribute data of entities within an [Entity]
#' object or its inheritance object. A related entity object can also be queried
#' at the same time.
#'
#' @param entity an [Entity] object or its inheritance object
#' @param ids a integer vactor that contains the ids of the entities to be inspected
#' @param related_entity an [Entity] object or its inheritance object which must
#' be related to `entity` but not the same as `entity`. For example, `related_entity`
#' can be [Household] if `entity` is [Individual].
#' @param verbose :: `logical(1)`
#' Should the data be printed to console.
#'
#' @return a named ist of data.tables.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#'
#' inspect(
#'   entity = world$get("Individual"),
#'   ids = c(1:3),
#'   related_entity = world$get("Household")
#' )
inspect <- function(entity, ids, related_entity = NULL, verbose = TRUE) {
  checkmate::assert_r6(entity, classes = c("Entity"))
  checkmate::assert_integerish(ids, lower = 0, any.missing = FALSE)
  checkmate::assert_r6(related_entity, classes = c("Entity"), null.ok = TRUE)

  cli::cli_alert_info("Attribute data of {entity$class()}")
  entity_data <- entity$get_data(ids = ids)
  print(entity_data)

  if (!is.null(related_entity)) {
    if (!entity$get_id_col() %in% related_entity$database$attrs$colnames &
      !related_entity$get_id_col() %in% entity$database$attrs$colnames) {
      stop(glue::glue("'entity' cannot be linked with 'related_entity' \\
                      through their primary id variables."))
    }
    if (verbose) {
      cli::cli_alert_info("Attribute data of {related_entity$class()}")
    }

    # entities are 'members' to related entities
    if (related_entity$get_id_col() %in% entity$database$attrs$colnames) {
      if (verbose) {
        cli::cli_alert_info("Note that, entities are 'members' to related entities.")
      }
      related_entity_ids <-
        entity$get_attr(x = related_entity$get_id_col(), ids = ids)
      related_entity_data <-
        related_entity$get_data(ids = related_entity_ids)
    }
    # entities are 'groups' of related entities
    if (entity$get_id_col() %in% related_entity$database$attrs$colnames) {
      if (verbose) {
        cli::cli_alert_info("Note that, entities are 'groups' of related entities.")
      }
      related_entity_data <-
        related_entity$get_data(copy = FALSE)[get(entity$get_id_col()) %in% ids]
    }

    if (verbose) {
      print(related_entity_data)
    }
  } else {
    related_entity_data <- NULL
  }

  if (!is.null(entity$database[["history"]])) {
    entity_history <- entity$get_data("history", copy = FALSE)[get(entity$get_id_col()) %in% ids, ]
    if (verbose) {
      cli::cli_alert_info("History data of {entity$class()}")
      print(entity_history)
    }
  } else {
    entity_history <- NULL
  }

  invisible(list(
    entity = entity_data,
    related_entity = related_entity_data,
    entity_history = entity_history
  ))
}

#' Plot history data
#'
#' @param x a history data as a data.table or [Container].
#' @param by_entity Subplots by entitiy. A logical value `TRUE` by default.
#'
#' @return a ggplot object
#' @export
plot_history <- function(x, by_entity = TRUE) {
  if (!requireNamespace("ggplot2", quietly = T)) {
    stop("Please install `ggplot2` to use this function.")
  }
  if (!requireNamespace("patchwork", quietly = T)) {
    stop("Please install `patchwork` to use this function.")
  }
  if (!requireNamespace("scales", quietly = T)) {
    stop("Please install `scales` to use this function.")
  }

  if (inherits(x, "Container")) {
    x <- combine_histories(x)
  }

  checkmate::assert_data_table(x)
  checkmate::assert_names(names(x),
    permutation.of = c(
      "time", "created_timestamp", "event",
      "id", "entity"
    )
  )
  if (by_entity) {
    p <-
      (purrr::map(
        .x = x[, unique(entity)],
        .f = ~ {
          p <-
            ggplot2::ggplot(data = x[entity == .x], ggplot2::aes(time, fill = event)) +
            ggplot2::geom_bar() +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
            ggplot2::labs(title = .x)
        }
      ) %>% {
        Reduce(`/`, .)
      }) +
        patchwork::plot_layout(guides = "collect") &
        patchwork::plot_annotation(
          title = "An aggregate plot of all Entities' historical events",
          theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
        )
  } else {
    p <-
      ggplot2::ggplot(data = x, ggplot2::aes(time, fill = event)) +
      ggplot2::geom_bar() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::labs(title = "An aggregate plot of all Entities' historical events") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
  }
  return(p)
}
