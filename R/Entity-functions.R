#' Add history to entity
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
  checkmate::assert_integerish(ids, lower = 0)
  checkmate::assert_string(event)
  checkmate::assert_integerish(time, lower = 0, len = 1)
  if (id_col_as_list) {
    history_data <- .create_history_data(entity$get_id_col(),
                                         id = ids,
                                         time = time,
                                         event = event)
  } else {
    history_data <- .create_history_data2(entity$get_id_col(),
                                          id = ids,
                                          time = time,
                                          event = event)
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
  data.table(time = time,
             created_datetime = Sys.time(),
             event = event,
             id = list(id)) %>%
    data.table::setnames(., old = "id", new = id_col)
}

.create_history_data2 <- function(id_col, time, event, id) {
  data.table(time = as.integer(time),
             created_datetime = as.integer(Sys.time()),
             event = as.factor(event),
             id = id) %>%
    data.table::setnames(., old = "id", new = id_col)
}

merge_entities <- function(entity_x, entity_y, x_dataname, y_dataname) {

}
