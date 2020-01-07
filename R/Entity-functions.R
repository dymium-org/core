add_history <- function(entity, ids, event, time = .get_sim_time()) {
  checkmate::assert_r6(entity, classes = "Entity")
  checkmate::assert_integerish(ids, lower = 0)
  checkmate::assert_string(event)
  checkmate::assert_number(time, lower = 0, finite = T)

  if (!"history" %in% entity$get_data_names()) {
    history_template <- .create_history_data2(id_col = entity$get_id_col())
    entity$add_data(.data = history_template, name = "history")
  }

  history_data <- .create_history_data2(entity$get_id_col(),
                                       id = ids,
                                       time = time,
                                       event = event)

  entity$data("history")$add(history_data)

  invisible()
}

# this shouldn't be used anywhere that is not inside `add_history()` function
.create_history_data <- function(id_col, time = 0, event = "INIT", id = 0L) {
  data.table(time = time,
             event = event,
             id = id) %>%
    data.table::setnames(., old = "id", new = id_col)
}

.create_history_data2 <- function(id_col, time = 0, event = "INIT", id = 0L) {
  data.table(time = time,
             event = event,
             id = list(id)) %>%
    data.table::setnames(., old = "id", new = id_col)
}

merge_entities <- function(entity_x, entity_y, x_dataname, y_dataname) {

}
