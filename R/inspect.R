inspect <- function(entity, ids, related_entity = NULL) {
  checkmate::assert_r6(entity, classes = c("Entity"))
  checkmate::assert_integerish(ids, lower = 0, any.missing = FALSE)
  checkmate::assert_r6(related_entity, classes = c("Entity"), null.ok = TRUE)

  cli::cli_alert_info("Attribute data of {entity$class()}")
  entity_data <- entity$get_data(ids = ids)
  print(entity_data)

  if (!is.null(related_entity)) {
    if (!related_entity$get_id_col() %in% entity$data()$colnames()) {
      stop(glue::glue("'entity' cannot be linked with 'related_entity' \\
                      through their id variables."))
    }
    cli::cli_alert_info("Attribute data of {related_entity$class()}")
    related_entity_ids <- unique(entity$get_attr(x = related_entity$get_id_col(), ids = ids))
    related_entity_data <- related_entity$get_data(ids = related_entity_ids)
    print(related_entity_data)
  } else {
    related_entity_data <- NULL
  }

  if (!is.null(entity$database[["history"]])) {
    entity_history <- entity$get_data("history", copy = FALSE)[get(entity$get_id_col()) %in% ids,]
    cli::cli_alert_info("History data of {entity$class()}")
    print(entity_history)
  } else {
    entity_history <- NULL
  }

  invisible(list(entity = entity_data,
                 related_entity = related_entity_data,
                 entity_history = entity_history))
}
