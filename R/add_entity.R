#' @title Add new records to an Entity object
#'
#' @description
#'
#' This function allows new records to be added to an [Entity] object stored inside
#' a [World] object. This is one of the pipe-friendly functions.
#'
#' @param world (`World`)\cr
#'  A [World] object.
#' @param entity (`character(1)`)\cr
#'  An [Entity]'s name.
#' @param newdata (`data.frame()`|[data.table::data.table()])\cr
#'  A new attribute data to be added to `entity`. The new data should have all
#'  the same columns as the attribute data of `entity`.
#' @param target (`Target`|`integer(1)`)\cr
#'  An external target indicating how many new records from `newdata` should be
#'  added. The selection is ramdomised with all records having the same weight.
#'  However, if the `weight_col` is specified then the weight of each record is
#'  equal to its value in `weight_col`.
#' @param weight_col (`character(1)`)\cr
#'  Name of the weight variable in `newdata` if any. This is only considered when
#'  the `target` is set. Otherwise all records in `newdata` will be added to
#'  the [Entity] named `entity`.
#' @param replace (`logical(1)`)\cr
#'  Random draw with replacement. Default as `TRUE`.
#' @param check_relationship_id_cols (`logical(1)`)\cr
#'  Check other relationship ids of the [Entity] in `entity` to see whether they
#'  exist in the data of existing `entity` or not. Default as `FALSE`.
#' @param condition (`any`)\cr
#'  This allows a condition to be set whether or not this function should be
#'  executed. Default as `TRUE`. But it actually accepts any object that when
#'  is evaulated returns a logical value.
#'
#' @return the input `World` object.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' add_entity(world, entity = "Individual", newdata = toy_individuals)
add_entity <-
  function(world,
           entity,
           newdata,
           target = NULL,
           weight_col = NULL,
           replace = TRUE,
           check_relationship_id_cols = FALSE,
           condition = TRUE) {
  checkmate::assert_r6(world, classes = "World")
  checkmate::assert_string(entity)
  checkmate::assert_data_frame(newdata, col.names = "strict", min.rows = 1)
  checkmate::assert(
    checkmate::check_count(target, null.ok = T, positive = T),
    check_target(target, null.ok = TRUE)
  )
  checkmate::assert_flag(check_relationship_id_cols, na.ok = FALSE, null.ok = FALSE)
  e <- world$get_entity(entity)
  main_cols <- omit_derived_varnames(e$database$attrs$data)
  checkmate::assert_names(x = e$database$attrs$colnames, identical.to = main_cols)
  if (!data.table::is.data.table(newdata)) {
    newdata <- as.data.table(newdata)
  }
  if (!is.null(target)) {
    if (is(target, "Target")) {
      target_val <- target$get()
    } else {
      target_val <- target
    }
    selected_ids <- sample_choice(x = newdata[[e$id_col[[1]]]],
                                  size = target_val,
                                  replace = replace,
                                  prob = newdata[[weight_col]])
    newdata <-
      newdata[get(e$id_col[[1]]) %in% selected_ids,] %>%
      .[, .SD, .SDcols = names(.)[!names(.) %in% weight_col]]
  }
  if (checkmate::test_true(condition)) {
    lg$warn("Added {nrow(newdata)} new records to {e$class()}.")
    e$add(.data = newdata, check_existing = check_relationship_id_cols)
  } else {
    lg$warn("Added 0 new records to {e$class()}, since `condition` was FALSE.")
  }
  invisible(world)
}