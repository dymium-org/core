#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @title Modified version of base::sample for programming in a Monte Carlo simulation
#'
#' @description
#' This version of the sample function removes the feature where x is an integer of length 1
#' sampling via sample takes place from 1:x. This can bring unexpected behaviour
#' as mentioned in the documentation of `base::sample()`.
#'
#' @param x a vector that contains value(s) that represents a choiceset.
#' @inheritParams base::sample
#' @export
#'
#' @examples
#'
#' sample_choice(7, 10, replace = TRUE) # equipvalent to rep(7, 10)
#' sample_choice(7, 1)
#' sample_choice(7) # which is equipvalent to the above
sample_choice <- function(x, size = 1, replace = FALSE, prob = NULL) {
  if (length(x) != 1) {
    sample(x = x, size = size, replace = replace, prob = prob)
  } else {
    if (replace || size == 1) {
      rep(x, size)
    } else {
      stop("cannot take a sample larger than the population when 'replace = FALSE'")
    }
  }
}

#' Condense rows
#'
#' @description
#' Condense two vectors - where the first vector is an id vector and the second
#' vector is an target (which must be unique) -  into a data.table with two
#' columns (id and target).
#'
#' @param id (`integer()`) a integer vector contains ids.
#' @param target (`integer()`) a integer vector contains values with the same length as `id`.
#' @param names (Default as `NULL`)
#'
#' @return a data.table with two columns, `id` and `target`.
condense_rows <- function(id, target, names = NULL){

  checkmate::assert(
    checkmate::check_integerish(id, any.missing = FALSE, null.ok = FALSE),
    checkmate::check_integerish(target, any.missing = FALSE, null.ok = FALSE),
    checkmate::check_true(length(id) == length(target)),
    combine = "and"
  )

  if (is.numeric(id)) {
    id <- as.integer(id)
  }

  if (is.numeric(target)) {
    target <- as.integer(target)
  }

  dt <- data.table(id = id, target = target)
  dt <- dt[, .(target = list(target)), by = id]

  if (!is.null(names)) {
    stopifnot(length(names) == ncol(dt))
    names(dt) <- names
  }

  return(dt)
}


#' @title element_wise_expand_lists
#'
#' @description expand two lists into a dataframe. l1 and l2 can be a nested list.
#'
#' @param l1 first list
#' @param l2 second list
#'
#' @return data.frame with two columns, Var1 and Var2.
#' @export
#'
#' @examples
#'  l1 <- list(1,2,3,4)
#'  l2 <- list(1,2,3,4)
#'  element_wise_expand_lists(l1,l2)
#'
#'  l1 <- list(1,2,3,4)
#'  l2 <- list(1,2,3,c(1,3))
#'  element_wise_expand_lists(l1,l2)
element_wise_expand_lists = function(l1, l2) {
  stopifnot(length(l1) == length(l2))
  as.data.table(do.call(rbind, Map(expand.grid, l1, l2)))
}


#' Look up and replace values in columns, including list columns.
#'
#' Replace all values in a data.frame using a lookup table. Maximum of one list
#' column can be include in `cols_to_lookup`.
#'
#' @param data a data.table/data.frame to be replaced.
#' @param lookup_table a data.table/data.frame contains two columns which must be
#' named as `.key` and `value`. The reason for requiring a dot infront of `key`
#' is that `key` is a reserved column name in `data.table` meaning no columns
#' can be named `key`. Also the `.key` column should have the same data type as
#' `cols_to_lookup` columns.
#' @param cols column names to be lookup and replace
#' @param id_col a column with unique value to be used in unnesting `data`. This
#' is not required if `cols_to_lookup` doesn't contain any list columns.
#'
#' @return data.table with new values
#'
#' @export
lookup_and_replace = function(data, lookup_table, cols, id_col = NULL) {
  # checks
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_names(names(data), must.include = cols),
    checkmate::check_data_frame(lookup_table, max.cols = 2),
    checkmate::check_names(names(lookup_table), subset.of = c(".key", "key", "value")),
    combine = "and"
  )

  # transform data
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  if (!is.data.table(lookup_table)) {
    lookup_table <- as.data.table(lookup_table)
  }

  if ("key" %in% names(lookup_table)) {
    data.table::setnames(lookup_table, "key", ".key")
  }

  all_list_cols <- names(data)[sapply(data, class) == "list"]
  data <- copy(data)

  data_emptied <- copy(data)[0, ]

  # replace values
  list_cols <- all_list_cols[all_list_cols %in% cols]
  non_list_cols <- cols[!cols %in% list_cols]

  if (length(list_cols) != 0 & is.null(id_col))
    stop("`id_col` must be provided if `list_col` is not emptied.")
  if (length(list_cols) == 1) {
    data <-
      .lookup_and_replace_list_cols(data, lookup_table, id_col, list_cols)
  }

  data <-
    .lookup_and_replace_non_list_cols(data, lookup_table, non_list_cols)

  stopifnot(identical(data[0, ], data_emptied[0, ]))
  return(data)
}


#' Look up and replace values in columns.
#'
#' @description
#'
#' Replace keys with their values.
#'
#' @param x a data.frame
#' @param cols a character vector of column names in `x` to be replaced.
#' @param mapping a data.frame contains two columns which are `.key` and `.value`
#'
#' @return a data.frame
#' @export
#'
#' @examples
lookup_and_replace2 <- function(x, cols, mapping) {
  checkmate::assert_data_frame(x)
  checkmate::assert_character(cols)
  checkmate::assert_names(names(x), must.include = cols)
  checkmate::assert_data_frame(mapping, min.rows = 1, min.cols = 2)
  checkmate::assert_names(names(mapping), must.include = c(".key", ".value"))

  if (!is.data.table(x)) {
    x <- as.data.table(x)
  }

  if (!is.data.table(mapping)) {
    mapping <- as.data.table(mapping)
  }

  # preserve for validation
  x_col_order <- names(x)
  x_str <- x[0, ]

  for (col in cols) {

    # check that the data type in key and col are matched
    if (typeof(x[["col"]]) != typeof(mapping[["value"]])) {
      stop(paste0("Type of '", col, "' in `x` and '.key' in `mapping` are not matched."))
    }

    # merge new values
    x <-
      merge(x = x,
            y = mapping,
            by.x = col,
            by.y = ".key",
            all.x = TRUE,
            sort = FALSE,
            allow.cartesian = FALSE
      )

    # check
    if (sum(is.na(x[[col]])) != sum(is.na(x[[".value"]]))) {
      stop(paste0("Some entries in the '", col, "' column couldn't ",
                  "find a mapping value in `mapping`."))
    }

    # final cleansing
    x %>%
      .[, c(col) := NULL] %>%
      data.table::setnames(., old = ".value", new = col)

  }

  # preserve the original column order
  data.table::setcolorder(x, x_col_order)

  # don't check keys
  data.table::setkey(x_str, NULL)
  data.table::setkey(x, NULL)

  # final checking of column types and data dimiensions
  checkmate::assert_data_table(x)
  checkmate::assert_names(names(x), identical.to = names(x_str))

  return(x)
}

#' .lookup_and_replace_list_cols
#'
#' @param data data.table
#' @param lookup_table lookup_table
#' @param id_col NA
#' @param list_cols NA
#'
#' @return data
#'
#' @noRd
.lookup_and_replace_list_cols = function(data, lookup_table, id_col, list_cols) {
  .data <- data
  if (length(list_cols) > 1)
    stop(
      paste(
        ".lookup_and_replace_list_cols can only work with one",
        "list column right now."
      )
    )
  if (length(names(.data)[sapply(.data, class) == "list"]) == 0)
    stop("There is no list columns in `data`.")
  if (length(.data[, get(id_col)]) != length(unique(.data[, get(id_col)])))
    stop("id_col must be unique!")
  if (!is.data.table(.data))
    lookup_table <- as.data.table(.data)
  if (!is.data.table(lookup_table))
    lookup_table <- as.data.table(lookup_table)


  .data <-
    unnest_datatable(.data, by_col = id_col) %>%
    .[lookup_table,
      on = paste0(list_cols, "==.key"),
      eval((list_cols)) := value] %>%
    # nest the list column back
    .[,
      `:=`(list_col_tmp = .(get(list_cols))),
      by = eval((id_col))] %>%
    .[, eval((list_cols)) := list_col_tmp] %>%
    .[, list_col_tmp := NULL] %>%
    .[, .SD[1], by = eval((id_col))] %>%
    .[, eval((list_cols)) := purrr::map(.x = get(list_cols),
      .f = ~ purrr::keep(.x, ~ !is.na(.x)))]

  return(.data)
}

.lookup_and_replace_non_list_cols = function(data, lookup_table, non_list_cols) {
  if (!is.data.table(lookup_table))
    lookup_table <- as.data.table(lookup_table)

  # replace values in normal columns
  for (col in non_list_cols) {
    # update on join
    data[lookup_table, on = paste0(col, "==.key"), (col) := value]
  }
  return(data)
}

#' unnest_datatable
#'
#' unnest data.table object
#'
#' @param dt data.table object
#' @param by_col reference column
#'
#' @export
unnest_datatable = function(dt, by_col) {
  stopifnot(is.data.table(dt))
  stopifnot(all(by_col %in% names(dt)))

  # unnest the list column
  # https://stackoverflow.com/questions/34692260/how-to-ungroup-list-columns-in-data-table
  dt <- dt[,
    lapply(.SD, unlist),
    by = eval((by_col))]

  return(dt)
}

#' Group a column into a list column and sort by the group-by column
#'
#' @param x :: [data.table::data.table()]
#' @param groupby_col :: `character(1)`
#'   the column to use in 'by' in the data.table `x`.
#' @param group_col :: `character(1)`
#'   the column to be grouped into a list column.
#' @param sort_order :: `vector()`
#'   A vector of ordered values to sort the `groupby_col`. If a value in the
#'   vector is missing from `groupby_col` in `x` its `group_col` will be `NA`.
#'
#' @return [data.table::data.table()] with 2 columns: `sort_col`, `group_col`.
#'
#' @note
#'
#' The returned [data.table::data.table()] should have the same number of rows
#' as the length of `sort_col`.
dt_group_and_sort <- function(x, groupby_col, group_col, sort_order) {
  checkmate::assert_data_table(x)
  stopifnot(groupby_col %in% names(x))
  stopifnot(group_col %in% names(x))
  checkmate::expect_integerish(sort_order, unique = T, lower = 0, min.len = 1)

  # group and sort
  x_new <-
    x %>%
    .[, .(group_col = list(get(group_col))), by = groupby_col] %>%
    merge(x = data.table(sort_col = sort_order), y = .,
          by.x = "sort_col", by.y = groupby_col, all.x = T, sort = FALSE)

  #' In short, the if-statement makes sure that all emptied `group_col` will be `NA` not `NULL`
  #'
  #' For cases where some `sort_order` are not in `groupby_col` of `x`
  #' those elements in `sort_order` will have their values in `groupby_col` as `NULL`
  if (!any(is.na(x[[group_col]])) | any(!sort_order %in% x[[groupby_col]])) {
    x_new[sapply(group_col, is.null), group_col := NA_integer_]
  }

  stopifnot(nrow(x_new) == length(sort_order))
  x_new
}


.slug = function(x, ext) {
  x_base <- path_ext_remove(x)
  x_ext <- path_ext(x)
  ext <- if (identical(tolower(x_ext), tolower(ext)))
    x_ext
  else ext
  path_ext_set(x_base, ext)
}

.check_file_name = function(x) {
  if (!checkmate::test_character(x, pattern = "^[a-zA-Z0-9._-]+$", len = 1, any.missing = FALSE, null.ok = FALSE)) {
    stop(glue::glue("'{x}' is not a valid file name. It should contain only \\
                    ASCII letters, numbers, '-', and '_'."))
  }
  return(TRUE)
}

check_names <- function(x, names) {
  if (!checkmate::test_named(x, type = "unique")) {
    stop("Names of 'x' must be unique!")
  }
  nms <- names(x)
  return(names %in% nms)
}

skip_on_not_master <- function() {
  if (get_current_git_branch() == "master") {
    return(invisible(TRUE))
  }
  skip("Skip on branch not master")
}

get_current_git_branch <- function() {
  branches <- system("git branch", intern = T)
  current_branch <-
    grep("\\*", branches, value = T) %>%
    gsub("\\*|\\ ", "", .)
  return(current_branch)
}
