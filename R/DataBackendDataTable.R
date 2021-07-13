#' @title data.table::data.table as data backend
#'
#' @description
#'
#' [DataBackendDataTable] uses [data.table::data.table] as back end to exploit
#' its fast and efficient implementations of many data operations. This includes
#' the `:=` operator which allows a data.table object to be modified in place,
#' without making a copy.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackendDataFrame]<-[DataBackend].
#' @include DataBackendDataFrame.R
#'
#' @section Construction:
#' ```
#' x <- DataBackendDataTable$new(.data, key = NULL)
#' ```
#'
#' * `.data` :: [data.table::data.table()]\cr
#'  The input [data.table::data.table()].
#'
#' * `key` :: `NULL`|`character(1)`\cr
#'  The key of `.data`.
#'
#' @section Fields:
#'
#'  * `key` :: `NULL`|`character(1)`\cr
#'  The key of `.data`.
#'
#'  * `data` :: [data.table::data.table()]\cr
#'  A copy of the data.table that was used to create the backend object.
#'  To get the reference of the data.table, use `$get(copy = FALSE)`.
#'
#'  * `removed_data` :: [data.table::data.table()]\cr
#'  A copy of the removed data.
#'
#' @section Methods:
#'
#'  * `add(.data, fill = FALSE)`\cr
#'  ([data.table::data.table]) -> `NULL`\cr
#'  Add data.
#'
#'  * `remove(rows, cols)`\cr
#'  (`integer()`, `integer()`) -> `NULL`\cr
#'  Remove the rows in `rows` and the columns in `cols` in the data.
#'
#'  * `get(rows, cols, copy = TRUE)`\cr
#'  (`integer(1)`, `character()`, `logical(1)`) -> `data.frame()`\cr
#'  Get data. If copy is `TRUE`, the returned data.table is a copy of the actual data.table
#'  object used to create this backend object. Otherwise, the reference to the
#'  data.table is returned which allows it to be modified with `data.table::set()` and
#'  `:=`. Note that, `rows` should be indexes of the data.table and `cols` are
#'  a character vector that contains column names to be returned.
#'
#'  * `view(interactive = FALSE)`\cr
#'  (`logical(1)`)\cr
#'  View the data. If `interactive` is TRUE, the data will be shown in your
#'  data tab if you are using RStudio.
#'
#'  * `head(n = 5)`\cr
#'  (`integer(1)`) -> `data.frame()`\cr
#'  Get the head of the data.
#'
#'  * `ncol()`\cr
#'  () -> `integer(1)`\cr
#'  Get the number of columns.
#'
#'  * `nrow()`\cr
#'  () -> `integer(1)`\cr
#'  Get the number of rows.
#'
#'  * `get_removed()`\cr
#'  () -> `data.frame()`\cr
#'  Get the removed data.
#'
#' @export
DataBackendDataTable <- R6::R6Class(
  classname = "DataBackendDataTable",
  inherit = DataBackendDataFrame,
  public = list(
    initialize = function(.data, key = NULL) {
      checkmate::assert_data_table(.data, min.rows = 1, null.ok = FALSE, col.names = "strict")
      .data <- data.table::copy(.data)
      if (!is.null(key)) {
        if (!key %in% names(.data)) {
          stop(paste0("'", key, "' key column doesn't exist in `.data`."))
        }
        if (!isTRUE(data.table::key(.data) == key)) {
          lg$trace("Setting `.data`'s key as '{key}' column.")
          data.table::setkeyv(x = .data, key)
        }
        private$.key <- key
      }
      super$initialize(.data)
    },
    add = function(.data, fill = FALSE) {
      checkmate::assert_data_table(.data, min.rows = 1, null.ok = FALSE, col.names = "strict")
      # check that all existing column names in the newdata (.data) have the same
      # types as the existing ones.
      common_names <- Reduce(intersect, list(names(.data), names(self$get())))
      typeof_new <- sapply(.data, typeof)[common_names]
      typeof_existing <- sapply(self$get(), typeof)[common_names]
      check_equality <- all.equal(typeof_existing, typeof_new, check.attributes = FALSE)
      if (!isTRUE(check_equality)) {
        cli::cli_alert_danger("Type mismatches found")
        cli::cli_alert_danger("Types of existing data:")
        print(typeof_existing)
        cli::cli_alert_danger("Types of new data (`.data`):")
        print(typeof_new)
        stop(check_equality)
      }
      private$.data <- rbind(private$.data, .data, fill = fill)
      if (!is.null(self$key)) {
        self$setkey()
      }
      invisible()
    },
    remove = function(rows, cols) {
      if (!missing(cols)) {
        checkmate::assert_names(names(private$.data), must.include = cols)
        private$.data[, c(cols) := NULL] # remove columns by reference
      }
      if (!missing(rows)) {
        checkmate::assert_integerish(rows, lower = 1, unique = TRUE, null.ok = FALSE, any.missing = FALSE)
        if (length(rows) == 0) {
          lg$warn("`rows` has length of 0, most likely integer(0). no rows have been removed.")
          return(invisible())
        }
        if (nrow(private$.data) < max(rows)) {
          .missing_rows <- rows[rows > nrow(private$.data)]
          lg$warn("These rows do not exist {.missing_rows_char}",
            .missing_rows_char = glue::glue_collapse(.missing_rows, width = 20, sep = ", ", last = " and ")
          )
          return(invisible())
        }
        private$.removed_data <- rbind(private$.removed_data, private$.data[rows], fill = TRUE)
        private$.data <- private$.data[-rows]
      }
    },
    get = function(rows, cols, copy = TRUE) {
      if (!copy) {
        return(private$.data)
      }
      if (!missing(rows)) {
        checkmate::assert_integerish(rows, any.missing = FALSE, lower = 1)
        if (max(rows) > nrow(private$.data)) {
          stop("max of 'rows' exceeds the number of rows of the data.")
        }
      }
      if (!missing(cols)) {
        checkmate::assert_names(cols, subset.of = names(private$.data))
      }
      data.table::copy(private$.data)[rows, .SD, .SDcols = cols]
    },
    setkey = function(key = private$.key) {
      data.table::setkeyv(x = private$.data, cols = key)
      private$.key <- key
      invisible(self)
    },
    get_removed_data = function() {
      self$removed_data
    }
  ),
  active = list(
    key = function() {
      return(base::get(".key", envir = private))
    },
    data = function() {
      return(data.table::copy(base::get(".data", envir = private)))
    },
    removed_data = function() {
      return(data.table::copy(base::get(".removed_data", envir = private)))
    }
  ),
  private = list(
    .key = NULL
  )
)
