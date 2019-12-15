#' @title data.table::data.table as data backend
#'
#' @description
#'
#' DataBackendDataTable class is immutable.
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include DataBackendDataFrame.R
#'
#' @section Construction:
#' ```
#' x <- DataBackendDataTable$new(.data)
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   The input [data.table::data.table()].
#'
#' @section Fields:
#'
#'  * `NULL`\cr
#'
#' @section Methods:
#'
#'  * `add(.data, fill = FALSE)`\cr
#'  ([data.table::data.table]) -> `NULL`\cr
#'  Add data.
#'
#' @export
DataBackendDataTable <- R6::R6Class(
  classname = "DataBackendDataTable",
  inherit = DataBackendDataFrame,
  public = list(
    initialize = function(.data) {
      checkmate::assert_data_table(.data, min.rows = 1, null.ok = FALSE, col.names = "strict")
      super$initialize(data.table::copy(.data))
    },

    add = function(.data, fill = FALSE) {
      checkmate::assert_data_table(.data, min.rows = 1, null.ok = FALSE, col.names = "strict")
      # check that all existing column names in the newdata (.data) have the same
      # types as the existing ones.
      common_names <- Reduce(intersect, list(names(.data), names(self$get())))
      typeof_new <- sapply(.data, typeof)[common_names]
      typeof_existing <- sapply(self$get(), typeof)[common_names]
      check_equality <- all.equal(typeof_existing, typeof_new)
      if (!isTRUE(check_equality)){
        cli::cli_alert_danger("Type mismatches found")
        cli::cli_alert_danger("Type of existing data:")
        print(typeof_existing)
        cli::cli_alert_danger("Type of existing new data (.data):")
        print(typeof_new)
        stop(check_equality)
      }
      private$.data <- rbind(private$.data, .data, fill = fill)
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
                  .missing_rows_char = glue::glue_collapse(.missing_rows, width = 20, sep = ", ", last = " and "))
          return(invisible())
        }
        private$.removed_data <- rbind(private$.removed_data, private$.data[rows], fill = TRUE)
        private$.data <- private$.data[-rows]
      }
    }
  )
)
