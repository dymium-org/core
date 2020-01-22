#' @title data.frame as data backend
#'
#' @description
#'
#' Data backend of data.frame. It is recommended to use [DataBackendDataTable]
#' as it is more feature rich, efficient and significantly faster for many operations.
#' This class is merely an attempt to generalise methods that belong to data.frame.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackend].
#' @include DataBackend.R
#'
#' @section Construction:
#' ```
#' x <- DataBackendDataFrame$new(.data)
#' ```
#'
#' * `data` :: [data.frame]\cr
#'   A [data.frame] object.
#'
#' @section Fields:
#'
#'  * `NULL`\cr
#'
#' @section Methods:
#'
#'  * `add(.data, fill = FALSE)`\cr
#'  (`data.frame()`) -> `NULL`\cr
#'  Add data.
#'
#'  * `get(rows, cols, copy = TRUE)`\cr
#'  (`integer(1)`, `character()`, `logical(1)`) -> `data.frame()`\cr
#'  Add data.
#'
#'  * `remove(rows, cols)`\cr
#'  (`integer()`, `integer()`) -> `NULL`\cr
#'  (abstract) Remove the data. (Developer's note: Those records that get removed
#'  should be stored in `private$.removed_data` see the source code of [DataBackendDataTable]
#'  for example.)
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
#'  * `colnames()`\cr
#'  () -> `character()`\cr
#'  Get the column names.
#'
#'  * `get_removed()`\cr
#'  () -> `data.frame()`\cr
#'  Get the removed data.
#'
#' @export
DataBackendDataFrame <-
  R6::R6Class(
    classname = "DataBackendDataFrame",
    inherit = DataBackend,
    public = list(
      initialize = function(.data) {
        checkmate::assert_data_frame(.data,
                                     min.rows = 1,
                                     null.ok = FALSE,
                                     col.names = "strict")
        if (is.data.table(.data)) {
          super$initialize(data.table::copy(.data))
        } else {
          super$initialize(.data)
        }

      },

      get = function(rows, cols, copy = TRUE) {
        .dataframe_get(self, private, .data = private$.data, rows = rows, cols = cols, copy = copy)
      },

      remove = function() {
        private$abstract()
        # checkmate::assert_integerish(rows, lower = 1, unique = TRUE)
        # if (is.data.table(private$.data)) {
        #   private$.data <- private$.data[-rows]
        #   private$.removed_data <- rbind(private$.removed_data, private$.data[rows])
        # } else {
        #   private$.data <- private$.data[-rows, , drop = FALSE]
        # }
        # checkmate::assert_names(names(private$.data), must.include = cols)
        # if (!missing(cols)) {
        #   if (is.data.table(private$.data)) {
        #     private$.data[, c(cols) := NULL]
        #   } else {
        #     subset(private$.data, )
        #     private$.data[, -cols, drop = FALSE]
        #   }
        # }
      },

      view = function(interactive = FALSE) {
        checkmate::assert_logical(interactive, any.missing = FALSE, len = 1, null.ok = FALSE)
        if (interactive) {
          View(private$.data)
        } else {
          print(private$.data)
        }
      },

      head = function(n) {
        head(private$.data, n)
      },

      ncol = function() {
        ncol(private$.data)
      },

      colnames = function() {
        colnames(private$.data)
      },

      nrow = function() {
        nrow(private$.data)
      },

      n = function() {
        stop("Method not available")
      },

      get_removed = function() {
        .dataframe_get(self, private, .data = private$.removed_data, copy = FALSE)
      }
    ))

.dataframe_get <- function(self, private, .data, rows, cols, copy = TRUE) {
  checkmate::assert(
    checkmate::check_data_frame(.data, null.ok = FALSE),
    checkmate::check_logical(copy, any.missing = FALSE, len = 1, null.ok = FALSE)
  )

  if (nrow(.data) == 0) {
    lg$trace(".data is emptied returning emptied data")
    return(.data)
  }

  if (copy == FALSE & !is.data.table(.data)) {
    lg$warn("`copy = FALSE` is ignored, a data.frame doesn't have a reference semetic.")
  }

  if (copy == FALSE & is.data.table(.data)) {
    if (!missing(rows) & !missing(cols)) {
      stop("Can't return a reference semetic to a subset of a data.table.")
    }
    return(.data)
  }

  if (missing(cols) & !missing(rows)) {
    checkmate::check_integerish(rows, any.missing = FALSE, null.ok = FALSE)
    return(.data[rows,])
  }

  if (!missing(cols)) {
    checkmate::assert_names(names(.data), must.include = cols)
  }

  if (is.data.table(.data)) {
    .data[rows, cols, with = FALSE]
  } else {
    .data[rows, cols, drop = FALSE]
  }

}
