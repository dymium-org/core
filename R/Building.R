#' @title Building class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include Asset.R
#'
#' @description  Create Building class, extended Agent class.
#'
#' @section Construction:
#'
#' ```
#' x <- Entity$new(databackend, .data, id_col)
#' ```
#'
#' Stores `.data` as a DataBackend object inside the object's list of data (`private$.data`)
#' and registers the `id_col` (`private$.id_col`).
#'
#' * `databackend` :: a [R6::R6Class] generator\cr
#'   A [R6::R6Class] generator that inherits from `DataBackend`.
#'
#' * `.data` :: `data.frame()`\cr
#'   A object that inherits from `data.frame`.
#'
#' * `id_col` :: `character(1)`\cr
#'   The id column of `.data`.
#'
#' @section Fields:
#'
#' * `NULL`\cr`
#'
#' @section Methods:
#'
#' * `is_occupied(ids)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Returns TRUE if dwelling in ids is occupied
#'
#' * `is_vacant(ids)`\cr
#'   (`integer()`) -> `logical()`\cr
#'   Returns TRUE if dwelling in ids is vacant.
#'
#' @export
Building <- R6Class(
  classname = "Building",
  inherit = Asset,
  public = list(

    # minimum required data structure
    data_template = function() {
      data.table(
        # the first column should always be the unique id column of the class
        bid = integer(), # building id
        zid = integer() # zone id
      )
    },

    is_occupied = function(ids) {
      !is.na(self$get_attr(x = "hid", ids = ids))
    },

    is_vacant = function(ids) {
      !self$is_occupied(ids = ids)
    }
  ),
  private = list()
)


