#' @title Asset class
#'
#' @include Entity.R
#'
#' @description
#' An [Asset] represents physical objects such as buildings and vehicles that
#' can be owned/rent/belong by or to agents of the [Agent] sub classes.
#' For example, [Household] agents own or rent residential buildings.
#'
#' @section Construction:
#'
#' ```
#' x <- Asset$new(.data, id_col, owner)
#' ```
#'
#' Stores `.data` as a DataBackend object inside the object's list of data (`private$.data`)
#' and registers the `id_col` (`private$.id_col`).
#'
#' * `.data` :: `data.frame`\cr
#'   A object that inherits from `data.frame`.
#'
#' * `id_col` :: `character(1)`\cr
#'   The id column of `.data`.
#'
#' * `owner` :: an [R6::R6Class] object that inherits [Agent].\cr
#'
#' @section Fields:
#'
#' `NULL`\cr
#'
#' @section Methods:
#'
#' * `owner(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Returns owner ids.
#'
#' * `get_owner_id_col()`\cr
#'  () -> `character(1)`.\cr
#'  Returns the id column of the owner object.
#'
#' * `get_owner()`\cr
#'  () -> an [R6::R6Class] object that inherits [Agent].\cr
#'  Returns the owner object of this asset.
#'
#' * `set_owner(x)`\cr
#'  (an [R6::R6Class] that inherits [Agent])\cr
#'  Set owner. This assigns the owner object to the `Owner` field of this
#'  asset hence you can access the owner object using `x$Owner` (assuming that
#'  the asset object was named `x`).
#'
#' * `is_owned(ids)`\cd
#'  (`integer()`) -> (`logical()`)\cr
#'  Check if the assets in `ids` are owned.
#'
#' * `who_owns(ids)`\cr
#'  (`integer()`) -> (`integer()`)\cr
#'  Get the owner ids of the assets in `ids`.
#'
#' @export
Asset <- R6::R6Class(
  classname = "Asset",
  inherit = Entity,
  public = list(

    initialize = function(.data, id_col, owner) {
      if (!missing(.data) & !missing(id_col)) {
        super$initialize(databackend = DataBackendDataTable,
                         .data = .data,
                         id_col = id_col)
      }
      if (!missing(owner)) {
        self$set_owner(owner)
      }
      invisible()
    },

    initialise_data = function(.data, id_col, owner) {
      if (length(private$.data) != 0) {
        stop("Agent's `attrs` data has already been initialised.")
      }
      self$initialize(.data = .data,
                      id_col = id_col,
                      owner = owner)
      # keep track of latest agent id
      self$generate_new_ids(n = max(.data[[private$.id_col]]))
      invisible()
    },

    set_owner = function(x) {
      checkmate::assert_r6(x, classes = c("Agent", "Entity"))
      owner_id_col <- x$get_id_col()
      checkmate::assert_names(x = x$data()$colnames(),
                              must.include = self$get_id_col(),
                              .var.name = "Attribute data of owner 'x'")
      if (!checkmate::test_subset(self$get_attr(x = owner_id_col),
                                  choices = x$get_ids())) {
        stop(glue::glue("Some {x$class()} owner ids in {self$class()} are missing from {x$class()}"))
      }
      lg$info("Setting owner of {class(self)[[1]]} to {class(x)[[1]]}")
      private$.Owner <- x
      private$.owner_id_col <- owner_id_col
      invisible()
    },

    get_owner_id_col = function() {
      if (is.null(private$.owner_id_col)) {
        lg$warn("owner id column is NULL, this is likely that the owner object of \\
                this asset has not been set.")
      }
      private$.owner_id_col
    },

    get_owner = function() {
      private$.Owner
    },

    is_owned = function(ids) {

    },

    who_owns = function(ids) {

    }
  ),

  private = list(
    .Owner = NULL, # a place holder for the owner object.
    .owner_id_col = NULL
  )
)
