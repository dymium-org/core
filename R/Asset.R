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
#' x <- Asset$new(.data, id_col, owner = NULL)
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
#' * `is_owned(ids)`\cr
#'  (`integer()`) -> (`logical()`)\cr
#'  Check if the assets in `ids` are owned. If `ids` is not given the result will
#'  be for all assets.
#'
#' * `get_owner(ids)`\cr
#'  (`integer()`) -> (`integer()`)\cr
#'  Get the owner ids of the assets in `ids`.
#'
#' * `set_owner(ids, owner_ids)`\cr
#'  (`integer()`|`integer()`)\cr
#'  Set the owners in `owner_ids` to the assets in `ids`. Both arguments must
#'  have an equal length. An asset can only be owned by one owner.
#'
#' * `get_owner_id_col()`\cr
#'  () -> `character(1)`.\cr
#'  Returns the id column of the owner object.
#'
#' * `get_owner_object()`\cr
#'  () -> an [R6::R6Class] object that inherits [Agent].\cr
#'  Returns the owner object of this asset.
#'
#' * `set_owner_object(x)`\cr
#'  (an [R6::R6Class] that inherits [Agent])\cr
#'  Set owner. This assigns the owner object to the `Owner` field of this
#'  asset hence you can access the owner object using `x$Owner` (assuming that
#'  the asset object was named `x`).
#'
#' * `remove_owner(ids)`\cr
#'  (`integer()`)\cr
#'  Remove the owner ids from the assets in `ids` by setting them to `NA`.
#'
#' * `owner_gets_asset_id(owner_ids)`\cr
#' (`integer()`) -> (`integer()`\cr
#'  Get the asset ids of the owners in `owner_ids`.
#'
#' @export
Asset <- R6::R6Class(
  classname = "Asset",
  inherit = Entity,
  public = list(

    initialize = function(.data, id_col, owner = NULL) {
      if (!missing(.data) & !missing(id_col)) {
        super$initialize(databackend = DataBackendDataTable,
                         .data = .data,
                         id_col = id_col)
      }
      if (!is.null(owner)) {
        self$set_owner_object(owner)
      }
      invisible()
    },

    initialise_data = function(.data, id_col, owner = NULL) {
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

    set_owner_object = function(x) {
      checkmate::assert_r6(x, classes = c("Agent", "Entity"))
      checkmate::assert_names(x = x$data()$colnames(),
                              must.include = self$get_id_col(),
                              .var.name = "Attribute data of owner 'x'")

      owner_id_col <- x$get_id_col()
      owner_ids_in_self <- na.omit(self$get_attr(x = owner_id_col))
      x_owner_ids <- x$get_ids()
      missing_ids <- owner_ids_in_self[!owner_ids_in_self %in% x_owner_ids]

      if (length(missing_ids) != 0) {
        stop(
          glue::glue(
            "Some {x$class()} owner ids in {self$class()} are missing from {x$class()}:\\
            {{missing_ids}}"
          )
        )
      }

      # if (!checkmate::test_subset(self$get_attr(x = owner_id_col),
      #                             choices = x$get_ids())) {
      #   stop(glue::glue("Some {x$class()} owner ids in {self$class()} are missing from {x$class()}"))
      # }
      lg$info("Setting owner of {class(self)[[1]]} to {class(x)[[1]]}")
      private$.Owner <- x
      private$.owner_id_col <- owner_id_col
      invisible()
    },

    get_owner_object = function() {
      private$.Owner
    },

    set_owner = function(ids, owner_ids) {

      # check that both self and owner are both available.
      if (!all(!self$is_owned(ids))) {
        err_msg <-
          glue("Not all assets in 'ids' are available.")
        stop(err_msg)
      }

      if (!all(is.na(self$owner_gets_asset_id(owner_ids)))) {
        err_msg <-
          glue("Not all owners in `owner_ids` are available.")
        stop(err_msg)
      }

      # self sets
      self_idx <- self$get_idx(ids)
      data.table::set(
        self$get_data(copy = FALSE),
        i = self_idx,
        j = self$get_owner_id_col(),
        value = owner_ids
      )

      # owner sets
      owner <- self$get_owner_object()
      owner_idx <- owner$get_idx(ids)
      data.table::set(
        self$get_owner_object()$get_data(copy = FALSE),
        i = owner_idx,
        j = self$get_id_col(),
        value = ids
      )

      invisible()
    },

    get_owner = function(ids) {
      if (!missing(ids)) {
        self$check_ids(ids)
        return(self$get_attr(x = self$get_owner_id_col(), ids = ids))
      }
      self$get_attr(x = self$get_owner_id_col())
    },

    get_owner_id_col = function() {
      if (is.null(private$.owner_id_col)) {
        lg$warn("owner id column is NULL, this is likely that the owner object of \\
                this asset has not been set.")
      }
      private$.owner_id_col
    },

    is_owned = function(ids) {
      !is.na(self$get_owner(ids))
    },

    remove_owner = function(ids) {
      owner_ids <- self$get_owner(ids)
      if (anyNA(owner_ids)) {
        stop("All assets in `ids` must have owners.")
      }
      owner <- self$get_owner_object()
      owner$check_ids(owner_ids)

      # self removes
      self_idx <- self$get_idx(ids)
      data.table::set(
        self$get_data(copy = FALSE),
        i = self_idx,
        j = self$get_owner_id_col(),
        value = NA_integer_
      )
      # owner removes
      owner_idx <- owner$get_idx(owner_ids)
      data.table::set(
        owner$get_data(copy = FALSE),
        i = owner_idx,
        j = self$get_id_col(),
        value = NA_integer_
      )
      invisible()
    },

    owner_gets_asset_id = function(owner_ids) {
      owner <- self$get_owner_object()
      owner$get_attr(x = self$get_id_col(), ids = owner_ids)
    }
  ),

  private = list(
    .Owner = NULL, # a place holder for the owner object.
    .owner_id_col = NULL
  )
)
