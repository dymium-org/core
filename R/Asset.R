#' @title Asset class
#'
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [Entity].
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
#' x <- Entity$new(databackend, .data, id_col)
#' ```
#'
#' Stores `.data` as a DataBackend object inside the object's list of data (`private$.data`)
#' and registers the `id_col` (`private$.id_col`).
#'
#' * `databackend` :: [DataBackend] classes\cr
#'   An [R6::R6Class] generator that inherits from `DataBackend`.
#'
#' * `.data` :: `data.frame`\cr
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
#' * `owner(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Returns owner ids.
#'
#' @export
Asset <- R6::R6Class(
  classname = "Asset",
  inherit = Entity,
  public = list(

    Owner = NULL, # a reference holder to the instance of the Owner class

    initialize = function(.data, id_col, owner_object) {
      if (!missing(.data) & !missing(id_col)) {
        super$initialize(databackend = DataBackendDataTable,
                         .data = .data,
                         id_col = id_col)
      }
      if (!missing(owner_object)) {
        self$set_owner(owner_object)
      }
      invisible()
    },

    set_owner = function(x) {
      checkmate::assert_r6(x, classes = c("Agent", "Entity"))
      checkmate::assert_names(x = x$data()$colnames(),
                              must.include = self$get_id_col(),
                              .var.name = "Attribute data of owner 'x'")
      if (!checkmate::test_subset(self$get_attr(x = x$get_id_col()),
                                  choices = x$get_ids())) {
        stop(glue::glue("Some {x$class()} owner ids in {self$class()} are missing from {x$class()}"))
      }
      lg$info("Setting owner of {class(self)[[1]]} to {class(x)[[1]]}")
      self$Owner <- x
      invisible()
    },

    get_owner_id_col = function() {

    },

    initialise_data = function(.data, id_col, owner_object) {
      if (length(private$.data) != 0) {
        stop("Agent's `attrs` data has already been initialised.")
      }
      self$initialize(.data = .data,
                      id_col = id_col,
                      owner_object = owner_object)
      self$generate_new_ids(n = max(.data[[private$.id_col]])) # keep track of latest agent id
      invisible()
    },

    get_owner = function(id) {
      private$abstract()
    }
  )
)
