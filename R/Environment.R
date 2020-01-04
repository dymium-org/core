#' @title Environment class
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Entity].
#' @include Entity.R
#'
#' @description
#' The base class for physical environment, infrastructure and boundaries. This
#' is different from Asset as Environment has no owner or belongs to the public.
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
#' * `ownerclass(ids)`\cr
#'  () -> `character(1)`\cr
#'  Returns owner's class.
#'
#' @export
Environment <-
  R6::R6Class(
    classname = "Environment",
    inherit = Entity,
    public = list(

      initialize = function(.data, id_col) {
        if (!missing(.data)) {
          if (!missing(id_col)) {
            super$initialize(databackend = DataBackendSpatialFeature,
                             .data = .data,
                             id_col = id_col)
          } else {
            stop(glue::glue("To initialise the attribute data of {self$class()} \\
                            the `id_col` argument must be given."))
          }
        }

        invisible()
      },

      initialise_data = function(.data, id_col) {
        if (length(private$.data) != 0) {
          stop("Agent's `attrs` data has already been initialised.")
        }
        super$initialize(databackend = DataBackendSpatialFeature,
                         .data = .data,
                         id_col = id_col)
        self$generate_new_ids(n = max(.data[[private$.id_col]])) # keep track of latest agent id
        invisible()
      }
    )
  )
