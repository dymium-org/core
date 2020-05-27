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
#' @section Construction:
#'
#' ```
#' x <- Environment$new()
#' ```
#'
#' * NULL\cr
#'
#' @section Fields:
#'
#' * `NULL`\cr`
#'
#' @section Methods:
#'
#' * NULL
#'
#' @export
Environment <-
  R6::R6Class(
    classname = "Environment",
    inherit = Entity,
    public = list(

      initialize = function(.data, id_col) {
        super$initialize(databackend = DataBackendSpatialFeature,
                         .data = .data,
                         id_col = id_col)
      }
  ))
