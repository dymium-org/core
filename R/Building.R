#' @title Building class
#' @format [R6::R6Class] object inheriting from [Asset]<-[Entity].
#' @include Asset.R
#'
#' @description
#' Create a building object.
#'
#' ```
#' x <- Building$new(.data, id_col, owner)
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
#' * `is_vacant(ids)`\cr
#'  (`integer()`) -> (`logical()`)\cr
#'  Check if the assets in ids are vacant
#'
#' * `vacate(ids)`\cr
#'  (`integer()`)\cr
#'  This removes the owners of the assets in `ids`.
#'
#' @export
Building <- R6Class(
  classname = "Building",
  inherit = Asset,
  public = list(
    is_vacant = function(ids) {
      !super$is_owned(ids)
    },
    vacate = function(ids) {
      super$remove_owner(ids)
    }
  )
)
