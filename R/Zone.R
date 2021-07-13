#' @title Zone class
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Environment]<-[Entity].
#'
#' @description  Create Zone class, extended Agent class.
#'
#' @include Environment.R
#'
#' @export
Zone <- R6::R6Class(
  classname = "Zone",
  inherit = Environment,
  public = list()
)
