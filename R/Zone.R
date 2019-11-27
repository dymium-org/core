#' @title Zone class
#'
#' @format [R6::R6Class] object inheriting from [Agent].
#'
#' @description  Create Zone class, extended Agent class.
#'
#' @include Environment.R
#' @export
Zone <- R6::R6Class(
  classname = "Zone",
  inherit = Environment,
  public = list(

    data_template = function() {
      data.table(
        zid = integer(),
        zname = character()
      )
    },

    initialise_data = function(data, id_col = "zid") {
      super$initialise_data(data, id_col)
      invisible()
    }

  )
)
