#' @title Household class
#'
#' @format [R6::R6Class] object inheriting from [Agent].
#' @include Agent.R
#' @include Individual.R
#'
#' @description  Create Household class, extended Agent class.
#'
#' @section Construction:
#' ```
#' Household$new()
#' ```
#'
#' @section Methods:
#'
#'  Inherits all methods in [Agent].
#'
#' @section Overwritten methods:
#'
#'  * `add_new_agents(data, n)`\cr
#'   ([data.table::data.table()], `integer(1)`) -> `NULL`\cr
#'   Add new households using data or generate `n` emptied households. If `data`
#'   is given `n` will be ignored.
#'
#'  * `initialise_data(data, id_col)`\cr
#'   ([data.table::data.table()], `character(1)`) -> `NULL`\cr
#'   Store data to data while making sure that `data` meets all the
#'   requirements.
#'
#' @family Agent
#'
#' @export
Household <- R6Class(
  "Household",
  inherit = Agent,
  public = list(

    add_new_agents = function(.data, n){
      if (!missing(.data)) {
        super$add_new_agents(.data = .data)
        return(invisible())
      }
      assert_that(assertthat::is.count(n))
      data_template <- self$get_data(copy = FALSE)[0, ]
      new_ids <- self$generate_new_ids(n = n)
      newdata <-
        # all undefined columns will be filled with NAs
        rbind(data_template, data.table::data.table(hid = new_ids), fill = TRUE)
      super$add_new_agents(.data = newdata)
      invisible()
    },

    initialise_data = function(.data, id_col = "hid") {
      super$initialise_data(.data = .data, id_col = id_col)
    }#,

    # data_template = function() {
    #   data.table(
    #     hid = integer(),
    #     hhsize = integer()
    #     )
    # }
  )
)
