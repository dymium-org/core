#' @title Household class
#'
#' @format [R6::R6Class] object inheriting from [Agent]<-[Entity].
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

      # add data
      if (!missing(.data)) {
        super$add_new_agents(.data = .data)
        return(invisible())
      }

      # generate by n
      checkmate::assert_count(n, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
      new_ids <- self$generate_new_ids(n = n)
      id_col <- self$get_id_col()
      newdata <-
        data.table(id_col = new_ids) %>%
        setnames(old = "id_col", new = id_col) %>%
        rbind(., self$get_data(copy = FALSE)[0, ], fill = TRUE)
      super$add_new_agents(.data = newdata)
      invisible()
    },

      super$add_entities(.data = .data)
      invisible()
    }
  )
)
