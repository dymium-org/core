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
#'  * `add(.data, n)`\cr
#'   ([data.table::data.table], `integer(1)`) -> `NULL`\cr
#'   Add new households using data or generate `n` emptied households. If `data`
#'   is given `n` will be ignored.
#'
#' @family Agent
#'
#' @export
Household <- R6Class(
  "Household",
  inherit = Agent,
  public = list(

    add = function(.data, n){
      # generate by n
      if (!xor(missing(.data), missing(n))) {
        stop("either `.data` or `n` should be given. But never both.")
      }

      if (!missing(n)) {
        checkmate::assert_count(n, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
        new_ids <- self$generate_new_ids(n = n)
        id_col <- self$get_id_col()
        .data <-
          data.table(id_col = new_ids) %>%
          setnames(old = "id_col", new = id_col) %>%
          rbind(., self$get_data(copy = FALSE)[0, ], fill = TRUE)
      }

      super$add(.data = .data)
      invisible()
    }
  )
)
