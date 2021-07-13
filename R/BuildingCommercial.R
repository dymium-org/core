#' Commercial Building class
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Building]<-[Asset]<-[Entity].
#'
#' @description  Create Commercial Building class, extended Agent class.
#'
#' @section Fields:
#'
#' * `NULL`\cr`
#'
#' @section Methods:
#'
#' * `is_occupied(ids)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Returns TRUE if dwelling in ids is occupied
#'
#' @export
BuildingCommercial <- R6Class(
  classname = "BuildingCommercial",
  inherit = Building,
  public = list(),
  active = list(
    # minimum required data structure
    data_template = function() {
      return(
        data.table(
          # the first column should always be the unique id column of the class
          did = integer(), # dwelling id
          zid = integer(), # zone id
          occupied_by = integer(),
          price = double() # price of dwelling
        )
      )
    }
  ),
  private = list()
)
