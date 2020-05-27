#' @title Industrial building class
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Building]<-[Asset]<-[Entity].
#'
#' @description  Create dwelling class, extended Agent class.
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
#' * `is_vacant(ids)`\cr
#'   (`integer()`) -> `logical()`\cr
#'   Returns TRUE if dwelling in ids is vacant.
#'
#' @export
BuildingIndustrial <- R6Class(
  classname = "BuildingIndustrial",
  inherit = Building,
  public = list(),
  active = list(
    data_template = function() {
      data.table(
        # the first column should always be the unique id column of the class
        did = integer(), # dwelling id
        zid = integer(), # zone id
        occupied_by = integer(),
        price = double()
      )
    }
  ),
  private = list()
)
