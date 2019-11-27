#' @title Building Residential class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include Asset.R
#'
#' @description
#' For storing methods and fields related to residential buildings.
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
BuildingResidential <- R6Class(
  classname = "BuildingResidential",
  inherit = Building,
  public = list(

    # minimum required data structure
    data_template = function() {
      data.table(
        # the first column should always be the unique id column of the class
        bid = integer(), # dwelling id
        zid = integer(), # zone id
        occupied_by = integer(), # household id, if is emptied then -1
        price = double(), # price of dwelling
        bedroom = integer(),
        bathroom = integer(),
        parking = integer(),
        type = character(),
      )
    }
  ),
  private = list()
)
