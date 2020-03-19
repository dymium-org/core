#' @title Agent class
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Entity].
#' @include Entity.R
#'
#' @description
#' Agent class is the generic class for creating urban actors such
#' as [Individual], [Household], firms, [Building] and more.
#'
#'
#' @section Construction:
#' ```
#' Agt <- Agent$new(...)
#' ```
#'
#' @section Fields:
#'
#'  * `data`::`data.table::data.table()`\cr
#'  Contain the attributes of agents. One of the attributes must be a unique id field.
#'
#' @section Methods:
#'
#'  * `add_new_agents(parent_ids = NULL, data = NULL)`\cr
#'  (`integer()`|`character()`, [data.table::data.table()]) -> `NULL`\cr
#'  Add new agent records to data.
#'
#'  * `get_latest_agent_id(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Return the id of the last agent that was created.
#'
#'  * `get_new_agent_ids()`\cr
#'  () -> `integer()`\cr
#'  Return the ids of new agents.
#'
#'  * `is_alive(ids)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Check if the agents with id in `ids` are still alive/active in its data object.
#'  or not.
#'
#'  * `generate_new_ids(n)`\cr
#'  (`integer(1)`) -> `integer()`\cr
#'  Generates `n` number of new ids.
#'
#'  * `hatch(ids)`\cr
#'  (`integer()`)\cr
#'  Clone existing agents in `ids` and inherit all attributes from their parents
#'  except for the id attribute.
#'
#'  * `add(.data)`\cr
#'  Add new agents in `.data` to the existing pool of agents. It should be noted,
#'  that all new agents must have all the columns that the existing agents possess
#'  with exception of derived variables which should be denoted by a `.` prefix.
#'  For example, `.past_partner_id` is a derived variable which is allowed to be
#'  missing in the new agents' data.
#'
#' @export
Agent <- R6Class(
  classname = "Agent",
  inherit = Entity,
  public = list(
    # public ------------------------------------------------------------------
    initialize = function(.data, id_col) {
      super$initialize(databackend = DataBackendDataTable,
                       .data = .data,
                       id_col = id_col)
    },

    hatch = function(parent_ids) {
      assert_entity_ids(self, parent_ids)
      idx_of_parent_ids <- self$get_idx(ids = parent_ids)
      newdata <- self$get_data(copy = TRUE)[idx_of_parent_ids,]
      new_ids <- self$generate_new_ids(n = nrow(newdata))
      newdata[, c(self$get_id_col()) := new_ids]
      self$data()$add(.data = newdata)
      return(invisible(self))
    },

    subset_ids = function(expr) {
      j_expr <- substitute(expr)
      subset(x = self$get_data(copy = FALSE), subset = eval(j_expr))[[self$get_id_col()]]
    },

    get_latest_agent_id = function() {
      self$get_last_id()
    },

    get_new_agent_ids = function() {
      self$get_new_ids()
    },

    is_alive = function(ids) {
      checkmate::assert_integerish(ids, any.missing = FALSE, lower = 1)
      return(ids %in% self$get_ids())
    }
  ),
  private = list(
    # private -----------------------------------------------------------------
    # start from 0
    options = list(verbose = TRUE)
  )
)
