#' @title Agent
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include Entity.R
#' @include History.R
#'
#' @description
#' Agent class is the generic class for creating urban agents such
#' as individuals, households, firms, dwelling and more.
#'
#'
#' @section Construction:
#' ```
#' Agt <- Agent$new(...)
#' ```
#'
#' During initialisation, [History] object is created.
#'
#' @section Fields:
#'
#'  * `history`\cr
#'
#'  * `data`::`data.table::data.table()`\cr
#'  Contains the attributes of agent. One of the attributes must be a unique id field
#'
#' @section Methods:
#'
#'  * **DEPRECATING** `initialise_data(data, id_col)`\cr
#'   ([data.table::data.table()], `character(1)`) -> `NULL`\cr
#'   Store data to data while making sure that `data` meets all the
#'   requirements. `data`, which is also the first set of data for the agent object,
#'   will be stored at the first index in `private$.data`, a named list of data objects, and named
#'   as `attrs`.
#'
#'  * `add_new_agents(parent_ids = NULL, data = NULL)`\cr
#'  (`integer()`|`character()`, [data.table::data.table()]) -> `NULL`\cr
#'  Adds new agent records to data.
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
#'  * `subset_ids(expression)`\cr
#'  (`expression`) -> `[data.table::data.table()]`\cr
#'  Return ids of rows matches the expression.
#'
#' @export
Agent <- R6Class(
  classname = "Agent",
  inherit = Entity,
  public = list(
    # public ------------------------------------------------------------------
    initialize = function(.data, id_col) {
      if (!missing(.data) & !missing(id_col)) {
        super$initialize(databackend = DataBackendDataTable,
                         .data = .data,
                         id_col = id_col)
      }
      invisible()
    },

    initialise_data = function(.data, id_col) {
      if (length(private$.data) != 0) {
        stop("Agent's `attrs` data has already been initialised.")
      }
      super$initialize(databackend = DataBackendDataTable,
                       .data = .data,
                       id_col = id_col)
      invisible()
    },

    add_new_agents = function(parent_ids, .data) {
      assert_that(xor(missing(parent_ids), missing(.data)),
        msg = "At least and at most of one of the arguments must be given.")
      assert_that(
        self$n() != 0,
        msg = "data is emptied!
        Can't not create new agents since the data fields are not defined."
      )

      if (!missing(.data)) {
        assert_that(is.data.frame(.data))
        private$add_new_agent_data(newdata = .data)
        return()
      }

      if (!missing(parent_ids)) {
        assert_that(self$ids_exist(ids = parent_ids, by_element = FALSE))
        private$add_new_agent_inherit_parent(parent_ids = parent_ids)
        return()
      }

      invisible()
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
      self$ids_exist(ids, by_element = TRUE)
    }
  ),
  private = list(
    # private -----------------------------------------------------------------
    # start from 0
    options = list(verbose = TRUE),

    # ***********************************************************
    # \code{add_new_agent_inherit_parent(parent_ids)} -
    #   As the name suggested, but the clone agents will have
    #   their unique ids not the same as their parents, as
    #   specify in `parent_ids` arg.
    #
    # ***********************************************************
    add_new_agent_inherit_parent = function(parent_ids) {
      idx_of_parent_ids <- self$get_idx(ids = parent_ids)
      newdata <- self$get_data(copy = TRUE)[idx_of_parent_ids,]
      new_ids <- self$generate_new_ids(n = nrow(newdata))
      newdata[, c(self$get_id_col()) := new_ids]
      self$data()$add(.data = newdata)
      invisible()
    },

    # ***********************************************************
    # \code{add_new_agent_data(newdata)} -
    #   `newdata` will get added to data if it passes
    #   all the compatibility checks: data type, matching colnames,
    #   column data types, ids are unique.
    #
    # ***********************************************************
    add_new_agent_data = function(newdata) {
      assert_that(is.data.table(newdata),
        msg = "`data` must be a data.table.")

      # check that both data are identical in their structures
      if (all.equal(self$get_data(copy = FALSE)[0, ], newdata[0, ]) != TRUE) {
        lg$error("showing head of self$data")
        print(head(self$get_data()))
        lg$error("showing head of newdata")
        print(head(newdata))
        stopifnot(all.equal(self$get_data(copy = FALSE)[0, ], newdata[0, ]), )
      }

      # check id uniqueness
      assert_that(uniqueN(newdata[[self$get_id_col()]]) == nrow(newdata),
        msg = "All ids must be unique.")

      # check id overlapping between new and old data
      assert_that(!self$ids_exist(newdata[[self$get_id_col()]], by_element = FALSE),
        msg = "not all ids in `data` are unique from ids in newdata")

      # if pass all the checks then bind to data
      self$data()$add(.data = newdata)

      invisible()
    }
  )
)
