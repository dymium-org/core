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
#'  * `hatch(ids)`\cr
#'  (`integer()`)\cr
#'  Clone existing agents in `ids` and inherit all attributes from their parents
#'  except for the id attribute.
#'
#'  * `add(.data)`\cr
#'  Add new agents in `.data` to the existing pool of agents. It should be noted,
#'  that all new agents must have all the columns that the existing agents possess
#'  with the exception of derived variables which is denoted by the `.` prefix.
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
      if (!missing(.data)) {
        if (!missing(id_col)) {
          super$initialize(databackend = DataBackendDataTable,
                           .data = .data,
                           id_col = id_col)
        } else {
          stop(glue::glue("To initialise the attribute data of {self$class()} \\
                            the `id_col` argument must be given."))
        }
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

      if (!xor(missing(parent_ids), missing(.data))) {
        stop("Only one of the arguments must be specified")
      }

      if (self$n() == 0) {
        stop(glue(
          "data is emptied! Can't not create new agents since the data fields \\
          are not defined."
        ))
      }

      if (!missing(.data)) {
        checkmate::assert_data_frame(.data)
        private$add_new_agent_data(newdata = .data)
        return()
      }

      if (!missing(parent_ids)) {
        if (!self$ids_exist(ids = parent_ids, by_element = FALSE)) {
          stop(glue("Not all parent_ids exists."))
        }
        private$add_new_agent_inherit_parent(parent_ids = parent_ids)
        return()
      }

      stop("Something went wrong! please try to debug this or file an issue.")
    },

    hatch = function(ids) {
      ids_dont_exist <- ids[!self$ids_exist(ids, by_element = TRUE)]
      if (length(ids_dont_exist) != 0) {
        stop(glue(
          "Assertion on 'ids' failed: These ids do not exists {{{.missing_ids}}}",
          .missing_ids = glue_collapse(ids_dont_exist, sep = ", ")
        ))
      }
      private$add_new_agent_inherit_parent(parent_ids = ids)
    },

    add = function(.data) {
      checkmate::assert_data_frame(.data)
      private$add_new_agent_data(newdata = .data)
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
      checkmate::assert_data_table(newdata, col.names = 'strict', null.ok = FALSE)

      # check that both data are identical in their structures
      check_res <- all.equal(target = omit_derived_vars(self$get_data(copy = FALSE)[0, ]),
                             current = omit_derived_vars(newdata[0, ]))
      if (!isTRUE(check_res)) {
        lg$error("showing head of self$data")
        print(head(self$get_data()))
        lg$error("showing head of newdata")
        print(head(newdata))
        stop(glue(
          "Existing data of {self$class()} and `newdata` are not the same for the following reasons:
          {reasons}",
          reasons = glue_collapse(paste("- ", check_res), sep = "\n")
        ))
      }

      id_col <- self$get_id_col()
      # check id uniqueness
      checkmate::assert_integerish(
        x = newdata[[id_col]],
        len = nrow(newdata),
        lower = 1,
        null.ok = FALSE,
        any.missing = FALSE,
        .var.name = "newdata's id column"
      )

      # check id overlapping between new and old data
      if (self$ids_exist(newdata[[id_col]], by_element = FALSE)) {
        stop("Not all ids in `data` are unique from ids in newdata")
      }

      # if pass all the checks then bind to data
      self$data()$add(.data = newdata, fill = TRUE)

      invisible()
    }
  )
)
