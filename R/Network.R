#' @title Network class
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Environment]<-[Entity].
#'
#' @description  Network class. For creating a road network etc.
#'
#' @include Environment.R
#'
#' @section Construction:
#'
#' ```
#' x <- Network$new()
#' ```
#'
#' * .data\cr
#' A linestring *sf* data.
#'
#' * id_col\cr
#' The name of the id column of `.data`.
#'
#' * from_node\cr
#' The name of the column that contains starting nodes.
#'
#' * to_node\cr
#' The name of the column that contains terminating nodes.
#'
#' * dist\cr
#' The name of the column that contains lengths or the distances of the edges.
#'
#' Note that each row of the input data in `.data` must contain a linestring
#' or a multilinestring feature which basically an edge of a graph.
#'
#' @note
#'
#' Network has a DataBackendDataTable object named "dodgr_graph" that basically
#' transform its attribute data into a *dodgr* graph object. This is particularlly
#' useful for path finding applications etc.
#'
#' @section Fields:
#'
#' * `NULL`\cr`
#'
#' @section Methods:
#'
#' * `get_dodgr_graph(ids)`\cr
#'  () -> [data.table::data.table()]\cr
#'  Returns a dodgr graph of the Network. This is just a shorthand for
#'  `Network$get_data(name = "dodgr_graph)`.
#'
#' @export
Network <- R6::R6Class(
  classname = "Network",
  inherit = Environment,
  public = list(
    initialize = function(.data, id_col, from_node, to_node, dist) {
      super$initialise_data(.data, id_col)
      self$add_data(.data = private$generate_dodgr_graph(from_node, to_node, dist),
                    name = "dodgr_graph")
      invisible()
    },

    get_dodgr_graph = function() {
      self$get_data("dodgr_graph")
    }
  ),
  private = list(
    generate_dodgr_graph = function(from_node, to_node, dist) {
      checkmate::assert_data_table(self$get_data())
      checkmate::assert_names(names(self$get_data()),
                              must.include = c(from_node, to_node, dist))
      graph <-
        self$get_data()[, .SD, .SDcols = c(self$get_id_col(), from_node, to_node, dist)]
      names(graph) <- c(self$get_id_col(), "from", "to", "dist")
      return(graph)
    }
  )
)
