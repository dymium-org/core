#' History class
#'
#' Create History class, record life-course changes of agents.
#' @usage NULL
#' @format NULL
#' @include Generic.R
#' @examples
#' h <- History$new()
#' h$add(
#'   id = sample(1L:10L, 20, replace = TRUE),
#'   time = sample(1L:4L, 20, replace = TRUE),
#'   event = sample(c("Birth", "LeaveHome", "Marriage"), 20, replace = TRUE))
#' @export
#' @section Public methods:
History <- R6::R6Class(
  'History',
  inherit = Generic,

# public ------------------------------------------------------------------

  public = list(
    initialize = function() {
      private$init_data()
    },

    # ***********************************************************
    #' \code{add(id, time, event)} -
    #'   add changes (`event`) of agents (`id`) with a timestamp (`time`)
    #'
    # ***********************************************************
    add = function(id, event, time = .get_sim_time()) {
      if (length(id) == 0) {
        return(invisible())
      }

      assert_that(is.numeric(id))
      assert_that(is.numeric(time))
      assert_that(is.character(event))
      if (!is.integer(id)) {
        id <- as.integer(id)
      }
      if (!is.integer(time)) {
        time <- as.integer(time)
      }

      tmp_data <-
        # put into a data.table
        data.table(id = id, time = time, event = event) %>%
        # reshape the id column into a list column
        .[, .(id = .(id)), by = .(time, event)] %>%
        .[, record_time := Sys.time()]

      # add new records to private$data
      private$data <- rbind(private$data, tmp_data)
      # reorder rows
      setorder(private$data, time, event)
    },

    # ***********************************************************
    #' \code{count(ids = NULL, event = NULL)} -
    #'   Returns the number of occurence (N) of events for each id.
    #'
    # ***********************************************************
    count = function(ids = NULL, event = NULL) {
      # avoid naming conflicts
      # due to scoping issue as pointed out in this SO thread
      # https://tinyurl.com/y26yb5hn we have to assign the args
      # to variables with different names to the existing names
      # of the columns in private$data
      filter_id <- ids
      filter_event <- event
      rm(ids, event)

      dat <- self$get_data()
      existing_events <- dat[, unique(event)]

      # filter for only selected events
      if (!is.null(filter_event)) {
        assert_that(is.character(filter_event))
        assert_that(all(filter_event %in% existing_events))
        dat <- dat[event %in% filter_event, ]
      }

      # filter only selected ids
      if (!is.null(filter_id)) {
        assert_that(is.numeric(filter_id))
        dat <- dat[purrr::map_lgl(id, ~ any(.x %in% filter_id)), ]
      }

      # return counts by id and event
      res <- dat %>%
        # unlist the id list column: id, time, event
        .[, lapply(.SD, unlist), by = 1:nrow(dat)] %>%
        .[, nrow := NULL] %>%
        .[, .N, by = .(id, event)]

      # return only filtered id
      if (!is.null(filter_id))
        return(res[id %in% filter_id, ])

      res
    },

    # ***********************************************************
    #' \code{inspect(ids)} -
    #'   Return lifecourse history of the agents in ids by time.
    #'
    # ***********************************************************
    inspect = function(ids = NULL) {
      assert_that(all(is.numeric(ids)))

      # filter all records that contains ids
      history_data <- self$get_data() %>%
        .[purrr::map_lgl(id, ~ any(.x %in% ids)), ]

      if (nrow(history_data) == 0){
        return(history_data[0, ])
      }

      # expand the records by each row
      history_data <- history_data[, lapply(.SD, unlist), by = 1:nrow(history_data)] %>%
        # filter selected ids
        .[id %in% ids, ] %>%
        # drop the group by column
        .[, nrow := NULL] %>%
        .[order(record_time), ]

      history_data
    },

    # ***********************************************************
    #' \code{impute(data, id_col, event = NULL)} -
    #'   Return the original data imputed with the number of occurence
    #'   of the event specified in `event` arg. `id_col` is used to
    #'   merge the output of `self$count()` with data.
    #'
    # ***********************************************************
    impute = function(data, id_col, event = NULL) {
      assert_that(is.data.table(data))
      assert_that(is.character(id_col))
      assert_that(id_col %in% colnames(data))
      assert_that(is.character(event))

      count <- self$count(event = event)
      count[data, on = paste0("id==", id_col), nomatch = NA]
    },

    # ***********************************************************
    #' \code{debug()} -
    #'   Makes debugging the object easier by assigning self and
    #'   private to the global environment of the current seesion.
    #'
    # ***********************************************************
    debug = function(pos = 1) {
      assign("self", self, envir = as.environment(pos))
      assign("private", private, envir = as.environment(pos))
      self$message("[self] and [private] assigned to global environment")
      # return self for piping
      return(invisible(self))
    },

    # ***********************************************************
    #' \code{get_data(copy = TRUE)} -
    #'   return a copy of `private$data`. Since `private$data` is
    #'   a data.table object, if `copy` is set to FALSE a reference
    #'   to `private$data` will be returned.
    #'
    # ***********************************************************
    get_data = function(copy = TRUE) {
      if (!copy) {
        private$data
      } else {
        data.table::copy(private$data)
      }
    },

    # ***********************************************************
    #' \code{message()} -
    #'   Print `message` onto the console with the object name
    #'   append in front of the message.
    #'
    # ***********************************************************
    message = function(x, ...) {
      xname <- as.character(as.list(match.call()))[2]
      if (TRUE) {
        if (is.character(x)) {
          message(class(self)[1], " : ", x, ...)
        } else{
          message(class(self)[1], " : ", xname, " : \n", x)
        }
      }
    }
  ),
  private = list(

# private -----------------------------------------------------------------
#' @section Private methods:

    data = data.table(),

    # ***********************************************************
    #' \code{init_data()} -
    #'   Initialise data in the correct structure.
    #'
    # ***********************************************************
    init_data = function() {
      private$data <- private$data_template()
    },

    # ***********************************************************
    #' \code{data_template()} -
    #'   Create a data template which mimics the structure and
    #'   column types of private$data.
    #'
    #'   **NOTE**: event_as_list can take up a lot of memory since
    #'   all event entries will be converted to lists.
    #'
    # ***********************************************************
    data_template = function() {
      data.table(
        time = integer(),
        event = character(),
        id = list(),
        record_time = as.POSIXct(character())
      )
    }
  )
)
