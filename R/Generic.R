#' @title Generic class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Can be used as a template for other objects. This contains useful utility
#' functions for abstraction objects. See the public members (methods).
#'
#' @section Construction:
#' ```
#' Obj <- Generic$new(...)
#' ```
#'
#' @section Fields:
#'
#'  * `NULL`
#'
#' @section Methods:
#'
#' * `debug()`\cr
#' () -> `self & private`\cr
#' Makes debugging the object easier by assigning self and private to the
#' global environment of the current seesion.
#'
#' * `log(desc, value, tag = "", time = .get_sim_time(), .lg = parent.frame()[["lg"]])`\cr
#' (`character(1)`, any objects, `character(1)`, `integer(1)`, [lgr::Logger])\cr
#' For logging simulation outcomes to `private$.log`. `get_log(x)` can be used to
#' extract the log data from any object that inherit `Generic`. Note that, the
#' logger in `log` can be specified. By default, it tries to look in the `parent.frame`
#' the calling scope to find a logger named `lg` which is the default name used in
#' dymium modules. If not found, then it will automatically use dymiumCore's logger.
#'
#'
#' * `is_dymium_class()`\cr
#' Returns `TRUE`. Use for internal checking.
#'
#' @section Private field:
#'
#' * `.log`\cr
#' A data.table for storing logs. There are 6 columns: time (integer),
#' created_timestamp(integer), class (character), tag (character), desc (character),
#' value (list). The `value` column is a list column gives it a flexibility to store
#' any types of objects.
#'
#' @export
Generic <- R6Class(
  classname = "Generic",
  public = list(
    initialize = function(...) {

    },
    debug = function() {
      pos <- 1
      assign("self", self, envir = as.environment(pos))
      assign("private", private, envir = as.environment(pos))
      lg$warn("[self] and [private] assigned to global environment")
      # return self for piping
      return(invisible(self))
    },
    log = function(desc, value, tag = NA_character_, time = .get_sim_time(), .lg = parent.frame()[["lg"]]) {
      checkmate::assert_string(desc, null.ok = FALSE, na.ok = FALSE)
      checkmate::assert_vector(value, any.missing = FALSE, null.ok = FALSE)
      checkmate::assert_string(tag, null.ok = FALSE, na.ok = TRUE)
      checkmate::assert_integerish(time, lower = 0, len = 1, null.ok = FALSE)
      .caller <- lgr::get_caller(-2)
      if (is.null(.lg)) {
        .lg <- lg
      }
      if (length(value) != 1) {
        .lg$info("{desc}: {.value}",
          caller = .caller,
          .value = glue::glue_collapse(value, sep = ", ", width = 100)
        )
      } else {
        .lg$info("{desc}: {value}", caller = .caller)
      }

      lg$trace("logging to private$.log")

      .log <- data.table(
        time = time,
        created_timestamp = as.integer(Sys.time()),
        class = self$class(),
        tag = tag,
        desc = desc,
        value = list(value)
      )

      private$.log <- rbind(private$.log, .log)

      invisible()
    },
    is_dymium_class = function() {
      TRUE
    },
    class = function() {
      class(self)[[1]]
    },
    print = function(...) {
      dots <- list(...)
      .class_inheritance <- glue::glue_collapse(class(self), sep = " <- ")

      if (length(dots) != 0) {
        message(
          glue::glue(
            "Class: {class(self)[[1]]}",
            "Inheritance: {.class_inheritance}",
            "{dots[[1]]}",
            .sep = "\n- "
          )
        )
      } else {
        message(
          glue::glue(
            "Class: {class(self)[[1]]}",
            "Inheritance: {.class_inheritance}",
            .sep = "\n- "
          )
        )
      }
    }
  ),
  private = list(
    .abstract = function(msg) {
      # this is a method for abstract methods
      if (!missing(msg)) {
        lg$fatal(msg)
      }
      stop("This is an abstract method which is to be implemented.")
    },
    abstract = function(msg) {
      # this is a method for abstract methods
      if (!missing(msg)) {
        lg$fatal(msg)
      }
      stop("This is an abstract method which is to be implemented.")
    },
    .log = data.table(
      time = integer(),
      created_timestamp = integer(),
      class = character(),
      tag = character(),
      desc = character(),
      value = list()
    )
  )
)
