#' @title Target
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object inheriting [Generic].
#'
#' @description
#'
#' `Target` is to be used within `TransitionClassification` or supply to event
#' functions. If the target is dynamic then its `get` will return its target
#' value at the current time or its closest time to the current time.
#'
#' @section Construction:
#'
#' ```
#' Target$new(x)
#' ```
#'
#' * `x` :: any object that passes `check_target()`\cr
#' A target object.
#'
#' @section Active Field (read-only):
#'
#'  * `data`:: a target object\cr
#'  A target object.
#'
#'  * `dynamic`:: `logical(1)`\cr
#'  A logical flag which indicates whether the target object is dynamic or not.
#'
#' @section Public Methods:
#'
#'  * `get(time = .get_sim_time())`\cr
#'  (`integer(1)`) -> a named `list()`\cr
#'  Get a alignment target as a named list.
#'
#' @aliases Models
#'
#' @examples
#'
#' # static target
#' TrgtStatic <- Target$new(list(yes = 10))
#' TrgtStatic$data
#' TrgtStatic$dynamic
#' TrgtStatic$get()
#'
#' # dynamic target
#' target_dynamic <- data.table(time = 1:10, yes = 1:10)
#' TrgtDynamic <- Target$new(list(yes = 10))
#' TrgtDynamic$data
#' TrgtDynamic$dynamic
#'
#' # if the `time` argument in `get()` is not specified then it will rely on
#' # the time step from the simulation clock from `.get_sim_time()`.
#' TrgtDynamic$get()
#' TrgtDynamic$get(1)
#' TrgtDynamic$get(10)
Target <- R6::R6Class(
  classname = "Target",
  inherit = dymiumCore::Generic,
  public = list(
    initialize = function(x) {
      assert_target(x, null.ok = FALSE)
      if (is.data.frame(x)) {
        if (!is.data.table(x)) {
          private$.data <- as.data.table(x)
        } else {
          private$.data <- data.table::copy(x)
        }
        if ("time" %in% names(x)) {
          private$.dynamic <- TRUE
        }
      }
      private$.data <- x
      return(invisible(self))
    },

    get = function(time = .get_sim_time()) {
      if (private$.dynamic) {
        closest_time_index <- which.min(abs(private$.data[['time']] - time))
        return(as.list(private$.data[closest_time_index, -c("time")]))
      }
      if (is.data.table(private$.data)) {
        return(copy(private$.data))
      }
      return(private$.data)
    },

    print = function() {
      msg <- glue::glue("dynamic: {private$.dynamic}")
      if (private$.dynamic) {
        period <- c(min(private$.data[["time"]]),
                    max(private$.data[["time"]]))
        msg <- glue::glue(msg,
                          "period: {period[1]} to {period[2]}", .sep = "\n- ")
      }
      super$print(msg)
    }
  ),

  active = list(
    data = function() {
      base::get(".data", envir = private)
    },
    dynamic = function() {
      base::get(".dynamic", envir = private)
    }
  ),

  private = list(
    .data = NULL,
    .dynamic = FALSE
  )

)
