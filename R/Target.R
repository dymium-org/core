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
#' Target$new(x, name)
#' ```
#'
#' * `x` :: any object that passes `check_target()`\cr
#' A target object or `NULL`.
#'
#' * `name` :: `character(1)`\cr
#' Name/Alias of the Target object. This will be used as the [Target] name when
#' it gets added to a [World].
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
#'  Get an alignment target as a named list. Note that, all the elements in the list
#'  will be scaled by multiplying with the value stored in getOption("dymium.simulation_scale")
#'  and all the values in the output are rounded to their nearest integers.
#'
#' @aliases Targets
#' @export
#'
#' @examples
#'
#' # static target
#' TrgtStatic <- Target$new(x = list(yes = 10))
#' TrgtStatic$data
#' TrgtStatic$dynamic
#' TrgtStatic$get()
#'
#' # dynamic target
#' target_dynamic <- data.frame(time = 1:10, yes = 1:10)
#' TrgtDynamic <- Target$new(x = target_dynamic)
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
  inherit = Generic,
  public = list(
    initialize = function(x, name) {
      assert_target(x, null.ok = TRUE)
      if (is.data.frame(x)) {
        if (!"time" %in% names(x)) {
          stop(
            paste("A column named `time` is missing in `x`. Note that,",
                  "data.frame should be used when creating a dynamic target",
                  "only. If you want to create a static target use `list` instead.")
          )
        }
        private$.dynamic <- TRUE
        if (!is.data.table(x)) {
          private$.data <- as.data.table(x)
        } else {
          private$.data <- data.table::copy(x)
        }
      } else {
        private$.data <- x
      }
      super$initialize(name = name)
      invisible(self)
    },

    get = function(time = .get_sim_time()) {
      if (private$.dynamic) {
        closest_time_index <- which.min(abs(private$.data[['time']] - time))
        return(lapply(private$.data[closest_time_index, -c("time")], function(x) round(x * getOption("dymium.simulation_scale"))))
      }
      if (is(object = private$.data, class2 = "list")) {
        return(lapply(private$.data, function(x) round(x * getOption("dymium.simulation_scale"))))
      }
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
      if (is.data.table(private$.data)) {
        return(data.table::copy(private$.data))
      }
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
