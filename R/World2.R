#' @title World Class
#'
#' @include Container.R
#'
#' @template param_world
#'
#' @description
#'
#' The first step to creating a microsimulation model with `Dymium` is to define
#' what the "world" should look like. Your world can have multiple entities and
#' properties which define the rules and environment that govern those entities.
#'
#' @section S3 methods:
#' * `as.data.table(t)`\cr
#'   [Task] -> [data.table::data.table()]\cr
#'   Returns the complete data as [data.table::data.table()].
#'
#' @export
#' @examples
#' world = World2$new()
World2 =
  R6::R6Class(
    classname = "World2",
    public = list(

      #' @field entities (named `list()`)\cr
      #'   A named list that contains [Entities] instances.
      entities = list(),
      #' @field properties (named `list()`)\cr
      #'   A named list that contains [Targets], [Models], and other values to
      #'   be used in your simulation.
      properties = list(),

      #' @description
      #' Constructor method.
      #'
      #' @return a `World` instance.
      initialize = function() {
        self$add(0L, ".time")
        self$add(1.0, ".scale")
        self$add(0L, ".last_id")
        invisible(self)
      },

      #' @description
      #'
      #' For adding [Entities] and properties (see the `properties` field) to [World].
      add = function(object, name) {
        checkmate::assert(
          checkmate::check_r6(object, classes = "Entity"),
          checkmate::check_r6(object, classes = "Target"),
          checkmate::check_r6(object, classes = "Model"),
          checkmate::check_list(object, any.missing = FALSE, min.len = 1, names = "unique"),
          # checkmate::check_class(object, classes = "list"),
          checkmate::check_class(object, classes = "numeric"),
          checkmate::check_class(object, classes = "integer"),
          combine = "or"
        )
        checkmate::assert_string(name, null.ok = TRUE)
        .field_name = ifelse(checkmate::test_r6(object, classes = "Entity"),
                             "entities",
                             "properties")
          len = length(self[[.field_name]])
          self[[.field_name]][[len + 1L]] = object
          names(self[[.field_name]])[[len + 1L]] = name
          return(self)
      },

      print = function() {
        message(sprintf("time = %s \nscale = %s", self$properties$.time, self$properties$.scale))
      }

    ))


#' @param value
#'
#' @rdname World2
#' @export
set_time = function(world, value) {
  checkmate::assert_class(world, "World2")
  world$properties$.time = checkmate::assert_integerish(
    value,
    lower = 0,
    len = 1,
    null.ok = FALSE
  )
  return(world)
}

#' @param value a value to be set.
#'
#' @rdname World2
#' @export
set_scale = function(world, value) {
  checkmate::assert_class(world, "World2")
  checkmate::assert_number(
    value,
    finite = TRUE,
    null.ok = FALSE,
    lower = 0
  )
  if (value == 0) {
    stop("`value` (scale) cannot be set to 0. If you would like to set your Targets",
         " to zero, please remove those Targets instead.")
  }
  world$properties$.scale = value
  return(world)
}
