#' @title World2 class
#'
#' @description
#' World is used to store other Entities. Think of it as a container that is to be
#' passed into event functions and let each event function uses data that it is storing.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [dymiumCore::Entity]
#' @include Generic.R
#'
#' @section Construction:
#'
#' ```
#' x <- World2$new()
#' ```
#'
#' * NULL\cr
#'
#' @section Public Fields:
#'
#' * `config`\cr
#'  Contains the config file that was used to create the instance, if there is any.
#'
#' * `Cont`\cr
#'  Contains the objects those were added using `self$add(...)`.
#'
#' * `Containers`\cr
#'  Contains the [Container], and its inheritences, objects those were added using `self$add(...)`.
#'
#' * `Entities`\cr
#'  Contains the [Entity], and its inheritences, objects those were added using `self$add(...)`.
#'
#' * `Models`\cr
#'  Contains the [Model] objects those were added using `self$add(...)`.
#'
#' * `info`\cr
#'  Contains information about the World object such as dymium's version it was
#'  created with, its built date, creator info, R version, etc.
#'
#'
#' @section Public Methods:
#'
#' * `add(x, name)`\cr
#'  ([dymiumCore::Entity] and inheritances | [dymiumCore::Container] | an object of the classes in
#'  [dymiumCore::SupportedTransitionModels])\cr
#'  Add an object the allowed types to `self$Cont`, `self$Entities`, `self$Containers`,
#'  `self$Models`. Only one instance of each class are allowed to be stored.
#'  The stored instances can be access via `self$<object-type>` or `self$get(x)`
#'
#' * `remove(x)`\cr
#'  (`character(1)` | `integer(1)`)\cr
#'  Remove an object in element or named `x`.
#'
#' * `get_entity(x)`\cr
#'  (`character(1)`)\cr
#'  Get a reference to the `x` Entity object.
#'
#' * `get_model(x)`\cr
#'  (`character(1)`)\cr
#'  Get a model object by name `x`.
#'
#' * `get_time()`\cr
#'  Get the value of .DMevn$sim_time.
#'
#' * `get_info()`\cr
#'  `NULL` -> `list()`\cr
#'  Get information about the creation of the world object.
#'  Dymium's version, dependencies, R version etc.
#'
#' * `set_time(x)`\cr
#'  (`integer(1)`) -> `NULL`\cr
#'  Set the value of .DMevn$sim_time.
#'
#' * `reset_time()`\cr
#'  Reset the value of .DMevn$sim_time to 0L (L is for forcing type integer
#'  otherwise 0 is of numeric type).
#'
#' * `start_iter(time_step, unit)`\cr
#'  (`integer(1)`, `character(1)`) -> `self`\cr
#'
#' @export
World <- R6::R6Class(
  classname = "World",
  inherit = dymiumCore::Container,

  public = list(
    Containers = list(),
    Entities = list(),
    Models = list(),
    info = list(
      built.datetime = Sys.time(),
      dymiumCore.version = utils::packageVersion("dymiumCore"),
      R.version = base::version
    ),
    config = NULL,

    initialize = function() {
      # always reset to 0
      if (self$get_time() != 0) {
        self$reset_time()
      }
      invisible()
    },

    add = function(x, name) {
      checkmate::assert(
        checkmate::check_r6(x, classes = c("Entity", "Generic"), null.ok = FALSE),
        checkmate::check_r6(x, classes = c("Container", "Generic"), null.ok = FALSE),
        checkmate::check_subset(class(x)[[1]],
                                choices = dymiumCore::SupportedTransitionModels(),
                                empty.ok = FALSE),
        combine = "or"
      )

      if (inherits(x, "Generic")) {
        stopifnot(x$is_dymium_class())
        if (!missing(name)) {
          lg$warn("The given `name` will be ignored since the object in x \\
                  is of a Dymium class object. The classname of the object will be \\
                  used as its name.")
        }
        name <- class(x)[[1]]
      }

      # only allows letters and underscores
      checkmate::assert_string(name,
                               pattern = "^[a-zA-Z_]*$",
                               na.ok = FALSE,
                               null.ok = FALSE)

      if (inherits(x, "Entity")) {
        lg$info("Adding an Entity object '{name}' to Entities.")
        .listname <- "Entities"
      }

      if (inherits(x, "Container")) {
        lg$info("Adding a Container object '{name}' to Containers.")
        x$unpack(self)
        .listname <- "Containers"
      }

      if (class(x)[[1]] %in% dymiumCore::SupportedTransitionModels()) {
        lg$info("Adding a Model object '{name}' to Models.")
        x <- Model$new(x)
        .listname <- "Models"
      }

      # make sure there is only one of each Entity sub class stored in Entities
      .listnames <- names(get(.listname, envir = self))
      if (name %in% .listnames) {
        stop(glue::glue("{name} already exists in {.listname}. Only one instance \\
                         of each class is allowed to be added."))
      }

      # all references are to be stored in self$Cont. To make accessing these
      # references easiser we added the three extra fields so that objects
      # of Entity, Container and Model are stored separately.
      super$add(x, name = name)
      .last_pos <- self$n()

      .pos <- length(get(.listname, envir = self)) + 1L

      switch(.listname,
             "Entities" = {
                self$Entities[[.pos]] <- self$get(.last_pos)
                names(self$Entities)[.pos] <- name
             },
             "Containers" = {
               self$Containers[[.pos]] <- self$get(.last_pos)
               names(self$Containers)[.pos] <- name
             },
             "Models" = {
               self$Models[[.pos]] <- self$get(.last_pos)
               names(self$Models)[.pos] <- name
             },
             stop("Something is not right please report this issue to the maintainer."))

      invisible()
    },

    remove = function(x) {
      checkmate::assert(
        checkmate::check_string(x, na.ok = FALSE, null.ok = FALSE),
        checkmate::check_number(x, lower = 1, finite = TRUE, na.ok = FALSE, null.ok = FALSE),
        combine = "or"
      )

      if (is.character(x)) {
        checkmate::assert_choice(x, choices = self$names())
        name <- x
      }

      if (is.numeric(x)) {
        self$check_pos(x)
        name <- self$names()[x]
      }

      .pos_Cont <- which(names(self$Cont) == name)
      .pos_Models <- which(names(self$Models) == name)
      .pos_Entities <- which(names(self$Entities) == name)
      .pos_Containers <- which(names(self$Containers) == name)

      if (length(.pos_Cont) != 0) super$remove(.pos_Cont)
      if (length(.pos_Containers) != 0) self$Containers[[.pos_Containers]] <- NULL
      if (length(.pos_Entities) != 0) self$Entities[[.pos_Entities]] <- NULL
      if (length(.pos_Models) != 0) self$Models[[.pos_Models]] <- NULL

      # recursively remove of named objects in containers
      for (c in self$Containers) {
        c$remove(name)
      }

      invisible()
    },

    get_entity = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These entities are available: {.entities}.",
                        .entities = glue::glue_collapse(names(self$Entities),
                                                         sep = ", ",
                                                         last = " and ",
                                                         width = 200)))
      }
      if (inherits(x, "R6ClassGenerator")) {
        x <- x$classname
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$Entities) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$Entities), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$Entities))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
                        .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$Entities[[.pos]])
    },

    get_model = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These models are available: {.models}.",
                        .models = glue::glue_collapse(names(self$Models),
                                                      sep = ", ",
                                                      last = " and ",
                                                      width = 200)))
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$Models) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$Models), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$Models))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
             .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$Models[[.pos]])
    },

    get_container = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These entities are available: {.entities}.",
                        .entities = glue::glue_collapse(names(self$Containers),
                                                        sep = ", ",
                                                        last = " and ",
                                                        width = 200)))
      }
      if (inherits(x, "R6ClassGenerator")) {
        x <- x$classname
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$Containers) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$Containers), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$Containers))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
                        .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$Containers[[.pos]])
    },

    get_time = function(x) {
      get("sim_time", envir = .DMevn)
    },

    get_info = function() {
      self$info
    },

    set_time = function(x) {
      assert_that(is.numeric(x))
      assign("sim_time", as.integer(x), envir = .DMevn)
      invisible()
    },

    print = function() {
      super$print()
      for (e in self$Entities) {
        e$print()
      }
    },

    reset_time = function() {
      self$set_time(0L)
      invisible()
    },

    start_iter = function(time_step, unit = "iteration") {
      assert_that(assertthat::is.count(time_step))
      self$set_time(x = time_step)
      lg$info("Starting {unit}: {self$get_time()}")
      invisible(self)
    }
  ),

  private = list(

  )
)
