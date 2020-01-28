#' @title A representation of World
#'
#' @description
#' World is to be used for storing [Entities], [Models] and [Containers], setting
#' the simulation clock, and for keeping a log of your simulation outputs. You may
#' think of it as a container that is to be passed into event functions and let
#' each event function accesses the instances it needs that are being stored inside
#' World.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Container]<-[ContainerGeneric]<-[Generic].
#' @include Generic.R
#'
#' @section Construction:
#'
#' ```
#' x <- World$new()
#' ```
#'
#' * NULL\cr
#'
#' @section Public Fields:
#'
#' * `Cont`\cr
#'  Contains the objects those were added using `self$add(...)`.
#'
#' * `info`\cr
#'  Contains information about the World object such as dymium's version it was
#'  created with, its built date, creator info, R version, etc.
#'
#' @section Active fields (read-only):
#'
#' * `containers`\cr
#'  Contains [Containers] those were added using `self$add(...)`.
#'
#' * `entities`\cr
#'  Contains [Entities] those were added using `self$add(...)`.
#'
#' * `models`\cr
#'  Contains [Models] those were added using `self$add(...)`.
#'
#' @section Public Methods:
#'
#' * `add(x, name)`\cr
#'  ([dymiumCore::Entity] and inheritances | [dymiumCore::Container] | an object of the classes in
#'  [dymiumCore::SupportedTransitionModels])\cr
#'  Add an object the allowed types to `self$Cont`, `self$entities`, `self$containers`,
#'  `self$models`. Only one instance of each class are allowed to be stored.
#'  The stored instances can be access via `self$<object-type>` or `self$get(x)`
#'
#' * `remove(x)`\cr
#'  (`character(1)` | `integer(1)`)\cr
#'  Remove an object in element or named `x`.
#'
#' * `get(x)`\cr
#'  (`character(1)`) -> (`an R6 object`)\cr
#'  Get the value of the key in `x`. For example, if your [World] object contains
#'  a [Population] object, then you can simply call `world$get("Population")` to
#'  get the [Population] object, assuming that the [World] object is named `world`
#'  in your calling scope.
#'
#' * `get_entity(x)`\cr
#'  (`character(1)`) -> An [Entity] object\cr
#'  Get a reference to the `x` Entity object.
#'
#' * `get_model(x)`\cr
#'  (`character(1)`) -> A [Model] object\cr
#'  Get a model object by name `x`. Note that, this returns a [Model] object of
#'  [R6::R6Class] not the actual model itself. To get the model object use `$get()`
#'  on the [Model] object.
#'
#' * `get_time()`\cr
#'  () -> (`integer(1)`)\cr
#'  Get the value of .DMevn$sim_time.
#'
#' * `get_info()`\cr
#'  `NULL` -> `list()`\cr
#'  Get information about the creation of the world object.
#'  Dymium's version, dependencies, R version etc.
#'
#' * `set_time(x)`\cr
#'  (`integer(1)`) -> `NULL`\cr
#'  Set the time on the World's simulation clock (.DMevn$sim_time).
#'
#' * `reset_time()`\cr
#'  Reset the value of .DMevn$sim_time to 0L (L is for forcing type integer
#'  otherwise 0 is of numeric type).
#'
#' * `start_iter(time_step, unit)`\cr
#'  (`integer(1)`, `character(1)`) -> `self`\cr
#'  Update the simulation time and returns self.
#'
#' @export
World <- R6::R6Class(
  classname = "World",
  inherit = dymiumCore::Container,

  public = list(
    info = list(
      built.datetime = Sys.time(),
      dymiumCore.version = utils::packageVersion("dymiumCore"),
      R.version = base::version
    ),

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
        lg$info("Adding an Entity object '{name}' to the `entities` field.")
        .listname <- ".entities"
      }

      if (inherits(x, "Container")) {
        lg$info("Adding a Container object '{name}' to the `containers` field.")
        x$unpack(target = self)
        .listname <- ".containers"
      }

      if (class(x)[[1]] %in% dymiumCore::SupportedTransitionModels()) {
        lg$info("Adding a Model object '{name}' to the `models` field.")
        x <- Model$new(x)
        .listname <- ".models"
      }

      # make sure there is only one of each Entity sub class stored in entities
      .listnames <- names(get(.listname, envir = private))
      if (name %in% .listnames) {
        stop(glue::glue("{name} already exists in {.listname}. Only one instance \\
                         of each class is allowed to be added."))
      }

      # all references are to be stored in self$Cont. To make accessing these
      # references easiser we added the three extra fields so that objects
      # of Entity, Container and Model are stored separately.
      super$add(x, name = name)
      .last_pos <- self$n()

      .pos <- length(get(.listname, envir = private)) + 1L

      switch(.listname,
             ".entities" = {
                private$.entities[[.pos]] <- self$get(.last_pos)
                names(private$.entities)[.pos] <- name
             },
             ".containers" = {
               private$.containers[[.pos]] <- self$get(.last_pos)
               names(private$.containers)[.pos] <- name
             },
             ".models" = {
               private$.models[[.pos]] <- self$get(.last_pos)
               names(private$.models)[.pos] <- name
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

      .pos_cont <- which(names(self$Cont) == name)
      .pos_models <- which(names(self$models) == name)
      .pos_entities <- which(names(self$entities) == name)
      .pos_containers <- which(names(self$containers) == name)

      if (length(.pos_cont) != 0) super$remove(.pos_cont)
      if (length(.pos_containers) != 0) private$.containers[[.pos_containers]] <- NULL
      if (length(.pos_entities) != 0) private$.entities[[.pos_entities]] <- NULL
      if (length(.pos_models) != 0) private$.models[[.pos_models]] <- NULL

      # Go through all `containers` stored in World and remove the object which
      # its name matches `name`
      for (c in self$containers) {
        if (x %in% c$names()) {
          c$remove(name)
        }
      }

      invisible()
    },

    replace = function(x, name) {
      self$remove(name)
      self$add(x, name)
    },

    get_entity = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These entities are available: {.entities}.",
                        .entities = glue::glue_collapse(names(self$entities),
                                                         sep = ", ",
                                                         last = " and ",
                                                         width = 200)))
      }
      if (inherits(x, "R6ClassGenerator")) {
        x <- x$classname
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$entities) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$entities), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$entities))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
                        .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$entities[[.pos]])
    },

    get_model = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These models are available: {.models}.",
                        .models = glue::glue_collapse(names(self$models),
                                                      sep = ", ",
                                                      last = " and ",
                                                      width = 200)))
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$models) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$models), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$models))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
             .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$models[[.pos]])
    },

    get_container = function(x) {
      if (missing(x)) {
        stop(glue::glue("`x` is missing, with no default. These entities are available: {.entities}.",
                        .entities = glue::glue_collapse(names(self$containers),
                                                        sep = ", ",
                                                        last = " and ",
                                                        width = 200)))
      }
      if (inherits(x, "R6ClassGenerator")) {
        x <- x$classname
      }
      checkmate::assert_string(x)
      .pos <- which(names(self$containers) == x)
      if (length(.pos) == 0) {
        possible_names <- agrep(x, names(self$containers), value = TRUE, max.distance = 0.25)
        if (length(possible_names) == 0) {
          checkmate::assert_choice(x, choices = names(self$containers))
        }
        stop(glue::glue("Did you mean {.possible_names}?",
                        .possible_names = glue::glue_collapse(possible_names, sep = ", ", last = " or ")))
      }
      return(self$containers[[.pos]])
    },

    get_time = function(x) {
      getOption("dymium.simulation_clock")
    },

    get_info = function() {
      self$info
    },

    # @description Set the simulation clock of World.
    # @param x An integer value.
    set_time = function(x) {
      checkmate::assert_integerish(x, lower = 0, len = 1)
      options(dymium.simulation_clock = x)
      invisible()
    },

    # @description Print self
    print = function() {
      super$print()
      for (e in self$entities) {
        e$print()
      }
    },

    reset_time = function() {
      self$set_time(0L)
      invisible()
    },

    run_checks = function() {
      fs <- list(validate_linkages)
      for (f in fs) {
        f(self)
      }
    },

    start_iter = function(time_step, unit = "iteration", run_checks = FALSE) {
      checkmate::assert_count(time_step, positive = T, na.ok = FALSE, null.ok = FALSE)
      self$set_time(x = time_step)
      if (run_checks) {
        self$run_checks()
      }
      lg$info("Starting {unit}: {self$get_time()}")
      invisible(self)
    }
  ),

  active = list(
    #' @field containers a list of all [Containers] stored in World.
    containers = function() {
      get(".containers", envir = private)
    },
    #' @field containers a list of all [Entities] stored in World.
    entities = function() {
      get(".entities", envir = private)
    },
    #' @field containers a list of all [Models] stored in World.
    models = function() {
      get(".models", envir = private)
    }
  ),

  private = list(
    .containers = list(),
    .entities = list(),
    .models = list()
  )
)
