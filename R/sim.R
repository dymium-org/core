#' @title Compile and execute a microsimulation pipeline
#'
#' @description
#' This function compiles and executes a microsimulation pipeline.
#'
#' @param world (`World`)\cr
#'  A [World] object.
#' @param pipeline (`function()`)\cr
#'  A functional sequence (`fseq`) object.
#' @param n_iters a number of iterations. (`integer(1)`)\cr
#'  Number of times the microsimulation pipeline should be repeated.
#' @param write.error.dump.folder (`character(1)`)\cr
#'  path: Saves the dump of the workspace in a specific folder instead of the
#'  working directory
#'
#' @return `NULL`
#' @export
#'
#' @examples
#'
#' library(data.table)
#'
#' # create simple models
#' birth_model <- list(yes = 0.1, no = 0.9)
#' death_model <- list(yes = 0.1, no = 0.9)
#'
#' # prepare population data
#' ind_data <-
#'   data.table::copy(toy_individuals) %>%
#'   .[, .give_birth := "no"]
#'
#' # create a World object, a container for all entities and models for simulation
#' world <- World$new()
#' world$add(x = Individual$new(.data = ind_data, id_col = "pid"))
#'
#' # create filters, this is a method for creating functions using `magrittr` and
#' # data.table's syntax
#' filter_eligible_females <-
#'   . %>%
#'   .[sex == "female" & age %between% c(18, 50)]
#'
#' filter_alive <-
#'   . %>%
#'   .[age != -1]
#'
#' microsimulation_pipeline <-
#'   . %>%
#'   # ageing
#'   mutate_entity(entity = "Individual",
#'                 age := age + 1L,
#'                 subset = age != -1L) %>%
#'   # simulate birth decision
#'   transition(entity = "Individual",
#'              model = birth_model,
#'              attr = ".give_birth",
#'              preprocessing_fn = . %>% filter_eligible_females %>% filter_alive) %>%
#'   # add newborns
#'   add_entity(entity = "Individual",
#'              newdata = toy_individuals[age == 0, ],
#'              target = .$entities$Individual$get_data()[.give_birth == "yes", .N]) %>%
#'   # reset the birth decision variable
#'   mutate_entity(entity = "Individual",
#'                 .give_birth := "no",
#'                 subset = age != -1L) %>%
#'   # simulate deaths
#'   transition(entity = "Individual",
#'              model = death_model,
#'              attr = "age",
#'              values = c(yes = -1L),
#'              preprocessing_fn = filter_alive) %>%
#'   # log the total number of alive individuals at the end of the iteration
#'   add_log(desc = "count:Individual",
#'           value = .$entities$Individual$get_data()[age != -1L, .N])
#'
#' # complie and execute a simulation pipeline
#' sim(world = world, pipeline = microsimulation_pipeline, n_iters = 10)
sim <- function(world, pipeline, n_iters, write.error.dump.file = FALSE, write.error.dump.folder) {

  checkmate::assert_r6(world, classes = "World")
  checkmate::assert_function(pipeline, nargs = 1)
  checkmate::assert_count(n_iters, positive = TRUE)

  if (!missing(write.error.dump.folder)) {
    checkmate::assert_directory_exists(write.error.dump.folder, access = "rwx")
    output_dir <- write.error.dump.folder
  } else {
    output_dir <- get_active_scenario()$output_dir
  }

  tryCatchLog::tryCatchLog({
    for (i in 1:n_iters) {
      world$start_iter(time_step = world$get_time() + 1L) %>%
        pipeline(.)
    }
  },
  write.error.dump.file = write.error.dump.file,
  write.error.dump.folder = get_active_scenario()$output_dir)

  invisible()
}




