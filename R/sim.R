#' @title Compile and execute a microsimulation pipeline
#'
#' @description
#' This function compiles and executes a microsimulation pipeline.
#'
#' `sim` mutates the given [World] object. While, `sim_parallel` clones the
#' given [World] object and returns `n_repeats` of mutated [Worlds].
#'
#' Note that, for `sim_parallel` all objects used in the pipeline must be stored
#' inside the world object and not in your global environment. You can store filter
#' functions as well, if you need them.
#'
#' @param world (`World`)\cr
#'  A [World] object.
#' @param pipeline (`function()`)\cr
#'  A functional sequence (`fseq`) object.
#' @param n_iters a number of iterations. (`integer(1)`)\cr
#'  Number of times the microsimulation pipeline, `pipeline`, should be repeated
#'  on the `world` object.
#' @param write.error.dump.folder (`character(1)`)\cr
#'  path: Saves the dump of the workspace in a specific folder instead of the
#'  working directory
#' @param n_repeats (`integer(1)`)\cr
#'  Number of times the entire simulation should be repeated. This is not the same as
#'  `n_iters`.
#' @param n_workers (`integer(1)`)\cr
#'  Number of parallel workers. This requires the `parallel` package to be installed.
#' @param DTthreads (`integer(1)`|`missing`)\cr
#'  Number of cores data.table is allowed to used in an R session. If missing,
#'  it will use the default number of cores set by [data.table::data.table], which
#'  is usually half of the available number of cores.
#' @param .options [furrr::future_options()]\cr
#'  By default this is set to `furrr::future_options(scheduling = FALSE)`.
#'  See [furrr::future_options()] for more details.
#'
#' @return `sim` doesn't return anything but mutates the given [World] object. While,
#' `sim_paralell()` returns `n_tasks` number of mutated [World] objects.
#'
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
sim <-
  function(world,
           pipeline,
           n_iters = 1,
           write.error.dump.folder = get_active_scenario()$output_dir,
           write.error.dump.file = FALSE) {

  checkmate::assert_r6(world, classes = "World")
  checkmate::assert_function(pipeline, nargs = 1)
  checkmate::assert_count(n_iters, positive = TRUE)
  checkmate::assert_directory_exists(write.error.dump.folder, access = "rwx")

  tryCatchLog::tryCatchLog({
    for (i in 1:n_iters) {
      world$start_iter(time_step = world$get_time() + 1L) %>%
        pipeline(.)
    }
  },
  write.error.dump.file = write.error.dump.file,
  write.error.dump.folder = write.error.dump.folder)

  invisible()
}


#' @rdname sim
#' @export
sim_parallel <-
  function(world,
           pipeline,
           n_iters = 1L,
           n_repeats = 1L,
           n_workers = 1L,
           DTthreads = data.table::getDTthreads(),
           .future_options,
           write.error.dump.file = FALSE,
           write.error.dump.folder = get_active_scenario()$output_dir) {

  check_package("furrr") # furrr requires future, future requires parallel.

  checkmate::assert_r6(world, classes = "World")
  checkmate::assert_function(pipeline, nargs = 1)
  checkmate::assert_count(n_iters, positive = TRUE)
  checkmate::assert_count(n_repeats, positive = TRUE)
  checkmate::assert_count(n_workers, positive = TRUE)
  checkmate::assert_count(DTthreads, positive = TRUE)
  checkmate::assert_flag(write.error.dump.file, null.ok = FALSE, na.ok = FALSE)
  checkmate::assert_directory_exists(write.error.dump.folder, access = "rwx")

  if (DTthreads > parallel::detectCores()) {
    stop(sprintf("DTthreads cannot be set more than the number of cores available (%s).",
                 parallel::detectCores()))
  }

  if (n_workers > parallel::detectCores()) {
    stop(sprintf("n_workers cannot be set more than the number of cores available (%s).",
                 parallel::detectCores()))
  }

  if (missing(.future_options)) {
    .future_options <- furrr::future_options(scheduling = FALSE, packages = "dymiumCore")
  } else {
    checkmate::assert_class(.future_options, classes = "future_options")
  }

  # save world to a folder
  world_saved_path <- fs::path(get_active_scenario()$input_dir, "world_tmp.rds")
  message("Saving `world` at ", world_saved_path)
  saveRDS(world, world_saved_path)

  # create a cluster
  message("Creating a cluster of ", n_workers, " workers.")
  cl <- future::makeClusterPSOCK(workers = n_workers)
  future::plan(future::cluster, workers = cl)
  # future::plan(future::sequential)

  # browser()

  message("Sending simualtion tasks to workers")
  simulated_worlds <-
    tryCatchLog::tryCatchLog({
      # assign simulation tasks to workers
      furrr::future_map(
        .x = seq_len(n_repeats),
        seed = FALSE,
        .options = .future_options,
        .progress = TRUE,
        .f = ~ {
        data.table::setDTthreads(threads = DTthreads)
        .world <- readRDS(world_saved_path)
        # run simulation
        for (i in 1:n_iters) {
          .world$start_iter(time_step = .world$get_time() + 1L) %>%
            pipeline(.)
        }
        return(.world)
      })
    }, finally = {
      if (exists("cl")) {
        parallel::stopCluster(cl)
      }
    },
    write.error.dump.file = write.error.dump.file,
    write.error.dump.folder = write.error.dump.folder,
    silent.messages = TRUE,
    silent.warnings = TRUE,
    include.full.call.stack = FALSE,
    include.compact.call.stack = FALSE)

  message("Done. ;)")
  return(simulated_worlds)
}


fsim <- function(x, n_reps, my_func) {
  cl <- future::makeClusterPSOCK(workers = 2)
  future::plan(future::cluster, workers = cl)

  res <- furrr::future_map(seq_len(n_reps),
                           ~ {
                             x %>%
                               my_func
                           })

  parallel::stopCluster(cl)

  return(res)
}
