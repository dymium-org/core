test_that("sim works", {
  # create simple models
  birth_model <- list(yes = 0.1, no = 0.9)
  death_model <- list(yes = 0.1, no = 0.9)

  # prepare population data
  ind_data <-
    data.table::copy(toy_individuals) %>%
    .[, .give_birth := "no"]

  # create a World object, a container for all entities and models for simulation
  world <- World$new()
  world$add(x = Individual$new(.data = ind_data, id_col = "pid"))

  # create filters, this is a method for creating functions using `magrittr` and
  # data.table's syntax
  filter_eligible_females <-
    . %>%
    .[sex == "female" & age %between% c(18, 50)]

  filter_alive <-
    . %>%
    .[age != -1]

  microsimulation_pipeline <-
    . %>%
    # ageing
    mutate_entity(entity = "Individual",
                  age := age + 1L,
                  subset = age != -1L) %>%
    # simulate birth decision
    transition(entity = "Individual",
               model = birth_model,
               attr = ".give_birth",
               preprocessing_fn = . %>% filter_eligible_females %>% filter_alive) %>%
    # add newborns
    add_entity(entity = "Individual",
               newdata = toy_individuals[age == 0, ],
               target = .$entities$Individual$get_data()[.give_birth == "yes", .N]) %>%
    # reset the birth decision variable
    mutate_entity(entity = "Individual",
                  .give_birth := "no",
                  subset = age != -1L) %>%
    # simulate deaths
    transition(entity = "Individual",
               model = death_model,
               attr = "age",
               values = c(yes = -1L),
               preprocessing_fn = filter_alive) %>%
    # log the total number of alive individuals at the end of the iteration
    add_log(desc = "count:Individual",
            value = .$entities$Individual$get_data()[age != -1L, .N])

  n_iters = 10

  sim(world = world, pipeline = microsimulation_pipeline, n_iters = n_iters)

  expect_equal(world$get_time(), n_iters)

  expect_error(sim(world = 1, pipeline = microsimulation_pipeline, n_iters = 10),
               "Assertion on 'world' failed: Must be an R6 class,")
  expect_error(sim(world = world, pipeline = microsimulation_pipeline, n_iters = 0),
               "Assertion on 'n_iters' failed: Must be >= 1.")
  expect_error(sim(world = world, pipeline = 1, n_iters = 10),
               "Assertion on 'pipeline' failed: Must be a function")

})
