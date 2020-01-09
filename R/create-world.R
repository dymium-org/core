#' Create a loaded population object
#'
#' Create a loaded population object and assign to the global environment as `pop`.
#'
#' @return assign `pop` to global env.
#' @export
create_toy_population <- function() {
  assign(x = "pop", value = Population$new(), envir = .GlobalEnv)
  pop$initialise_data(ind_data = dymiumCore::toy_individuals,
                      hh_data = dymiumCore::toy_households)
  invisible()
}

create_pop_sample <- function(){
  create_toy_population()
}

#' @title create toy world
#' @description
#' Create a toy world (assigned to the global environment as `world`) for running tests
#' @export
create_toy_world <- function() {
  world <<- World$new()
  world$add(Population$new(ind_data = toy_individuals, hh_data = toy_households))
  world$add(BuildingResidential$new(toy_dwellings, "did"))
  world$get(BuildingResidential)$set_owner_object(world$get(Household))
  world$add(Zone$new(toy_zones, "zid"))
  invisible(world)
}
