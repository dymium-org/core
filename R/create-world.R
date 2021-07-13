#' Create a loaded population object
#'
#' Create a loaded population object and assign to the global environment as `pop`.
#'
#' @return assign `pop` to global env.
#' @export
create_toy_population <- function() {
  pop <<- Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid", "partner_id", "mother_id", "father_id"),
    hid_col = c("hid")
  )
  invisible(pop)
}

create_pop_sample <- function() {
  create_toy_population()
}

#' @title create toy world
#' @description
#' Create a toy world (assigned to the global environment as `world`) for running tests
#' @param add_toy_zones add `toy_zones` to world (default TRUE).
#' @export
create_toy_world <- function(add_toy_zones = TRUE) {
  world <<- World$new()
  world$add(
    Population$new(
      ind_data = toy_individuals,
      hh_data = toy_households,
      pid_col = c("pid", "partner_id", "mother_id", "father_id"),
      hid_col = c("hid")
    )
  )
  world$add(BuildingResidential$new(toy_dwellings, "did"))
  world$get(BuildingResidential)$set_owner_object(world$get(Household))
  if (add_toy_zones) {
    world$add(Zone$new(toy_zones, "zid"))
  }
  invisible(world)
}
