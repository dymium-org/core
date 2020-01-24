#' Create, set, and get scenario.
#'
#' @description
#'
#' A scenario folder is where you organise and store the scripts, the data and
#' the models that associate with a microsimulation model implementation. This
#' is a recommended practice for dymium models.
#'
#' `create_scenario`: creates a scenario folder inside the `scenarios` folder, which
#' will be created if not already exists, at the root folder of your active RStudio
#' project. The standard structure of a scenario folder contains an 'inputs' folder,
#' an 'outputs' folder inside. However, if the scenario is already existed then it
#' this will set that scenario to `active` if `active` is `TRUE`. When you call a
#' dymium function such as `dm_save`, it will automatically save the data to the
#' `outputs` folder of the currently active scenario.
#'
#' `set_active_scenario`: is useful for event functions to access the current
#' active scenario directory. If the scenerio folder doesn't have 'inputs' and
#' 'ouputs' folders then they will be created.
#'
#' `get_active_scenario`: returns a list of 3 elements: 'scenario, 'inputs' and
#' 'outputs' directories.
#'
#' @param name :: `character(1)`\cr
#'  Name of the scenario
#' @param .basedir :: `character(1)`\cr
#'  The base directory that the downloaded module will be saved at. [here::here()] is
#'  used to provide the default value which is is the root folder of the active RStudio project.
#' @param active :: `logical(1)\cr
#'  Default as `TRUE`. Set the newly created scenario as active scenario.
#' @export
#'
#' @return `create_` and `set_` invisibly returns the scenario path and `get_` returns a named list.
#'
#' @examples
#'
#' \dontrun{
#'   create_scenario(name = "test", active = FALSE)
#'   set_active_scenario(name = "test")
#'   get_active_scenario()
#' }
create_scenario <- function(name, active = TRUE, .basedir = here::here()) {
  .check_file_name(name)
  path <- fs::path(.basedir, "scenarios", name)
  fs::dir_create(path, recurse = TRUE)
  message("Created: ", path)
  fs::dir_create(fs::path(path, "inputs"))
  message("Created: ", fs::path(path, "inputs"))
  fs::dir_create(fs::path(path, "outputs"))
  message("Created: ", fs::path(path, "outputs"))
  if (active) {
    set_active_scenario(name)
  }
  invisible(path)
}

#' @rdname create_scenario
#' @export
set_active_scenario <- function(name, .basedir = here::here()) {
  checkmate::assert_string(name,
                           pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._.-]*$",
                           na.ok = FALSE,
                           null.ok = FALSE)

  scenario_path <- fs::path(.basedir, "scenarios", name)
  input_path <- fs::path(scenario_path, "inputs")
  output_path <- fs::path(scenario_path,  "outputs")

  if (!checkmate::test_directory_exists(scenario_path, access = "rw")) {
    stop(glue::glue(
      "{scenario_path} doesn't exist. You can use `create_scenario()` to create \\
      a new scenario under your project directory or create it manually."
    ))
  }

  if (!checkmate::test_directory_exists(input_path, access = "rw")) {
    fs::dir_create(input_path)
  }

  if (!checkmate::test_directory_exists(output_path, access = "rw")) {
    fs::dir_create(output_path)
  }

  opts.dymium <- list(
    dymium.scenario_dir = scenario_path,
    dymium.input_dir = input_path,
    dymium.output_dir = output_path
  )
  options(opts.dymium)

  .dymium_options_msg()

  invisible(scenario_path)
}

#' @rdname create_scenario
#' @export
get_active_scenario <- function() {
  .dymium_options()
}
