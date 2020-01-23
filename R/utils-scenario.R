#' Set an active scenario directory.
#'
#' @description
#' This is useful for event functions to access the current active scenario directory.
#'
#' @param name name of the scenario folder to become active.
#' @param .basedir :: `character(1)`\cr
#' The base directory that the downloaded module will be saved at. [here::here()] is
#' used to provide the default value which is is the root folder of the active RStudio project.
#'
#' @return a list contains scenario directories.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  use_module("test")
#'  set_active_scenario("test")
#' }
set_active_scenario <- function(name, .basedir = here::here()) {

  scenario_path <- fs::path(.basedir, "scenarios", name)
  input_path <- fs::path(scenario_path, "inputs")
  output_path <- fs::path(scenario_path,  "outputs")

  checkmate::assert_directory_exists(scenario_path, access = "rw")
  checkmate::assert_directory_exists(input_path, access = "rw")
  checkmate::assert_directory_exists(output_path, access = "rw")

  opts.dymium <- list(
    dymium.scenario_dir = scenario_path,
    dymium.input_dir = input_path,
    dymium.output_dir = output_path
  )
  options(opts.dymium)

  .dymium_options_msg()

  invisible(scenario_path)

}

#' Get the main directories of the currently active scenario folder.
#'
#' @return a list of 3 elements: 'scenario', 'inputs' and 'outputs' directories.
#' @export
active_scenario <- function() {
  .dymium_options()
}
