#' Set an active scenario directory.
#'
#' @description
#' This is useful for event functions to access the current active scenario directory.
#'
#' @param name name of the scenario folder to become active.
#' @param active a logical value with defauly being FALSE. This determines whether
#' to set the current active scenario to this newly created scenario or not.
#'
#' @return a list contains scenario directories.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'  use_module("test")
#'  set_active_scenario("test")
#' }
set_active_scenario <- function(name) {

  scenario_path <- fs::path_wd("scenarios", name)
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
