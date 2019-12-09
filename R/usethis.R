#' Create an event.
#'
#' @description
#'
#' This function creates an event script from the provided event template inside
#' a module along with a testtthat test script.
#'
#' @param name Name of the event.
#' @param module Name of the module folder to add a event file to. The function
#' looks for a folder inside the `modules` folder at the root folder of the active
#' R project. If the module folder is not found or has not been created this will
#' return an error.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   # Note: running this will create a folder called "modules" and a sub-folder
#'   #       to your working directory within the folder called "demography"
#'   use_module(name = "demography")
#'
#'   # create an event called 'birth' inside the 'demography' module.
#'   use_event(name = "birth", module = 'demography')
#' }
use_event <- function(name, module) {
  .check_file_name(name)

  if (!has_module(module)) {
    stop(glue("A module called '{module}' doesn't exist. Please make \\
               sure the module has been created with `dymiumCore::use_module('{module}')` \\
               before using this function."))
  }

  event_path <- fs::path("modules", module, .slug(name, "R"))
  module_path <- fs::path("modules", "module")

  usethis::use_template("event.R",
                        save_as = event_path,
                        data = list(module_path = module_path,
                                    event_name = name),
                        package = "dymiumCore")

  invisible(event_path)
}

#' Create and setup a module folder.
#'
#' @description
#' This function creates a new module inside the modules folder of an active r project.
#' If the 'modules' folder doesn't exist it will create it then adds a new folder
#' with the name as specified in the `name` argument inside the 'modules' folder.
#' R scripts to be used across the module will be added which contain the following:
#'
#' * a lgr logger script,
#' * a script that contains constant values, and
#' * a script for storing helper functions.
#'
#' Note that, to add event functions to a module see [use_event].
#'
#' @param name Name of the module.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   # Note: running this will create a folder called "modules" and a sub-folder
#'   #       to your working directory within the folder called "demography"
#'   use_module(name = "demography")
#' }
use_module <- function(name) {
  .check_file_name(name)

  required_pkgs <- c("modules", "lgr", "checkmate", "here", "R6")
  sapply(required_pkgs, usethis:::check_installed)

  module_path <- fs::path("modules", name)

  if (has_module(name)) {
    stop(glue("The module {name} already exists at {module_path}."))
  }

  usethis::use_directory("modules", ignore = TRUE)
  usethis::use_directory(module_path)

  usethis::use_template(
    template = "logger.R",
    save_as = fs::path(module_path, "logger.R"),
    data = list(module_path = module_path),
    package = "dymiumCore"
  )
  usethis::use_template(
    template = "constants.R",
    save_as = fs::path(module_path, "constants.R"),
    data = list(module_path = module_path),
    package = "dymiumCore"
  )
  usethis::use_template(
    template = "helpers.R",
    save_as = fs::path(module_path, "helpers.R"),
    data = list(module_path = module_path),
    package = "dymiumCore"
  )

  invisible(module_path)
}

#' Create a dymium scenario folder
#'
#' @description
#' Creates a scenario folder using a standard format which contains an 'inputs' folder,
#' an 'outputs' folder inside.
#'
#'
#' @param name Name of the scenario
#' @param active a logical value with defauly being FALSE. This determines whether
#' to set the current active scenario to this newly created scenario or not.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   use_scenario(name = "demography")
#' }
use_scenario <- function(name, active = TRUE) {
  .check_file_name(name)
  path <- fs::path("scenarios", name)
  usethis::use_directory("scenarios", ignore = TRUE)
  usethis::use_directory(path)
  usethis::use_directory(fs::path(path, "inputs"))
  usethis::use_directory(fs::path(path, "outputs"))
  invisible(path)
}

has_module <- function(name) {
  path <- fs::path("modules", name)
  fs::file_exists(path)
}
