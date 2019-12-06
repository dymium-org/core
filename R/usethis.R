#' Create a dymium event file.
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
use_event <- function(name, module) {
  usethis:::check_file_name(name)

  if (!has_module(module)) {
    stop(glue("A module called '{module}' doesn't exist. Please make \\
               sure the module has been created with `dymiumCore::use_module('{module}')` \\
               before using this function."))
  }

  event_path <- fs::path("modules", module, usethis:::slug(name, "R"))
  module_path <- fs::path("modules", "module")

  usethis::use_template("event.R",
                        save_as = event_path,
                        data = list(module_path = module_path,
                                    event_name = name),
                        package = "dymiumCore")

  invisible(event_path)
}

#' Create a dymium module folder and events
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
#'   use_module(
#'     name = "demography"
#'   )
#' }
use_module <- function(name) {
  usethis:::check_file_name(name)

  required_pkgs <- c("modules", "lgr", "checkmate", "here", "R6")
  sapply(required_pkgs, usethis:::check_installed)

  module_path <- fs::path("modules", name)

  if (has_module(name)) {
    usethis:::ui_stop("The module {name} already exists at {module_path}.")
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
#' @param name Name of the scenario
#' @export
#'
#' @examples
use_scenario <- function(name) {
  usethis:::check_file_name(name)
  path <- fs::path("scenarios", name)
  usethis::use_directory("scenarios", ignore = TRUE)
  usethis::use_directory(path)
  invisible(path)
}

has_module <- function(name) {
  path <- fs::path("modules", name)
  fs::file_exists(path)
}
