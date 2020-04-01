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
#' @param with_comments a logical value. If `TRUE` the generated event script will contain
#' comments about what each component inside the script does and some recommendations
#' for the user to follow when authoring an event. For advance users, you may not need
#' this hence you may specify `FALSE`. If missing, it will be prompted in the console
#' for you to decide.
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
use_event <- function(name, module, with_comments) {
  .check_file_name(name)

  if (!has_module(module)) {
    stop(glue("A module called '{module}' doesn't exist. Please make \\
               sure the module has been created with `dymiumCore::use_module('{module}')` \\
               before using this function."))
  }

  event_path <- fs::path("modules", module, .slug(name, "R"))
  module_path <- fs::path("modules", module)

  if (missing(with_comments)) {
    with_comments <- c(FALSE, TRUE)[utils::menu(choices = c("No", "Yes"),
                                         title = "Do you want to have authors' comments in the event script?")]
  } else {
    checkmate::assert_logical(with_comments, len = 1, null.ok = FALSE)
  }

  template <- ifelse(with_comments, "event.R", "event-no-comments.R")

  usethis::use_template(template = template,
                        save_as = event_path,
                        data = list(module_path = module_path,
                                    event = name,
                                    module = module),
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
#' Since dymium modules use the 'modules' and 'checkmate' packages, if these
#' packages are not installed the function will ask whether you would like to
#' install them or not.
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
  sapply(required_pkgs, check_pkg_installed)

  module_path <- fs::path("modules", name)

  if (has_module(name)) {
    stop(glue("The module {name} already exists at {module_path}."))
  }

  usethis::use_directory("modules", ignore = TRUE)
  usethis::use_directory(module_path)

  usethis::use_template(
    template = "module-README.rmd",
    save_as = fs::path(module_path, "README.rmd"),
    data = list(module_path = module_path,
                module = name),
    package = "dymiumCore"
  )
  usethis::use_template(
    template = "logger.R",
    save_as = fs::path(module_path, "logger.R"),
    data = list(module_path = module_path,
                module = name),
    package = "dymiumCore"
  )
  usethis::use_template(
    template = "constants.R",
    save_as = fs::path(module_path, "constants.R"),
    data = list(module_path = module_path,
                module = name),
    package = "dymiumCore"
  )
  usethis::use_template(
    template = "helpers.R",
    save_as = fs::path(module_path, "helpers.R"),
    data = list(module_path = module_path,
                module = name),
    package = "dymiumCore"
  )

  invisible(module_path)
}

#' Add a README rmarkdown file to an existing module
#'
#' @param name name of an existing module
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # this assumes that you have a module named 'demography'
#' use_module_readme(name = "demography")
#' }
use_module_readme <- function(name) {
  module_path <- fs::path("modules", name)
  if (!has_module(name)) {
    stop(glue("The module {name} doesn't exists at {module_path}."))
  }
  usethis::use_template(
    template = "module-README.rmd",
    save_as = fs::path(module_path, "README.rmd"),
    data = list(module_path = module_path,
                module = name),
    package = "dymiumCore"
  )
}

check_pkg_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
   stop(glue("Package '{pkg}' required. Please install before re-trying."))
  }
}

has_module <- function(name) {
  path <- fs::path("modules", name)
  fs::file_exists(path)
}
