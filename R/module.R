#' Create new module
#'
#' @param name module
#' @param event events
#' @param path path to create module
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' create_new_module("populationDynamic", event = c("age", "birth", "death"), path = tempdir())
create_new_module <- function(name, event, path) {
  checkmate::assert_string(name)
  checkmate::assert_character(event, unique = T)
  checkmate::assert_string(path)
  checkmate::assert(.checkNames(name))
  checkmate::assert(.checkNames(event, allow_numbers = T))

  # check required packages are installed
  .req_packages <- c('dymiumCore', 'modules', 'R6', 'here', 'lgr')
  check_result <- .req_packages %in% rownames(utils::installed.packages())
  if (!all(check_result)) {
    stop(glue::glue("Some of the required packages are missing. To use create_new_modules \\
                    these packages need to be installed: {tidied_req_packages}.
                    Please install these missing packages: {missing_packages}",
                    tidied_req_packages = glue::glue_collapse(.req_packages, sep = ", ", last = " and "),
                    missing_packages = glue::glue_collapse(.req_packages[!check_result], sep = ", ", last = " and ")))
  }

  module_name <- name
  module_path <- glue::glue("{path}/{name}")
  checkmate::assert(checkmate::check_path_for_output(module_path))

  # create a folder for module
  message(glue::glue("path to module: {module_path}"))
  message(glue::glue("creating a module folder"))
  dir.create(path = module_path)

  # create constants.R
  message(glue::glue("creating constants.R"))
  cat(make_module_constants(module_path = module_path),
      file = glue::glue("{module_path}/constants.R"))

  # create helpers
  message(glue::glue("creating helpers.R"))
  cat(make_module_helpers(module_path = module_path),
      file = glue::glue("{module_path}/helpers.R"))

  # create logger.R
  message(glue::glue("creating logger.R"))
  cat(make_module_logger(module_name = module_name,
                         module_path = module_path),
      file = glue::glue("{module_path}/logger.R"))


  for (this_event in event) {
    create_new_event(event_name = this_event,
                     module_name = module_name,
                     module_path = module_path,
                     filename = this_event)
  }
  invisible()
}

#' Create a event script from an recommended event template
#'
#' @param event_name `character(1)`\cr
#'  Name of the event to be created. This will also be used as its event script name
#'  (i.e. for example if 'age' was given then the script will be named as 'age.R')
#'  and also as the function name: 'event_(module_name)_(event_name)'.
#' @param module_name `character(1)`\cr
#'  Name of the module, this function will be named in this format:
#'  'event_(module_name)_(event_name)'.
#' @param module_path `character(1)`\cr
#'  Directory where the event script will be created.
#' @param filename `character(1)` or `NULL`\cr
#'  The name of the script to be created. If `NULL` event_name is used as the filename.
#' @param author name of aithor(s)
#' @param author.email author's email
#' @param module.version module version eg: (0.0.x).
#' @param dymiumCore.version version of dymiumCore that was used to create the event.
#' @param .overwrite `logical(1)` default as FALSE.
#'
#' @return Invinsibly returns the path to the generated script.
#' @export
#'
#' @examples
#'
#' create_new_event(event_name = "age",
#'                  module_name = "demography",
#'                  module_path = tempdir())
create_new_event <-
  function(event_name, module_name, module_path, filename = NULL,
           author = NULL, author.email = NULL,
           module.version = "0.0.1",
           dymiumCore.version = utils::packageVersion("dymiumCore"),
           .overwrite = FALSE) {
  checkmate::assert_character(event_name, len = 1)
  checkmate::assert_character(module_name, len = 1)
  checkmate::assert_character(module_path, len = 1)
  checkmate::assert_directory_exists(module_path)
  if (event_name != gsub(" ", "", event_name))
    stop("`event_name` can't contain any spaces")
  if (module_name != gsub(" ", "", module_name))
    stop("`module_name` can't contain any spaces")

  # construct path
  if (is.null(filename)) {
    message(glue::glue("`filename` was not given, using `event_name`: '{event_name} \\
                       as the name of the event script."))
  }

  filename <- ifelse(is.null(filename), event_name, filename)
  if (filename != gsub(" ", "", filename))
    stop("`filename` can't contain any spaces")

  event_path <- paste0(module_path, "/", filename, ".R")
  if (!.overwrite)
    checkmate::assert_path_for_output(event_path)

  # write event template to event_path
  full_event_name <- glue::glue("event_{module_name}_{event_name}")
  message(glue::glue("creating an event template"))
  cat(make_event_recommended_name(event_name, module_name, event_path),
      make_event_imports(module_path = module_path),
      template_event_exports,
      make_event_doc(event_name),
      make_event_run(event_name),
      "# private utility functions (.util_*) -------------------------------------",
      template_event_private_util,
      template_event_transition,
      "# exported utility functions (util_*) -------------------------------------",
      template_event_util,
      sep = "\n", file = event_path)
  message(glue::glue("path to event script: {event_path}"))

  # write a testthat test script
  testthat_path <- glue::glue("{module_path}/tests/testthat")
  test_path <- glue::glue("{testthat_path}/test-{gsub('_','',event_name) %>% tolower()}.R")
  if (!file.exists(testthat_path)) {
    message(glue::glue("make testthat folder: {testthat_path}"))
    dir.create(testthat_path, recursive = T)
  }
  if (!.overwrite)
    checkmate::assert_path_for_output(test_path)
  cat(make_event_test(event_name = event_name,
                      module_name = module_name,
                      test_path = test_path,
                      event_path = event_path),
      file = test_path)
  message(glue::glue("path to test script: {test_path}"))

  invisible(event_path)
  }

make_event_run <- function(event_name) {
  template_event_run %>%
    gsub("\\[event_name\\]", tools::toTitleCase(event_name), x = .)
}

#' @keywords internal
make_event_doc <- function(event_name = "Title") {
  gsub("\\[TITLE\\]", tools::toTitleCase(event_name), template_event_doc)
}

#' @examples make_event_recommended_name("marriage", "demography")
#' @keywords internal
make_event_recommended_name <- function(event_name, module_name, event_path) {
  if (missing(event_path)) {
    event_path <- "path/to/this-module.R"
  }
  event_name <- gsub(" ", "", event_name) %>% tolower()
  module_name <- gsub(" ", "", module_name) %>% tolower()
  rname <- glue::glue("event_{module_name}_{event_name}")
  glue::glue("# It is recommended to assign this module to a variable called: {rname}
              # for example: {rname} <- modules::use('{event_path}')")
}

make_event_test <- function(event_name, module_name, test_path, event_path) {
  testthat_path <- glue::glue("{test_path}/testthat")
  event_name <- gsub(" ", "", event_name) %>% tolower()
  module_name <- gsub(" ", "", module_name) %>% tolower()
  # modify the template
  template_event_test %>%
    gsub("\\[test_path\\]", test_path, x = .) %>%
    gsub("\\[event_path\\]", event_path, x = .) %>%
    gsub("\\[event_function_name\\]", glue::glue("event_{module_name}_{event_name}"), x = .)
}

make_event_imports <- function(module_path) {
  template_event_imports %>%
    gsub("\\[module_path\\]", module_path, x = .)
}

make_testthat_folder <- function(event_name, module_name, test_path) {

}

make_module_constants <- function(module_path) {
  template_module_constants %>%
    gsub("\\[module_path\\]", module_path, x = .)
}

make_module_helpers <- function(module_path) {
  template_module_helpers %>%
    gsub("\\[module_path\\]", module_path, x = .)
}

make_module_logger <- function(module_name, module_path) {
  template_module_logger %>%
    gsub("\\[module_name\\]", module_name, x = .) %>%
    gsub("\\[module_path\\]", module_path, x = .)
}

# .fill_boxes <- function(x) {
#   x %>%
#     gsub("\\[module_name\\]", module_name, x = .) %>%
#     gsub("\\[module_path\\]", module_path, x = .) %>%
# }
