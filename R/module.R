dymiumModulesRepo <- "dymium-org/dymiumModules"

#' Download a module from a repository.
#'
#' @description
#'
#' Download and extract a module to the 'modules' folder in the active working directory.
#' If you are using an RStudio project then it will be saved in the 'modules' folder
#' of your project. If the 'modules' folder does not exist, it will be created.
#'
#' @param name name of the module.
#' @param version the version of the module to download. If not given, the latest version will
#' be downloaded.
#' @param force A logical value. force download even though the module already exists locally.
#' @param remove_download a logical value whether to delete the downloaded zip file or not.
#' @param .basedir :: `character(1)`\cr
#' The base directory that the downloaded module will be saved at. [here::here()] is
#' used to provide the default value which is is the root folder of the active RStudio project.
#' @template param_modulerepo
#'
#' @return path to the module.
#'
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   # download an test module.
#'   download_modules('test', version = '0.0.1')
#' }
#'
download_module <- function(name, repo = dymiumModulesRepo, version, force = FALSE, remove_download = FALSE) {
  modules_path <- fs::path("modules")
  usethis::use_directory('modules')
  all_module_files <- get_all_module_files()
  all_versions <- extract_module_versions(name = name, filenames = all_module_files)
  if (missing(version)) {
    cli::cli_alert_warning("The argument 'version' was not specified. The latest \\
                            version of the module '{.strong {name}}' will be downloaded.")
    version <- all_versions[[length(all_versions)]]
    cli::cli_alert_info("The latest version of module '{.strong {name}}' is '{version}'.")
  } else {
    check_version(version, all_versions, name = name)
  }

  module_filename <- paste0(name, "_", version)
  if (isFALSE(force) && fs::dir_exists(fs::path(modules_path, module_filename))) {
    cli::cli_alert_danger("'{.strong {module_filename}}' already exists in \\
                          directory: '{modules_path}'. Since `force` is FALSE \\
                          the module will not be overwritten.")
  }
  if (force) {
    cli::cli_alert_warning("Force overwriting the module if already exists.")
  }
  module_download_url <-
    paste0("https://github.com/", repo, "/raw/master/modules/", name, "/", module_filename, ".zip")
  tmp_module_path <- fs::path("modules", "temp-module.zip")
  utils::download.file(url = module_download_url, destfile = tmp_module_path, overwrite = FALSE, cacheOK = FALSE)
  utils::unzip(zipfile = tmp_module_path, exdir = modules_path, overwrite = FALSE)
  if (remove_download) {
    fs::file_delete(path = tmp_module_path)
  }
  cli::cli_alert_success("'{.strong {name}}' module version {.strong {version}} was successfully downloaded \\
                         and added to directory: '{modules_path}'")
  invisible()
}

#' Check if a module exists in a remote repository
#'
#' @param name name of the module to check.
#' @template param_modulerepo
#'
#' @return a logical value.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   check_module('test')
#' }
check_module <- function(name, repo = dymiumModulesRepo) {
  checkmate::assert_character(name, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::test_subset(name, choices = get_modules(), empty.ok = FALSE)
}

#' Check the existence of a module version.
#'
#' @param name name of the module.
#' @param version a character. For example, if you would like to check
#' for version 0.1.0 type it as a character '0.1.0'.
#'
#' @template param_modulerepo
#'
#' @return a logical value
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   check_module_version('test', version = '0.0.1')
#' }
check_module_version <- function(name, repo = dymiumModulesRepo, version) {
  all_versions <- get_module_versions(name = name, repo = repo)
  res <- checkmate::test_subset(version, choices = all_versions)
  return(check_version(version, all_versions, name = name))
}

check_version <- function(x, versions, name = "this") {
  res <- checkmate::test_subset(x, choices = versions)
  if (!res) {
    cli::cli_alert_warning("{.strong {name}} module doesn't have a version {.strong {x}}.")
    if (length(versions) != 0) {
      cli::cli_alert_info("These are the available versions of {.strong {name}} module:")
      cli::cli_li(items = sort(versions))
    }
    stop(glue::glue("The requested version of {name} module doesn't exist."))
  }
  return(res)
}

extract_module_versions <- function(name, filenames) {
  versions <- .filter_zip_versions(x = filenames, name = name)
  if (length(versions) == 0) {
    stop(glue("'{name}' module has no available versions."))
  }
  return(versions)
}

#' Get all version numbers of a module
#'
#' @param name name of the module.
#' @template param_modulerepo
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_module_versions("demography")
#' }
get_module_versions <- function(name, repo = dymiumModulesRepo) {
  module_files <- get_module_files(name = name, repo = repo)
  versions <- .filter_zip_versions(x = module_files, name = name)
  if (length(versions) == 0) {
    stop(glue("'{name}' module has no available versions."))
  }
  return(versions)
}

.filter_zip_versions <- function(x, name) {
  x %>%
    gsub(pattern = paste0("modules/", name, "/"), replacement =  "", x = .) %>%
    grep(paste0("^", name, ".+.zip"), x = ., value = TRUE) %>%
    gsub(pattern = paste0(name, "_"), replacement = "", x = .) %>%
    gsub(pattern = "\\.zip", replacement = "", x = .) %>%
    sort()
}

#' Get the names of available modules from a remote repository
#'
#' @template param_modulerepo
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_modules()
#' }
get_modules <- function(repo = dymiumModulesRepo) {
  all_files <- get_all_module_files(repo = repo)
  available_modules <- grep(paste0("^modules/"), all_files, value = TRUE) %>%
    # replace everything after the second back slash with -1
    gsub("^([^/]*/[^/]*/).*$","-1", .) %>%
    gsub("modules/", "", .) %>%
    grep(pattern = "-1", x = ., value = TRUE, invert = TRUE)
  return(available_modules)
}

#' Get all files from a module
#'
#' @param name name of the module.
#' @template param_modulerepo
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_module_files("demography")
#' }
#'
get_module_files <- function(name, repo = dymiumModulesRepo) {
  checkmate::assert_character(name, len = 1, null.ok = FALSE, any.missing = FALSE)
  if (!checkmate::test_subset(name, choices = get_modules(repo = repo), empty.ok = FALSE)) {
    stop(glue::glue("'{name}' module doesn't exists in the '{repo}' repository."))
  }
  module_files <- get_all_module_files(repo = repo) %>%
    grep(name, x = ., value = T)
  return(module_files)
}

#' Get all files from all modules in a repository.
#'
#' @template param_modulerepo
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_all_module_files("dymium-org/dymiumModules)
#' }
#'
get_all_module_files <- function(repo = dymiumModulesRepo) {
  checkmate::assert_character(repo, len = 1, null.ok = FALSE, any.missing = FALSE)
  apiurl <- paste0("https://api.github.com/repos/", repo, "/git/trees/master?recursive=1")
  pat <- Sys.getenv("GITHUB_PAT")
  request <- if (identical(pat, "")) {
    httr::GET(apiurl)
  } else {
    cli::cli_alert_info("Using GitHub PAT from envvar GITHUB_PAT")
    httr::GET(apiurl, config = list(httr::config(token = pat)))
  }
  request <- httr::GET(apiurl)
  httr::stop_for_status(request)
  all_module_files <- unlist(lapply(httr::content(request)$tree, "[", "path"), use.names = FALSE) %>%
    grep("^modules/", x = ., value = TRUE)
  return(all_module_files)
}
