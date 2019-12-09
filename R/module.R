dymiumModulesRepo <- "dymium-org/dymiumModules"

#' Download a module from a repository.
#'
#' @param name name of the module.
#' @param version the version of the module to download. If not given, the latest version will
#' be downloaded.
#' @param force A logical value. force download even though the module already exists locally.
#' @template repo-arg
#'
#' @return path to the module.
#'
#' @export
download_module <- function(name, repo = dymiumModulesRepo, version, force = FALSE) {

  # prepare 'modules' folder
  modules_path <- fs::path("modules")
  use_directory('modules_path')

  # check if the request version exists
  check_module_version(name = name, repo = repo, version = version)

  # download
  module_download_url <-
    paste0("https://github.com/", repo, "/raw/master/modules/", name, "/", name, "_", version, ".zip")
  tmp_module_path <- "modules/temp-module.zip"
  utils::download.file(url = module_download_url, destfile = module_path)

  # extract module zip
  utils::unzip(zipfile = module_path, files = modules_path, overwrite = FALSE)

  # remove zip file
  fs::file_delete(path = module_path)

  invisible()
}

#' Check if a module exists in a remote repository
#'
#' @template repo-arg
#'
#' @return a logical value.
#' @export
#'
#' @examples
check_module <- function(name, repo = dymiumModulesRepo) {
  checkmate::assert_character(name, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::test_subset(name, choices = get_modules(), empty.ok = FALSE)
}

#' Check the existence of a module version
#'
#' @param name name of the module.
#' @param version a character. For example, if you would like to check
#' for version 0.1.0 type it as a character '0.1.0'.
#'
#' @template repo-arg
#'
#' @return
#' @export
#'
#' @examples
#'
#' check_module_version('test', version = '0.0.1')
check_module_version <- function(name, repo = dymiumModulesRepo, version) {
  all_versions <- get_module_versions(name = name, repo = repo)
  res <- checkmate::test_subset(version, choices = all_versions)
  if (!res) {
    cli::cli_alert_warning("'{.strong {name}}' module doesn't have a version {.strong {version}}.")
    if (length(all_versions) != 0) {
      cli::cli_alert_info("These are the available versions of '{.strong {name}}' module:")
      cli::cli_li(items = sort(all_versions))
    }
    stop("The request version of the module doesn't exist.")
  }
  return(res)
}

#' Get all version numbers of a module
#'
#' @param name name of the module.
#' @template repo-arg
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' get_module_versions("demography")
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
    gsub(pattern = "\\.zip", replacement = "", x = .)
}

#' Get the names of available modules from a remote repository
#'
#' @template repo-arg
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' get_modules()
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
#' @template repo-arg
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' get_module_files("demography")
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
#' @template repo-arg
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' get_all_module_files("dymium-org/dymiumModules)
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
