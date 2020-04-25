#' Save an instance of Entity or Container.
#'
#' @description
#' This is part of the dm_* family of functions to be used inside a microsimulation
#' pipeline. This saves an instance of Entity or Container to a directory.
#'
#' @param x an instance of [Entity] of [Container].
#' @param dir By default this is set to the active scenario directory which can be
#' viewed by calling `getOption("dymium.output_dir")`. `x` will be saved as a .RDS
#' file with the name being the class name of `x`.
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   # create a World object and assign it as 'world' to the global environment.
#'   create_toy_world()
#'
#'   # this saves the 'world' object to the outputs folder of the active scenario folder.
#'   dm_save(world)
#' }
dm_save <- function(x, dir = getOption("dymium.output_dir")) {
  checkmate::assert(
    checkmate::check_r6(x, c("Container")),
    checkmate::check_r6(x, classes = c("Entity")),
    combine = "or"
  )
  iter <- get("sim_time", envir = .DMevn)
  foldername <- paste0("iter-", iter)
  classname <- class(x)[[1]]
  filename <- paste0(class(x)[[1]], ".rds")
  folderdir <- fs::path(dir, foldername)
  fs::dir_create(dir)
  fs::dir_create(folderdir)
  path <- fs::path(folderdir, filename)
  if (fs::file_exists(path)) {
    cli::cli_alert_warning("File already exists at: {path}")
    cli::cli_alert_warning("Overwriting: {path}")
  }
  saveRDS(x, file = path)
  cli::cli_alert_success("Saved {class(x)[[1]]} to path:{path}")
  invisible()
}

#' Extract data from Entity objects
#'
#' @param x a [World] or [Entity] object.
#'
#' @return a named list of data from their DataBackend objects.
#' @export
#'
#' @examples
#'
#' create_toy_world()
#' extract_data(world)
#' extract_data(world$entities$Individual)
extract_data <- function(x) {
  UseMethod("extract_data", x)
}

#' @export
#' @rdname extract_data
extract_data.World <- function(x) {
  d_lst <- lapply(x$Cont[!names(x$Cont) %in% names(x$containers)], extract_data)
  flatten_names <- names(unlist(d_lst, recursive = FALSE))
  d_lst <- purrr::flatten(d_lst)
  names(d_lst) <- flatten_names
  return(d_lst)
}

#' @export
#' @rdname extract_data
extract_data.Entity <- function(x) {
  d_lst <- lapply(x$database, extract_data)
  flatten_names <- names(unlist(d_lst, recursive = FALSE))
  d_lst <- purrr::flatten(d_lst)
  names(d_lst) <- flatten_names
  return(d_lst)
}

#' @export
#' @rdname extract_data
extract_data.DataBackendDataFrame <- function(x) {
  list(data = x$data, removed_data = x$removed_data)
}

extract_data.Target <- function(x) {
  list(data = x$data)
}

extract_data.Model <- function(x) {
  list(model = x$get(), preprocessing_fnc = x$preprocessing_fn)
}
