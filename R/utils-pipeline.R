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
#' if (FALSE) {
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
