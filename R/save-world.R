#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_world_obj <- function(path) {
  stop("not done yet")
  obj_dt <- readRDS(path)
  world <- World$new()
  for (i in 1:nrow(obj_dt)) {
    world$add(x = obj_dt[['obj']][i], name = obj_dt[['name']][i])
  }
  browser()
  invisible(world)
}

#' Title
#'
#' @param x
#' @param file
#' @param compress
#'
#' @return
#' @export
#'
#' @examples
write_world_obj <- function(x, file, compress = TRUE) {
  checkmate::assert_r6(x, classes = "World")
  checkmate::assert_directory_exists(dirname(file), access = "rwx")
  saveRDS(object = extract_data(x), file = file, compress = compress)
  invisible()
}
