#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_world_obj <- function(path) {
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
  container_names <- names(world$containers)
  obj_names <- names(world$Cont)[!names(world$Cont) %in% container_names]
  obj_classes <- sapply(world$Cont[obj_names], class)
  obj_data <- lapply(world$Cont[obj_names], function(.x) {
    if (checkmate::test_r6(.x, classes = "Entity")) {
      return(.x$get_data())
    }
    if (checkmate::test_r6(.x, classes = "Model")) {
      return(.x$model)
    }
    if (checkmate::test_r6(.x, classes = "Target")) {
      return(.x$data)
    }
  })
  obj_dt <- data.table(name = obj_names, class = obj_classes, obj = obj_data)
  saveRDS(object = obj_dt, file = file, compress = compress)
  invisible()
}
