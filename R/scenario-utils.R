#' @title save dymium object
#'
#' @description
#' This function save the input object and returns the object. This is designed to
#' work in a dymium pipeline. Note that this function can overwrite saved objects
#' but not existing folders.
#'
#' @param x a dymium class object
#' @param path a character, containing a path. This is only needed if the object
#'  doesn't have a config path. Otherwise it will be save in the standard dymium
#'  output path, which is `<config-path>/outputs/iter-<number>`.
#' @param .overwrite `logical(1)`
#' @return the input `object`` invisibly
dm_save <- function(x, path = getOption('dymium.output_dir'), .overwrite = FALSE) {
  stop('this needs fixing')
  # argument  checks
  stopifnot(is_dymium_class(x))
  default_output_foldername <- 'outputs'
  if (is.null(path)) {

      # determine path to base folder
      config_path <- x$config$path
      if (is.null(config_path)) {
        stop(glue::glue("No `path` were given nor the `object` was created using \\
                      a config file. Please provide a `path` for the object to \\
                      be saved at."))
      }
      path <- config_path
      lg$info("Extracted `path` from the `object`.")

      # determine output folder name
      config_output_foldername <- basename(x$config$settings$output_dir)
      if (!is.null(config_output_foldername)) {
        default_output_foldername <- config_output_foldername
      }

  }

  # fix dirname, read dirname's description about path having no separator.
  if (dirname(path) == '.') {
    path_dir <- path
  } else {
    path_dir <- dirname(path)
  }
  folder_name <- glue::glue("iter-{dymiumCore:::.DMevn$sim_time}")
  folder_path <- glue::glue("{path_dir}/{default_output_foldername}/{folder_name}")
  checkmate::assert_access(path_dir, access = 'rw')

  # create a folder
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = T)
    lg$info("Created a folder at {folder_path}")
  }

  # save object in the folder named as its current iteration number eg: `iter-0`
  object_name <- class(x)[[1]]
  object_path <- glue::glue("{folder_path}/{object_name}.rds")

  if (file.exists(object_path)) {
    if (.overwrite) {
      lg$warn("overwriting '{object_path}'")
    } else {
      stop(glue::glue("File exists at '{object_path}' and is not allowed to be overwritten."))
    }
  }

  saveRDS(x, file = object_path)

  # return object
  invisible(x)
}


#' set and get output path
#'
#' @param x
#'
#' @return return the current output path
#' @export
#'
#' @examples
#' dm_outputpath()
dm_output_dir <- function(x) {
  if (missing(x)) {
    return(getOption("dymium.output_dir"))
  }
  checkmate::assert_directory_exists(x, access = "rw")
  message("setting output directory ('dymium.output_dir') to: ", x)
  options(dymium.output_dir = x)
}


#' set and get input path
#'
#' @param x
#'
#' @return the current input path
#' @export
#'
#' @examples
dm_input_dir <- function(x) {
  if (missing(x)) {
    return(getOption("dymium.input_dir"))
  }
  checkmate::assert_directory_exists(x, access = "rw")
  message("setting input directory ('dymium.input_dir') to: ", x)
  options(dymium.input_dir = x)
}

#' set an active scenario directory
#'
#' @description
#'
#' This function sets the current scenario directory and creates input and output
#' folders with they do not exist. However, if the `overwrite` argument is set to
#' `TRUE` this will overwrite the existing output folder (scenario-dir/outputs).
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dm_scenario_dir <- function(dir, overwrite = FALSE) {

  if (missing(input_dir) && missing(output_dir)) {
    .dirs <- list(output_dir = getOption('dymium.output_dir'),
                  input_dir = getOption('dymium.input_dir'))
    message(glue::glue("
    *----- dymium's options -----*
    output_dir: {.dirs[['output_dir']]}
    input_dir: {.dirs[['input_dir']]}
    *----- dymium's options -----*
                   "))
    return(invisible(.dirs))
  }

  if (!missing(input_dir)) {
    dm_input_dir(input_dir)
  }

  if (!missing(output_dir)) {
    dm_output_dir(output_dir)
  }

}
