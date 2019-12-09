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
