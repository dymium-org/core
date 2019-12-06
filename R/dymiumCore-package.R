#' \code{dymiumCore} package
#'
#' Dynamic microsimulation framework for integrated urban models.
#'
#' See the README on
#' \href{https://github.com/dymium-org/dymiumCore#readme}{GitHub}
#'
#' @docType package
#' @name dymiumCore
#' @import R6
#' @import data.table
#' @import assertthat
#' @import glue
#' @importFrom fs path
#' @importFrom usethis use_template use_directory
#' @importFrom here here
#' @importFrom magrittr freduce functions set_colnames
#' @importFrom furrr future_map_dfr future_options
#' @importFrom purrr flatten_int map map_lgl map2 map2_int map2_dfr walk walk2
#' @importFrom lest case_when
#' @importFrom matchingR galeShapley.marriageMarket galeShapley.collegeAdmissions
#' @importFrom utils packageVersion
#' @keywords internal
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
