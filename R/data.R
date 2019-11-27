#' @title Microdata of individuals
#'
#' @description
#'
#' This dataset contains a toy population of individuals that is linkable with [toy_households]
#' that contains its corresponding toy population of households. This dataset is only to be used for testing
#' dymium packages.
#'
#' @docType data
#'
#' @usage data(toy_individuals)
#' @format A data table with 346 rows and 9 variables:
#' \describe{
#'   \item{pid}{(`integer()`) person id}
#'   \item{hid}{(`integer()`) household id}
#'   \item{age}{(`integer()`) age of the person}
#'   \item{sex}{(`character()`) sex of the person - (male, female)}
#'   \item{marital_status}{(`character()`) - (never married, married, divorced, widowed, not applicable)}
#'   \item{partner_id}{(`integer()`)}
#'   \item{father_id}{(`integer()`)}
#'   \item{mother_id}{(`integer()`)}
#' }
'toy_individuals'

#' @title Toy microdata of households
#'
#' @description
#'
#' This dataset contains a toy population of households that is linked with [toy_individuals]
#' that contains its corresponding toy population of individuals. This dataset is only to be used for testing
#' dymium packages.
#' @docType data
#'
#' @usage data(toy_households)
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{hid}{(`integer()`) household id}
#'   \item{did}{(`integer()`) dwelling id}
#' }
'toy_households'

#' @title Toy microdata of dwellings
#'
#' @description
#'
#' This dataset contains a toy population of dwellings that is linked with [toy_individuals] and [toy_households].
#' This dataset is only to be used for testing dymium packages.
#'
#' @docType data
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{did}{(`integer(1)`) dwelling id}
#'   \item{hid}{(`integer(1)`) household id}
#'   \item{zid}{(`integer(1)`) zone id}
#'   \item{structure}{(`character(1)`) structure of dwelling}
#'   \item{bedroom}{(`integer(1)`) number of bedrooms}
#' }
'toy_dwellings'

#' @title Toy zones
#'
#' @description
#'
#' This dataset contains 11 SA2 zones of Melborune CBD, Australia in a [sf::sf] format.
#'
#' @docType data
#'
#' @format A [sf::sf] data frame with 11 rows and 14 variables:
#' \describe{
#'   \item{zid}{(`integer(1)`) zone id}
#'   \item{sa2_main}{(`integer(1)`)}
#'   \item{sa2_5dig11}{(`character()`)}
#'   \item{sa2_name11}{(`character()`)}
#'   \item{sa3_code11}{(`character()`)}
#'   \item{sa3_name11}{(`character()`)}
#'   \item{sa4_code11}{(`character()`)}
#'   \item{sa4_name11}{(`character()`)}
#'   \item{gcc_code11}{(`character()`)}
#'   \item{gcc_name11}{(`character()`)}
#'   \item{ste_code11}{(`character(1)`)}
#'   \item{ste_name11}{(`integer(1)`)}
#'   \item{albers_sqm}{(`double()`)}
#'   \item{geometry}{(`list()`)}
#' }
'toy_zones'
