#' @title Toy individual microdata
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
#'
#' @examples
#' toy_individuals
'toy_individuals'

#' @title Toy household microdata
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
#' @examples
#' toy_households
'toy_households'

#' @title Toy dwelling microdata
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
#'   \item{did}{(`integer()`) dwelling id}
#'   \item{hid}{(`integer()`) household id}
#'   \item{zid}{(`integer()`) zone id}
#'   \item{structure}{(`character()`) structure of dwelling}
#'   \item{bedroom}{(`integer()`) number of bedrooms}
#' }
#' @examples
#' toy_dwellings
'toy_dwellings'

#' @title Toy zone data
#'
#' @description
#'
#' Note that this dataset requires the `sf` package to work properly. This
#' dataset contains 11 SA2 zones of Melborune CBD, Australia in a [sf::sf] format.
#'
#' @docType data
#'
#' @format A [sf::sf] data frame with 11 rows and 14 variables:
#' \describe{
#'   \item{zid}{(`integer()`) zone id}
#'   \item{sa2_main}{(`integer()`)}
#'   \item{sa2_5dig11}{(`character()`)}
#'   \item{sa2_name11}{(`character()`)}
#'   \item{sa3_code11}{(`character()`)}
#'   \item{sa3_name11}{(`character()`)}
#'   \item{sa4_code11}{(`character()`)}
#'   \item{sa4_name11}{(`character()`)}
#'   \item{gcc_code11}{(`character()`)}
#'   \item{gcc_name11}{(`character()`)}
#'   \item{ste_code11}{(`character()`)}
#'   \item{ste_name11}{(`integer()`)}
#'   \item{albers_sqm}{(`double()`)}
#'   \item{geometry}{(`list()`)}
#' }
#' @examples
#' toy_zones
'toy_zones'

#' @title Toy transport network data
#'
#' @description
#'
#' Note that this dataset requires the `sf` package to work properly. This
#' dataset contains street network within some SA2 zones of Melborune CBD,
#' Australia in a [sf::sf] format. This network is contained within the boundaries
#' of [toy_zones].
#'
#' @docType data
#'
#' @format A [sf::sf] data frame with 11 rows and 14 variables:
#' \describe{
#'   \item{id}{(`integer()`) zone id}
#'   \item{nb_lanes}{(`integer()`)}
#'   \item{speed}{(`double()`)}
#'   \item{capacity}{(`double()`)}
#'   \item{rd_type}{(`integer()`)}
#'   \item{fnode}{(`double()`)}
#'   \item{tnode}{(`double()`)}
#'   \item{geometry}{(`list()`)}
#'   \item{length}{(`double()`)}
#' }
#' @seealso toy_zones
#' @examples
#' toy_transport_network
'toy_transport_network'
