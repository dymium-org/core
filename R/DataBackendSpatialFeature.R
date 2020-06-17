#' @title sf::sf as data backend
#'
#' @description
#'
#' For storing spatial data. The data is stored as a data.table but has methods
#' to transform the object back to its intiail sf format.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackendDataTable]<-[DataBackendDataFrame]<-[DataBackend].
#' @include DataBackendDataFrame.R
#'
#' @section Construction:
#' ```
#' x <- DataBackendSpatialFeature$new(data)
#' ```
#'
#' * `data` :: [sf::sf]\cr
#'   The input must be an object of class [sf::sf] (Simple feature) from the sf package.
#'
#' * `key` :: `NULL`|`character(1)`\cr
#'  The key of `.data`.
#'
#' @section Fields:
#'
#'  * `NULL`\cr
#'
#' @section Public Methods:
#'
#'  * `add(.data)`\cr
#'  (a [sf::sf] data.frame) -> `NULL`\cr
#'  Add data.
#'
#'  * `get_sf()`\cr
#'  Get data as an [sf::sf] object.
#'
#' @note
#'
#' This class could be furthur improved by using a data.table to store non-spatial
#' information of the data object and another just to store the spatial features. Both
#' objects are linkable through a id column. To make sure both spatial and non-spatial
#' information always match we can invoke a consistency check between the two everytime
#' the user or its methods try to access private$.data.
#'
#' @export
DataBackendSpatialFeature <- R6::R6Class(
  classname = "DataBackendSpatialFeature",
  inherit = DataBackendDataTable,
  public = list(
    initialize = function(.data, key = NULL) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        message("DataBackendSpatialFeature requires the sf package. Installing from https://cloud.r-project.org...")
        install.packages("sf", repos = "https://cloud.r-project.org")
      }
      stopifnot(inherits(.data, "sf"))
      private$.sf_attrs <- .data[0, ]
      super$initialize(as.data.table(.data), key)
    },

    get_sf = function() {
      private$.get_sf_data()
    },

    view = function(interactive = FALSE) {

      checkmate::assert_logical(interactive, any.missing = FALSE, len = 1, null.ok = FALSE)

      if (interactive && requireNamespace("mapview", quietly = TRUE)) {
        return(mapview::mapview(private$.get_sf_data(), legend = FALSE, layer.name = "layer"))
      }

      if (interactive) {
        lg$error("Interactive view requires the `mapview` package. \\
                 To install use `install.packages('mapview') then try this again.")
        lg$warn("Returning a static map instead.")
      }

      return(
        graphics::plot(
          x = private$.get_sf_data(),
          col = "grey",
          border = 'black',
          axes = TRUE,
          main = glue::glue("{nrow(private$.data)} zones")
        )
      )
    }
  ),

  private = list(
    .sf_data = NULL,
    .sf_attrs = NULL,
    .get_sf_data = function() {
      if (!"geometry" %in% names(private$.data)) {
        stop("`geometry` variable is missing from the data.")
      }
      sf_data <- sf::st_as_sf(private$.data)
      if (sf::st_crs(sf_data) != sf::st_crs(private$.sf_attrs)) {
        sf::st_set_crs(sf_data, sf::st_crs(private$.sf_attrs))
      }
      return(sf_data)
    }
  )
)
