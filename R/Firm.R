#' @title Firm class
#'
#' @description
#' (Not done yet.)
#' A class that contains methods and data of firms in a microsimulation model.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Agent]<-[Entity].
#' @include Entity.R
#'
#' @section Construction:
#' ```
#' Frm <- Firm$new(.data, id_col = "fid")
#' ```
#' * .data::[data.table::data.table]\cr
#'   Microdata of firms.
#'
#' * id_col::`character(1)`\cr
#'   Name of the id colum in `.data`
#'
#' @section Fields:
#'
#'  * `NULL`\cr
#'
#' @section Methods:
#'
#'  * `has_job_offers(ids)`\cr
#'  (`integer()`|`NULL`)->(`logit()`)\cr
#'  Returns a logical vector of the same length as `ids` if it is given. Otherwise,
#'  return a logical vector of the same length as the total number of firms in `Firm`.
#'
#'  * `available_jobs(ids)`\cr
#'  (`integer()`|`NULL`)->(`integer()`)\cr
#'  Returns number of available jobs by firms in ids
#'
#'  * `update_number_of_workers(expression)`\cr
#'
Firm <- R6::R6Class(
  classname = "Firm",
  inherit = Agent,
  public = list(

    initialize = function(.data, id_col) {
      if (!missing(.data)) {
        if (!missing(id_col)) {
          super$initialize(databackend = DataBackendDataTable,
                           .data = .data,
                           id_col = id_col)
        } else {
          stop(glue::glue("To initialise the attribute data of {self$class()} \\
                            the `id_col` argument must be given."))
        }
      }
      invisible()
    },

    initialise_data = function(data, id_col = "fid") {
      super$initialise_data(data, id_col)
    },

    # returns ids of firms with available jobs
    has_job_offers = function(ids) {

      if (!missing(ids)) {
        return(
          self$data$get(copy = FALSE) %>%
            .[employment_size > number_of_workers, self$get_id_col(), with = FALSE] %>%
            .[self$get_idx(ids = ids), ] %>%
            .[[self$get_id_col()]]
        )
      }

      self$data$get(copy = FALSE)[employment_size > number_of_workers, self$get_id_col(), with = FALSE][[self$get_id_col()]]
    },

    # returns number of available jobs by firms in ids_
    available_jobs = function(ids) {
      self$data$get(copy = FALSE)[, employment_size - number_of_workers]
    },

    # update the `number_of_workers` column
    update_number_of_workers = function() {
      stop()
    }

  ),

  active = list(
    data_template = function() {
      data.table(
        fid = integer(),
        zid = integer(),
        industry = character(),
        type = character(), # industrial or commercial
        employment_size = integer(),
        number_of_workers = integer()
      )
    }
  )
)
