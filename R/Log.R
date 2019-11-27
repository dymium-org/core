#' @include Generic.R
Log <- R6Class(
  classname = "Log",
  inherit = Generic,
  public = list(
    # public ------------------------------------------------------------------

    # for storing values to be used later
    vars = list(),

    initialize = function(){
    },

    summary = function() {
      private$abstract()
    },

    get_log = function(type = "all"){

      if (type == "all") {
        return(data.table::copy(private$.log))
      }

      if (type == "id") {
        return(data.table::copy(private$.log)[grepl("^id:", var),])
      }

      .extract_value_from_list <- function(dt) {
        dt[, value := sapply(value, function(x) x)]
      }

      if (type %in% c("count", "occ")) {
        dt <- data.table::copy(private$.log) %>%
          .[grepl(pattern = paste0("^", type, ":"), x = var), ]
        if (is.list(dt$value)) {
          # manipulated the column by reference hence no need to
          # assign back (dt <- ..) to dt
          .extract_value_from_list(dt)
        }
        return(dt)
      }

      stop(type, " doesn't match any of {all, count, occ, id}.")

    },

    keep_log = function(var, value, time = .get_sim_time()){
      new_log_entry <- data.table(var = var, value = value, time = time)
      private$.log <- rbind(private$.log, new_log_entry)
      invisible()
    }
  ),

  private = list(
    .log = data.table()
  )
)
