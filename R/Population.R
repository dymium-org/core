#' @title Population
#'
#' @description
#'
#' A [Container] specifically made for a [Individual] object and a [Household] object.
#' It also contains methods that modify both if its contained objects simutaneously.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Container]<-[ContainerGeneric]<-[Generic].
#' @include Individual.R
#' @include Household.R
#'
#' @section Construction:
#'
#' ```
#' Pop <- Population$new()
#' ```
#'
#' * ind_data::[data.table::data.table]\cr
#'   Microdata of Individuals/Persons.
#'
#' * hh_data::[data.table::data.table]\cr
#'   Microdata of Households.
#'
#' * pid_col::`character(1)`\cr
#'   Individual/Person id column in `ind_data`.
#'
#' * hid_col::`character(1)`\cr
#'   Hoysehold id column in `hh_data`
#'
#' @section Public Fields:
#'
#' * `ind`:: `NULL` | an [R6::R6Class] object\cr
#'  Shorthand to the [Individual] object.
#'
#' * `hh`:: `NULL` | an [R6::R6Class] object\cr
#'  Shorthand to the [Household] object.
#'
#' @section Public Methods:
#'
#' * `initialise_data(ind_data, hh_data, pid_col, hid_col)`\cr
#'  Load the microdata of individuals and households to construct [Individual] and
#'  [Household] objects.
#'
#' * `add_population(ind_data, hh_data)`\cr
#'  ([data.table::data.table()], [data.table::data.table()])\cr
#'  add a new population. This requires that all individuals `ind_data` belong
#'  to valid households. In the case, that `hh_data` is not provided, household ids of
#'  `ind_data` will be checked against household ids of the existing households inside
#'  the Population object being added to. One may use `pop_register()` to replace
#'  the id columns of the new population with ids that don't exist in the Population object.
#'
#' * `join_household(ind_ids, hh_ids)`\cr
#'  (`integer()`, `integer()`)\cr
#'  Individuals join their new households and the households' affected attributes,
#'  from the joining of new members, will also be updated. Note! All individuals
#'  must not be in any household prior to joining a new one. Individual's
#'  existing household can be removed using `leave_household`.
#'
#' * `leave_household(ind_ids)`\cr
#'  (`integer()`)\cr
#'  Remove the individuals' household ids in ind_ids and update the households'
#'  affected attributes, from members leaving the household.
#'
#' * `remove_emptied_households()`\cr
#'  Remove emptied households.
#'
#' * `remove_population(pid = NULL, hid = NULL)`\cr
#'  (`integer()`, `integer()`)\cr
#'  Remove population from `$ind` and `$hh` of this `Pop` object. If only `hid`
#'  is given all household members of households in `hid` arg will be removed.
#'  To remove only individuals leave `hid` to NULL and specify individuals by their ids
#'  in `pid`.
#'
#' * `get_hhsize(hids = NULL)`\cr
#'  (`integer()` | `NULL`) -> (`integer()`)\cr
#'  Get household size of the households in `hids` if `NULL` then household size
#'  of all households will be returned.
#'
#' * `update_hhsize()`\cr
#'  Update household size.
#'
#' * `update()`\cr
#'  mask all the household update functions that need to be adjust after changes
#'  in household members or in their attributes; such as change in partnership status,
#'  change of income, birth.
#'
#' * `check_unique_id_cols(ind_data, hh_data = NULL)`\cr
#'  ([data.table::data.table()], [data.table::data.table()]) -> `logical(1)`\cr
#'  Check that all id cols of the input data are unique from the existing ids in
#'  their respective objects.
#' @export
Population <- R6Class(
  "Population",
  inherit = Container,
  public = list(
    # public ------------------------------------------------------------------

    ind = NULL,
    hh = NULL,

    initialize = function(ind_data, hh_data, pid_col = NULL, hid_col = NULL){
      self$add(Individual$new(), name = "Individual")
      self$add(Household$new(), name = "Household")

      # make it compatible with old modules
      self$ind <- self$get("Individual")
      self$hh <- self$get("Household")

      if (!missing(ind_data) & !missing(hh_data)) {
        self$initialise_data(ind_data, hh_data, pid_col, hid_col)
      }

      invisible()
    },

    initialise_data = function(ind_data, hh_data, pid_col = NULL, hid_col = NULL) {
      # automatically figure out pid col
      if (is.null(pid_col)) {
        if ("pid" %in% names(ind_data)) {
          lg$info("`pid_col` is not given. Use 'pid' as id col of ind_data")
          pid_col <- "pid"
        } else {
          stop("`pid_col` is not given.")
        }
      }
      # automatically figure out hid col
      if (is.null(hid_col)) {
        if ("hid" %in% names(hh_data)) {
          lg$info("`hid_col` is not given. Use 'hid' as id col of hh_data")
          hid_col <- "hid"
        } else {
          stop("`hid_col` is not given.")
        }
      }
      checkmate::assert_names(names(ind_data), must.include = hid_col)
      if (!checkmate::test_set_equal(unique(ind_data[[hid_col]]), hh_data[[hid_col]])) {
        stop(
          glue::glue(
            "Some ids in `hid_col` are not linkable between `ind_data` or `hh_data`. \\
             Please check for missing ids."
          )
        )
      }
      if (!"hhsize" %in% names(hh_data)) {
        lg$warn("Creating `hhsize` as it is not provided with `hh_data`.")
        hhsize_dt <- ind_data[, .(hhsize = .N), by = c(hid_col)]
        hh_data <- hh_data[hhsize_dt, , on = c(hid_col)]
      }
      stopifnot(nrow(ind_data) == hh_data[, sum(hhsize)])
      self$get("Individual")$initialise_data(ind_data, id_col = pid_col, hid_col = hid_col)
      self$get("Household")$initialise_data(hh_data, id_col = hid_col)
      invisible()
    },

    add_population = function(ind_data, hh_data) {
      # only add if there the population object is not empty.
      if (self$get("Individual")$n() == 0) {
        stop("New population data cannot be added to an empty \\
              population object. Please use `Pop$initialise_data()` \\
              method to populate the data fields first.")
      }
      checkmate::assert_data_frame(ind_data, null.ok = FALSE)
      if (!is.data.table(ind_data)) {
        ind_data <- data.table::copy(ind_data)
      }
      hid_col <- self$get("Individual")$get_hid_col()
      # check that all individuals belong to valid households
      if (missing(hh_data)) {
        # check that all individuals belong to existing households
        stopifnot(hid_col %in% names(ind_data))
        newdata_hids <- ind_data[[hid_col]]
        if (!self$get("Household")$ids_exist(newdata_hids)) {
          stop("Not all household ids of the new individual data exist")
        }
      } else {
        # add both household and individual data
        checkmate::assert_data_frame(hh_data, null.ok = FALSE)
        if (!is.data.table(hh_data)) {
          hh_data <- data.table::copy(hh_data)
        }
        # create household size column is missing
        if (!'hhsize' %in% names(hh_data)) {
          hh_data[, hhsize := NA_integer_]
        }
        # check that all individuals belong to households in hh_data
        stopifnot(all(unique(ind_data[[hid_col]]) %in% hh_data[[hid_col]]))
        # add new household agents
        self$get("Household")$add_new_agents(.data = hh_data)
      }
      # add ind_data to the population object
      self$get("Individual")$add_new_agents(.data = ind_data)
      self$update()
      invisible()
    },

    join_household = function(ind_ids, hh_ids) {
      Ind <- self$get(Individual)
      Hh <- self$get(Household)
      stopifnot(Ind$ids_exist(ids = ind_ids))
      stopifnot(Hh$ids_exist(ids = hh_ids))
      # make sure all individuals in ind_ids don't have hid
      all_hids_are_na <-
        all(is.na(Ind$get_attr(x = Ind$get_hid_col(), ids = ind_ids)))
      if (!all_hids_are_na) {
        stop("Not all individuals in ind_ids have left their households.")
      }
      # update hid for individidual in ind_ids
      Ind$add_household_id(ids = ind_ids, hh_ids = hh_ids)
      add_history(entity = Ind, ids = ind_ids, event = EVENT$JOINED_HOUSEHOLD)
      # update household attributes
      self$update()
      invisible()
    },

    leave_household = function(ind_ids) {
      # check that ids in ind_ids and their household ids exist
      stopifnot(self$get("Individual")$ids_exist(ids = ind_ids))
      stopifnot(self$get("Household")$ids_exist(ids = self$get("Individual")$get_household_ids(ids = ind_ids)))
      # leave household
      self$get("Individual")$remove_household_id(ids = ind_ids)
      add_history(entity = self$get("Individual"),
                  ids = ind_ids, event = EVENT$LEFT_HOUSEHOLD)
      # households update themselves
      self$update_hhsize()
      invisible()
    },

    remove_emptied_households = function() {
      hhsize_dt <- self$get_hhsize()
      hids_hhsize_0 <- hhsize_dt[is.na(hhsize), get(self$get("Household")$get_id_col())]
      self$get("Household")$remove(ids = hids_hhsize_0)
    },

    remove_population = function(pid = NULL, hid = NULL) {
      checkmate::assert(
        checkmate::check_integerish(pid, lower = 1, any.missing = FALSE),
        checkmate::check_integerish(hid, lower = 1, any.missing = FALSE),
        combine = "or"
      )
      if (!is.null(hid)) {
        member_ids <- self$get("Individual")$get_ids_in_hids(hids = hid)
        self$get("Individual")$remove(ids = member_ids)
        self$get("Household")$remove(ids = hid)
      }
      if (!is.null(pid)) {
        self$get("Individual")$remove(ids = pid)
      }
    },

    check_hhsize = function() {
      n_individuals <- self$get("Individual")$n()
      n_members_in_households <- self$get_sum_hhsize()
      n_households <- self$get("Household")$n()
      n_non_emptied_households <- sum(self$get_hhsize() != 0)
      n_emptied_households <- n_non_emptied_households - n_non_emptied_households
      if (n_households != n_non_emptied_households) {
        stop(glue::glue("Emptied households exist. There are {n_emptied_households} \\
                        empied households."))
      }
      lg$info("check_hhsize: returns consitence is true.")
      return(invisible(list(
        n_inds = n_individuals,
        n_members_in_households = n_members_in_households,
        n_households = n_households,
        n_non_emptied_households = n_non_emptied_households
      )))
    },

    check_unique_id_cols = function(ind_data, hh_data) {
      checkmate::assert_data_table(ind_data, null.ok = FALSE)
      # specify id cols to be checked
      pid_cols <- c(self$get("Individual")$get_id_col(), IND$ID_COLS)
      hid_col <- self$get("Household")$get_id_col()
      # extract all ids
      ind_data_pids <-
        ind_data[, unlist(lapply(.SD, unlist)), .SDcol = pid_cols] %>%
        unique() %>%
        .[!is.na(.)]
      # check uniqueness
      if (self$get("Individual")$ids_exist(ind_data_pids, by_element = FALSE)) {
        stop("There are ids that exist in data already.")
      }
      # if no hh_data is given then all household id should be NA
      if (missing(hh_data))  {
        if (!all(is.na(ind_data[[hid_col]]))) {
          stop(glue::glue("Not all household ids are NAs. When hh_data is not \\
                            given it is expected that individuals in ind_data will \\
                            join existing households hence all their household id \\
                           which in this case is `{hid_col}` should all be NAs."))
        }
      }
      # for household id (hid)
      if (!missing(hh_data)) {
        checkmate::assert_data_table(hh_data, null.ok = FALSE)
        # extract all ids
        hh_data_hids <-
          hh_data[, unlist(.SD, use.names = FALSE), .SDcols = hid_col] %>%
          .[!is.na(.)]
        ind_data_hids <-
          ind_data[, unlist(.SD, use.names = FALSE), .SDcols = hid_col] %>%
          unique() %>%
          .[!is.na(.)]
        # check uniqueness
        if (self$get("Household")$ids_exist(hh_data_hids)) {
          stop("Some hids in hh_data exist in the household data of the existing population")
        }
        if (self$get("Household")$ids_exist(ind_data_hids)) {
          stop("Some hids in ind_data exist in the household data of the existing population")
        }
      }
      return(TRUE)
    },

    get_hhsize = function(hids = NULL) {
      hid_col <- self$get("Individual")$get_hid_col()
      if (is.null(hids)) {
        hhsize_dt <-
          self$get("Individual")$get_data()[, .(hhsize = .N), by = c(hid_col)]
        hids <- self$get("Household")$get_ids()
      } else {
        stopifnot(self$get("Household")$ids_exist(hids))
        hhsize_dt <-
          self$get("Individual")$get_data() %>%
          .[get(hid_col) %in% hids, .(hhsize = .N), by = c(hid_col)]
      }

      # make sure all hids in the household object are get returned
      # eventhough no individual agents belong to those household agents.
      hhsize_dt <-
        merge(
          x = data.table::data.table(hid = hids),
          y = hhsize_dt,
          by.x = "hid",
          by.y = hid_col,
          all.x = T, # this make sure all hids get returned
          sort = FALSE
        ) %>%
        # replace NAs with 0s
        .[is.na(hhsize), hhsize := 0]

      return(hhsize_dt[["hhsize"]])
    },

    get_sum_hhsize = function(hids) {
      sum(self$get_hhsize(hids))
    },

    update_hhsize = function() {
      hid_col <- self$get("Household")$get_id_col()
      self$get("Household")$get_data(copy = FALSE)[, hhsize := self$get_hhsize(get(hid_col))]
    },

    update = function() {
      self$update_hhsize()
    print = function() {
      super$print()
      for (e in self$Cont) {
        e$print()
      }
    }
  )
)
