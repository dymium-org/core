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
#' Population$new(ind_data, hh_data, pid_col, hid_col)
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
#'   Household id column in `hh_data`
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
#' * `add_population(ind_data, hh_data)`\cr
#'  ([data.table::data.table()], [data.table::data.table()])\cr
#'  add a new population. This requires that all individuals `ind_data` belong
#'  to valid households. In the case, that `hh_data` is not provided, household ids of
#'  `ind_data` will be checked against household ids of the existing households inside
#'  the Population object being added to. All records in `ind_data` and `hh_data` will
#'  be assigned new unique ids to make sure that their ids are not a duplicate of
#'  the ids of existing entities of their respective entity type.
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
#'  Remove the household ids of the individuals in ind_ids and update the households'
#'  affected attributes, from their members leaving. Note that, if the
#'  household has no individuals then it will be removed. This will only cause
#'  a problem if all members of two or more households are to swap their households.
#'  There are no good reasons why that case should be allowed anyway.
#'
#' * `remove_emptied_households()`\cr
#'  Remove all emptied households.
#'
#' * `remove_population(pid, hid)`\cr
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
#'  Update household size of all household agents.
#'
#' * `update()`\cr
#'  Masks all the household update functions that need to be adjust after changes
#'  in household members or in their attributes; such as change in partnership status,
#'  change of income, birth.
#'
#' * `check_unique_id_cols(ind_data, hh_data = NULL)`\cr
#'  ([data.table::data.table()], [data.table::data.table()]) -> `logical(1)`\cr
#'  Check that all id cols of the input data are unique from the existing ids in
#'  their respective objects.
#'
#' * `plot_relationship(hid)`\cr
#'  (`integer(1)`)\cr
#'  Plot the relationship network within the household of `hid`.
#' @export
Population <- R6Class(
  "Population",
  inherit = Container,
  public = list(
    # public ------------------------------------------------------------------

    ind = NULL,
    hh = NULL,

    initialize = function(ind_data, hh_data, pid_col = NULL, hid_col = NULL){
      checkmate::assert_data_table(ind_data, min.rows = 1)
      checkmate::assert_data_table(hh_data, min.rows = 1)
      checkmate::assert_character(pid_col, any.missing = FALSE, min.len = 1, unique = T)
      checkmate::assert_character(hid_col, any.missing = FALSE, min.len = 1, unique = T)
      checkmate::assert_names(names(ind_data), must.include = c(pid_col, hid_col))
      checkmate::assert_names(names(hh_data), must.include = hid_col)
      checkmate::assert_integerish(ind_data[[pid_col[1]]], lower = 1, unique = T, all.missing = FALSE)
      checkmate::assert_integerish(hh_data[[hid_col[1]]], lower = 1, unique = T, all.missing = FALSE)

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
      } else {
        checkmate::assert_integerish(hh_data[["hhsize"]],
                                     lower = 1,
                                     any.missing = FALSE,
                                     null.ok = FALSE)
      }

      if (nrow(ind_data) != hh_data[, sum(hhsize)]) {
        stop(glue::glue("The total number of individuals in `ind_data` does not \\
                        equal to the sum of household size (hhsize) of `hh_data`."))
      }

      self$add(Individual$new(ind_data, id_col = pid_col, hid_col = hid_col[1]), name = "Individual")
      self$add(Household$new(hh_data, id_col = hid_col), name = "Household")

      # make it compatible with old modules
      self$ind <- self$get("Individual")
      self$hh <- self$get("Household")

      return(invisible(self))
    },

    add_population = function(ind_data, hh_data = NULL) {

      Ind <- self$get("Individual")
      Hh <- self$get("Household")
      pid_col <- Ind$id_col
      hid_col <- Hh$id_col

      checkmate::assert_data_table(ind_data, min.rows = 1)
      checkmate::assert_character(pid_col, any.missing = FALSE, min.len = 1, unique = T)
      checkmate::assert_names(names(ind_data), must.include = c(pid_col, hid_col))
      checkmate::assert_integerish(ind_data[[pid_col[1]]], lower = 1, unique = T, all.missing = FALSE)

      if (!is.null(hh_data)) {
        checkmate::assert_data_table(hh_data, min.rows = 1)
        checkmate::assert_character(hid_col, any.missing = FALSE, min.len = 1, unique = T)
        checkmate::assert_names(names(hh_data), must.include = hid_col)
        checkmate::assert_integerish(hh_data[[hid_col[1]]], lower = 1, unique = T, all.missing = FALSE)

        if (!checkmate::test_set_equal(ind_data[[hid_col[1]]], hh_data[[hid_col[1]]], fmatch = TRUE)) {
          stop("Not all household ids exist in both `ind_data` and `hh_data`.")
        }

        # add household size
        if (!"hhsize" %in% names(hh_data)) {
          lg$warn("Creating `hhsize` as it is not provided with `hh_data`.")
          hhsize_dt <- ind_data[, .(hhsize = .N), by = c(hid_col)]
          hh_data <- hh_data[hhsize_dt, , on = c(hid_col)]
        } else {
          checkmate::assert_integerish(hh_data[["hhsize"]],
                                       lower = 1,
                                       any.missing = FALSE,
                                       null.ok = FALSE)
        }
        # check hhsize
        if (nrow(ind_data) != hh_data[, sum(hhsize)]) {
          stop(glue::glue("The total number of individuals in `ind_data` does not \\
                        equal to the sum of household size (hhsize) of `hh_data`."))
        }
      }

      # assign new ids
      ind_data <- register(Ind, ind_data)[[1]]
      if (!is.null(hh_data)) {
        pop_data_ls <- register(Hh, ind_data, hh_data)
        ind_data <- pop_data_ls$ind_data
        hh_data <- pop_data_ls$hh_data
      }
      rm(pop_data_ls)

      # add new data
      IndNewData <- DataBackendDataTable$new(ind_data, key = Ind$id_col[[1]])
      Ind$add(.data = IndNewData$data, add_population = TRUE)
      if (!is.null(hh_data)) {
        HhNewData <- DataBackendDataTable$new(hh_data, key = Hh$id_col[[1]])
        Hh$add(.data = HhNewData$data)
      }

      return(invisible(self))
    },

    join_household = function(ind_ids, hh_ids) {
      Ind <- self$get(Individual)
      Hh <- self$get(Household)
      assert_entity_ids(Ind, ind_ids)
      assert_entity_ids(Hh, hh_ids)
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
      self$update()
      invisible()
    },

    remove_emptied_households = function(update_hhsize = TRUE) {
      checkmate::assert_flag(update_hhsize, na.ok = FALSE)
      if (update_hhsize) {
        self$update_hhsize()
      }
      Hh <- self$get("Household")
      hh_with_hhsize_0 <- Hh$get_data()[hhsize == 0, get(Hh$get_id_col())]
      if (length(hh_with_hhsize_0) != 0) {
        self$log(desc = "n_emptied_households_removed", value = length(hh_with_hhsize_0))
        Hh$remove(ids = hh_with_hhsize_0)
      }
      invisible()
    },

    remove_population = function(pid, hid) {

      if (missing(pid) & missing(hid)) {
        stop("`pid` or `hid` or both must be specified.")
      }

      if (!missing(hid)) {
        checkmate::check_integerish(hid, lower = 1, any.missing = FALSE)
        member_ids <- self$get("Individual")$get_ids_in_hids(hids = hid)
        self$get("Individual")$remove(ids = member_ids)
        self$get("Household")$remove(ids = hid)
      }
      if (!missing(pid)) {
        checkmate::check_integerish(pid, lower = 1, any.missing = FALSE)
        self$get("Individual")$remove(ids = pid)
        self$remove_emptied_households(update_hhsize = TRUE)
      }
      invisible()
    },

    check_hhsize = function() {
      n_individuals <- self$get("Individual")$n()
      n_members_in_households <- sum(self$get_hhsize())
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
      if (self$get("Individual")$ids_exist(ind_data_pids)) {
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

    update_hhsize = function() {
      hid_col <- self$get("Household")$get_id_col()
      self$get("Household")$get_data(copy = FALSE)[, hhsize := self$get_hhsize()]
    },

    update = function() {
      self$update_hhsize()
    },

    print = function() {
      super$print()
      for (e in self$Cont) {
        e$print()
      }
    },

    plot_relatioship = function(hid) {
      if (!requireNamespace("visNetwork", quietly = TRUE)) {
        .choice <-
          utils::menu(choices = c("Yes", "No"),
                      title = glue::glue("plot_relationship needs the `visNetwork` package. \\
                                Would you like to download the sf package now?"))
        if (.choice == 1) {
          install.packages("visNetwork", repos = "https://cloud.r-project.org")
        } else {
          stop("The `visNetwork` package is not installed.")
        }
      }

      Ind <- self$get("Individual")
      Hh <- self$get("Household")

      members <- inspect(Hh, hid, Ind, verbose = FALSE)$related_entity

      nodes <- members[, .(
        id = pid,
        label = paste0(pid, ":", age),
        group = sex,
        title = paste0("<p>pid:<b>", pid, "</b><br>",
                       "Age:<b>", age, "</b><br>",
                       "MS:<b>", marital_status, "</b></p>"))]

      edges <-
        rbindlist(list(members[, .(from = pid, to = father_id, label = "father")],
                       members[, .(from = pid, to = mother_id, label = "mother")],
                       members[, .(from = pid, to = partner_id, label = "partner")]))

      visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visEdges(
          shadow = TRUE,
          arrows = list(to = list(
            enabled = TRUE, scaleFactor = 1
          )),
          color = list(color = "lightblue", highlight = "red")
        ) %>%
        visNetwork::visGroups(
          groupname = "female",
          color = "salmon",
          shape = "circle",
          shadow = list(enabled = TRUE)
        ) %>%
        visNetwork::visGroups(
          groupname = "male",
          color = "#97C2FC",
          shape = "circle",
          shadow = list(enabled = TRUE)
        ) %>%
        visNetwork::visLegend(width = 0.2,
                  position = "right",
                  main = "Group") %>%
        visNetwork::visEdges(smooth = FALSE) %>%
        visNetwork::visInteraction(navigationButtons = TRUE)

    })
)
