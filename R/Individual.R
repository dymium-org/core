#' @title Individual class
#' @description
#' Create Individual class, extended Agent class.
#'
#' @usage NULL
#' @format [R6::R6Class] object
#' @include Agent.R
#'
#' @section Construction:
#'
#' ```
#' Ind <- Individual$new()
#' ```
#'
#' * .data::[data.table::data.table]\cr
#'   Microdata of Individuals.
#'
#' * id_col::`character(1)`\cr
#'   Name of the id colum in `.data`
#'
#' @section Public Fields:
#'
#'  * `history`::[History]\cr
#'
#' @section Public Methods:
#'
#'  Inherits all methods from [Agent] and the following..
#'
#'  * `initialise_data(data, id_col = "pid", hid_col)`\cr
#'  ([data.table::data.table()], `character(1)`, `character(1)`) -> `NULL`\cr
#'  Add data.
#'
#'  * `get_father(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Get father ids of the corresponding individual ids.
#'
#'  * `get_mother(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Get mother ids of the corresponding individual ids.
#'
#'  * `get_partner(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Get partner ids of the corresponding individual ids.
#'
#'  * `get_children(ids)`\cr
#'  (`integer()`) -> `list()`\cr
#'  Returns a list of children that has the same length as `ids`. NAs are returned
#'  where individual agents have no children. Please note that only the number of children
#'  that are alive will be returned. To get the number of children an individual ever have
#'  during the simulation you may need to implement a function that look at the historical
#'  records of the individual to determine that.
#'
#'  * `get_n_children(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Returns the number of children each individual agent in the given ids has.
#'
#'  * `get_n_resident_children(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Returns the number of children that live in the same household as the individuals
#'  in `ids`.
#'
#'  * `living_together`\cr
#'  (`integer()`, `inteter()`) -> `logical()`\cr
#'  For checking if two agents are residing in the same household. If either of
#'  the ids is NA the function will return NA for those indexes.
#'
#'  * `get_household_ids(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Returns household ids of the individual in `ids` arg.
#'
#'  * `get_ids_in_hids(hids = NULL)`\cr -
#'  (`integer()`) -> `integer()`\cr
#'  Returns individual ids matches the input `hids`.
#'
#'  * `get_hid_col()`\cr
#'  () -> `character(1)`\cr
#'  Returns household id or `hid` of the individual-object.
#'
#'  * `add_relationship(ids, target_ids, type = c('father', 'mother', 'partner'))`\cr
#'  (`integer()`, `integer()`, `character(1)`)\cr
#'  Return nothing, but it adds relationship of the type specifies in `type` by
#'  the user with the id of `target_ids` to individuals with ids in `ids`.
#'
#'  * `remove_relationship(ids, type = "partner")`\cr
#'  (`integer()`, `character()`)\cr
#'  Note: children and parents can't be removed, only partner can be removed.
#'
#'  * `have_relationship(ids = NULL, type)`\cr
#'  (`integer()`, `character(1)`) -> `logical()`\cr
#'  Check if individuals in `ids` have the relationship of type `type`.
#'
#'  * `remove_household_id(ids)`\cr
#'  (`integer()`)\cr
#'  Remove household ids of individuals in `ids`.
#'
#'  * `add_household_id(ids, hh_ids)`\cr
#'  (`integer()`, `integer()`)\cr
#'  Replace household ids of individuals in `ids`. `ids` and `hh_ids` must be
#'  of the same length.
#'
#'  * `get_parent_hid(ids = NULL)`\cr
#'  (`integer()`) -> [data.table::data.table()]\cr
#'  Returns a data.table with three columns: pid, father_hid and mother_hid.
#'
#'  * `living_with_parents(ids = NULL)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Returns a logical of length `ids` if ids is not NULL else the length will be
#'  equal to the number of rows of the data. The idea is to compare both parents'
#'  household ids with the household id of self. Dead individuals will always return
#'  FALSE as their answer.
#'
#'  * `living_together(self_ids, target_ids)`\cr
#'  (`integer()`, `integer()`) -> `logical()`\cr
#'  Check if two individuals are living together in the same household. Returns
#'  logical vector.
#'
#'  * `have_resident_child(ids = NULL)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Returns a logical of length `ids` if ids is not NULL else the length will be
#'  equal to the number of rows of the data. Resident children are those children
#'  who are living in the same household as their parents.
#'
#'  * `get_ids_from_id_cols(id_cols = NULL, na.rm = TRUE)`\cr
#'  (`character()`, `logical(1)`) -> `character()`\cr
#'  Returns all unique ids in id_cols in a vector, excluding household ids.
#' @export
Individual <- R6::R6Class(
  "Individual",
  inherit = Agent,
  public = list(
    initialise_data = function(.data, id_col = "pid", hid_col) {
      super$initialise_data(.data = .data, id_col = id_col)
      if (!missing(hid_col)) {
        checkmate::assert_names(names(.data), must.include = hid_col)
        private$.hid_col <- hid_col
        lg$info("sets hid_col to: '{private$.hid_col}'")
      }
      invisible()
    },

    data_template = function() {
      data.table(
        pid = integer(),
        hid = integer(),
        # fid = integer(), # firm/work id
        age = integer(),
        sex = character(),
        marital_status = character(),
        partner_id = integer(),
        father_id = integer(),
        mother_id = integer()
      )
    },

    get_household_ids = function(ids) {
      if (missing(ids)) {
        return(self$get_attr(x = private$.hid_col))
      }
      self$get_attr(x = private$.hid_col, ids = ids)
    },

    get_ids_in_hids = function(hids) {
      checkmate::assert_integerish(hids, any.missing = FALSE, lower = 1, null.ok = FALSE)
      if (is.null(self$get_hid_col())) {
        stop("`self$get_hid_col()` returned NULL. There is no household id column.")
      }
      .id_col <- self$get_id_col()
      .hid_col <- self$get_hid_col()
      res <- self$get_data()[get(.hid_col) %in% hids, .SD, .SDcol = c(.id_col, .hid_col)]
      # raise error if there are some non-existed hids
      if (res[, uniqueN(get(.hid_col))] != uniqueN(hids)) {
        missing_hids <- hids[!hids %in% res[, unique(get(.hid_col))]]
        stop("These hids don't exist in data: ", missing_hids)
      }
      res[[.id_col]]
    },

    get_hid_col = function() {
      if (length(private$.hid_col) != 0) {
        return(private$.hid_col)
      }
      self$msg_warn("`hid_col` has not been specified")
      invisible()
    },

    get_ids_from_id_cols = function(id_cols = NULL, na.rm = TRUE) {
      if (is.null(id_cols)) {
        id_cols <- c(self$get_id_col(), IND$ID_COLS)
      }
      checkmate::assert_names(names(self$get_data()), must.include = id_cols)
      checkmate::assert_subset(id_cols, choices = c(self$get_id_col(), IND$ID_COLS), empty.ok = FALSE)
      res <- self$get_data() %>%
        .[, unlist(.SD, use.names = FALSE), .SDcol = id_cols] %>%
        unique()
      if (na.rm) {
        return(res[!is.na(res)])
      }
      res
    },

    add_relationship = function(ids, target_ids, type = c('father', 'mother', 'partner')) {

      type <- match.arg(type)

      # checks
      stopifnot(self$ids_exist(ids = ids))
      if (type == "father") {
        # It is possible that a female give birth without having a partner
        # althought this case maybe rare, let check the statistics and revise
        # this if necessary. But for now this stays like this.
        stopifnot(self$ids_exist(ids = na_omit(target_ids)))
      } else {
        stopifnot(self$ids_exist(ids = target_ids))
      }

      self_idx <- self$get_idx(ids = unique(ids))

      # add relationship
      switch(type,
             "father" = {
               # expect that if emptied == integer(0)
               stopifnot(
                 all(self$get_data(copy = FALSE)[self_idx, is.na(father_id)]),
                 msg = paste0(type,
                              " id should only have one agent id at birth.",
                              " See issue #25")
               )

               self$get_data(copy = FALSE)[self_idx, father_id := target_ids]
             },
             "mother" = {
               # expect that if emptied == integer(0)
               stopifnot(
                 all(self$get_data(copy = FALSE)[self_idx, is.na(mother_id)]),
                 msg = paste0(type,
                              " id should only have one agent id at birth.",
                              " See issue #25")
               )

               self$get_data(copy = FALSE)[self_idx,
                                         mother_id := target_ids]

             },
             "partner" = {
               # expect that if emptied == integer(0)
               if (!all(self$get_data(copy = FALSE)[self_idx, is.na(partner_id)])) {
                 print(self$get_data(copy = FALSE)[self_idx,])
                 stop(paste0(
                   type,
                   " id cannot be overwrite but can be removed.",
                   " See issue #25"
                 ))
               }

               target_idx <- self$get_idx(ids = target_ids)

               # self adds partner
               self$get_data(copy = FALSE)[self_idx,
                                         partner_id := target_ids]

               # partner adds self
               self$get_data(copy = FALSE)[target_idx,
                                         partner_id := ids]

             })

      return(invisible())
    },

    remove_relationship = function(ids, type = c("partner")) {

      type <- match.arg(type)
      stopifnot(self$ids_exist(ids))

      switch(type,
             "partner" = {
               my_partner_ids <-
                 self$get_data(copy = FALSE)[get(self$get_id_col()) %in% ids, unlist(partner_id)]

               # self remove partner
               self$get_data(copy = FALSE)[get(self$get_id_col()) %in% ids,
                                         # TODO: add partner to history before remove
                                         partner_id := NA_integer_]

               # partner removes self
               self$get_data(copy = FALSE)[get(self$get_id_col()) %in% my_partner_ids,
                                         # TODO: add partner to history before remove
                                         partner_id := NA_integer_]
             })

      invisible()
    },

    get_father = function(ids) {
      private$get_relationship(ids, type = "father")
    },

    get_mother = function(ids) {
      private$get_relationship(ids, type = "mother")
    },

    get_partner = function(ids) {
      private$get_relationship(ids, type = "partner")
    },

    get_children = function(ids) {
      pid_col <- self$get_id_col()
      result <-
        private$get_relationship(ids, type = "children") %>%
        dt_group_and_sort(x = ., groupby_col = pid_col, group_col = "child_id", sort_order = ids)
      checkmate::expect_set_equal(ids, result[['sort_col']], ordered = T,
                                  info = "`ids` and the result are not equal.")
      result[["group_col"]]
    },

    get_resident_children = function(ids) {
      pid_col <- self$get_id_col()
      result <-
        private$get_relationship(ids, type = "children") %>%
        .[, living_together := self$living_together(self_ids = get(pid_col), target_ids = child_id)] %>%
        .[living_together == TRUE] %>%
        dt_group_and_sort(x = ., groupby_col = pid_col, group_col = "child_id", sort_order = ids)
      checkmate::expect_set_equal(ids, result[['sort_col']], ordered = T,
                                  info = "`ids` and the result are not equal.")
      result[["group_col"]]
    },

    have_relationship = function(ids, type = private$relationship_types) {

      type <- match.arg(type)

      if (!missing(ids))
        stopifnot(self$ids_exist(ids = ids))

      idx <- self$get_idx(ids = ids)

      result <-
        switch(
          type,
          "partner" = {
            self$get_data(copy = FALSE)[idx, !is.na(partner_id)]
          },
          "father" = {
            self$get_data(copy = FALSE)[idx, !is.na(partner_id)]
          },
          "mother" = {
            self$get_data(copy = FALSE)[idx, !is.na(partner_id)]
          },
          "children" = {
            parent_child_dt <- self$get_children(ids = ids)
            ids %in% parent_child_dt[["id"]]
          }
        )

      stopifnot(all(is.logical(result)))

      result
    },

    remove_household_id = function(ids) {
      stopifnot(self$ids_exist(ids))
      # remove household id
      self$get_data(copy = FALSE)[get(self$get_id_col()) %in% ids,
                                (self$get_hid_col()) := NA_integer_]
      return(invisible())
    },

    add_household_id = function(ids, hh_ids) {
      stopifnot(self$ids_exist(ids))
      stopifnot(all(sapply(hh_ids, function(x)
        length(x) == 1)))
      stopifnot(length(ids) == length(hh_ids))
      stopifnot(is.vector(hh_ids))
      stopifnot(is.numeric(hh_ids))

      if (!is.integer(hh_ids)) {
        hh_ids <- as.integer(hh_ids)
      }

      # add household ids
      # id idx are used to make sure that the args ids and hh_ids are
      # in the same order.
      idx <- self$get_idx(ids = ids)
      self$get_data(copy = FALSE)[idx, (private$.hid_col) := hh_ids]

      return(invisible())
    },

    get_parent_hid = function(ids = NULL) {
      data <- self$get_data()
      # merge hid of mother and father
      father_hid <-
        data[data, .(pid, father_hid = hid), on = .(pid == father_id)]
      mother_hid <-
        data[data, .(pid, mother_hid = hid), on = .(pid == mother_id)]
      parent_hids <-
        father_hid[mother_hid, on = self$get_id_col()]

      if (!is.null(ids)) {
        parent_hids[pid %in% ids]
      } else {
        parent_hids
      }
    },

    living_with_parents = function(ids = NULL) {
      stop("use $living_together instead until this function is fixed.")
      # TODO: potential bugs
      # A case where both parents (who have been divorced and living in different
      # households) are in the matching market at the same time and both needs
      # to check for dependent children there will be more than one instance of
      # their children ids in `ids` arg. The bug only shows when we try to
      # rearrange `data` to match the order of `ids` with `order(match(pid, ids)`
      # this bug is cauaght by the last asserttion statement that
      # `result$pid` and ids are not match.
      #
      # Potential fixes
      # - use merge instead of order
      #
      # filter out dead individuals
      active_ind_ids <- ids[self$is_alive(ids = ids)]

      # get parents' household ids
      parent_hids <- self$get_parent_hid(active_ind_ids)
      data <-
        self$get_data(active_ind_ids)[, .(pid, hid)] %>%
        .[parent_hids, on = self$get_id_col()] %>%
        .[, rowId := 1:.N]

      # check if self household id is the same as either of the parents'
      result <-
        data[, .(living_with_parents = any(hid %in% c(father_hid, mother_hid))),
             by = .(pid, rowId)][order(match(pid, ids)), ]

      # check if there is any dead individuals in `ids`
      if (length(active_ind_ids) != length(ids)) {
        # dead individuals will always return FALSE as their answer
        dead_ind <-
          data.table::data.table(pid = ids[!ids %in% active_ind_ids],
                                 living_with_parents = FALSE)
        result <- rbind(result, dead_ind) %>%
          # sort the order of `result` to match the order of `ids`
          .[list(pid = ids), on = "pid"]
      }

      # check that we get all the result of the active individuals
      stopifnot(all(result$pid == ids))

      return(result$living_with_parents)
    },

    living_together = function(self_ids, target_ids) {
      stopifnot(length(self_ids) == length(target_ids))
      stopifnot(self$ids_exist(na.omit(c(self_ids, target_ids)), include_removed_data = TRUE))
      # merge household id (hid.x) to parents as specified in `ids`
      dat <- data.table(self_id = self_ids, target_id = target_ids)
      hid_col <- self$get_hid_col()
      pid_col <- self$get_id_col()
      ind_data <- self$get_data()[, .SD, .SDcols = c(pid_col, hid_col)]
      result <-
        merge(x = dat, y = ind_data, by.x = "self_id", by.y = pid_col, all.x = T, sort = FALSE) %>%
        merge(x = ., y = ind_data, by.x = "target_id", by.y = pid_col, all.x = T, sort = FALSE) %>%
        .[, hid.x == hid.y]
      stopifnot(length(result) == length(self_ids))
      result
    },

    have_resident_child = function(ids) {
      stop("Has not been implemented yet.")
    }

  ),

  private = list(
    # private -----------------------------------------------------------------
    .hid_col = character(),
    relationship_types = c("father", "mother", "partner", "children"),
    # ***********************************************************
    # get_relationship(ids, type):
    #    return a list of ids those match the relationship type of the input
    # ***********************************************************
    get_relationship = function(ids, type = private$relationship_types) {
      type = match.arg(type)
      if (missing(ids)) {
        idx <- TRUE # this returns the entire data.table
      } else {
        stopifnot(self$ids_exist(ids))
        idx <- self$get_idx(ids = ids)
      }

      .get_children <- function(ids) {
        checkmate::assert_integerish(ids, lower = 1, any.missing = FALSE, unique = TRUE)
        pid_col <- self$get_id_col()
        result <-
          self$get_data() %>%
          .[mother_id %in% ids | father_id %in% ids, ] %>%
          .[, .SD, .SDcols = c(pid_col, "father_id", "mother_id")] %>%
          data.table::melt(., id.vars = pid_col, value.name = "parent_id") %>%
          # remove parent_id that does not exist in `ids`
          .[parent_id %in% ids] %>%
          .[, .SD, .SDcols = c(pid_col, "parent_id")] %>%
          data.table::setnames(., old = c(pid_col, "parent_id"), new = c("child_id", pid_col)) %>%
          # children of lone parents will have the missing parents as NA
          # hene we want to filter those NA parent ids out
          .[!is.na(get(pid_col))]
        merge(result, data.table(id = ids), by.x = pid_col, by.y = "id", all.y = T)
      }

      switch(type,
             "father" = {return(self$get_data(copy = FALSE)[idx, father_id])},
             "mother" = {return(self$get_data(copy = FALSE)[idx, mother_id])},
             "partner" = {return(self$get_data(copy = FALSE)[idx, partner_id])},
             "children" = {return(.get_children(ids))})
    }
))
