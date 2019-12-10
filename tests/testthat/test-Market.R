test_that("initialise", {
  if (FALSE) {
    Mrkt <- Market$new()

    create_toy_world()

    toy_households[, .(choices = sample(toy_dwellings[zid == zid]))]

    # rough codes
    library(collections)
    households <- toy_households[toy_dwellings[, .(did, zid)], on = "did"]
    households <- purrr::map_dfr(1:50, ~ households)[, hid := 1:.N]
    dwellings <- purrr::map_dfr(1:50, ~ toy_dwellings)[, did := 1:.N]
    all_hids <- as.character(households[["hid"]])
    all_zids <- households[["zid"]]
    choicesets <- Dict()

    for (i in 1:nrow(households)) {

      # define variables
      self_hid <- all_hids[i]
      self_zid <- all_zids[i]

      # apply rules
      choiceset <- sample_choice(dwellings[zid == self_zid, did], 5, replace = TRUE)

      # record choiceset
      choicesets$set(key = self_hid, value = choiceset)
    }

    choiceset_dt <- data.table(.key = choicesets$keys(),
                               .value = choicesets$values())
    choiceset_dt <-
      choiceset_dt[, lapply(.SD, unlist), by = .key]

    # mental model
    choicesets <- households$create_choiceset(toy_dwellings, rules)

    scores <- households$evaluate(choicesets, rules)

    matches <- market(households, choicesets, scores)

    households$update_dwelling(matches$hid, matches$did)
  }



})


test_that("initialise2", {

  if (FALSE) {
    # required data
    # - household data
    # - dwelling data
    # - rules or models

    # rough codes
    households <-
      toy_households[toy_dwellings[, .(did, zid)], on = "did"] %>%
      as.data.frame()

    households <- purrr::map_dfr(1:50, ~ households) %>%
      setDT(.) %>%
      .[, `:=`(hid = 1:.N, choiceset = list())]

    dwellings <-
      purrr::map_dfr(1:50, ~ toy_dwellings)[, did := 1:.N]

    all_zids <- households[["zid"]]

    dg <- split(dwellings, dwellings$zid)

    for (i in 1:nrow(households)) {
      self_zid <- all_zids[[i]]
      # .choiceset <- sample(dwellings[zid == self_zid, did], size = 5, replace = FALSE)
      .choiceset <- sample(dg[[self_zid]][["did"]], size = 10, replace = FALSE)
      set(households, i, j = 'choiceset', value = list(.choiceset))
    }

    households <- households[, lapply(.SD, unlist), by = hid]

    households
  }


})
