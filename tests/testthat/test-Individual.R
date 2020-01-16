context('individual class')

test_that("individual class's methods", {
  create_toy_population()
  Ind <- pop$get("Individual")

# is_alive ----------------------------------------------------------------

  expect_equal(object = Ind$is_alive(c(1,999999,2)),
               expected = c(TRUE, FALSE, TRUE))

# get_children functions --------------------------------------------------

  rand_ids <- sample(Ind$get_ids(), 10, replace = FALSE)

  expect_true(length(Ind$get_children(ids = c(rand_ids))) == length(rand_ids))

  # NA shouldn't work
  expect_error(Ind$get_children(ids = c(rand_ids ,NA)))

  # id doesn't exist
  expect_error(Ind$get_resident_children(ids = c(rand_ids ,9999999)))

# living_together ---------------------------------------------------------
  expect_is(object = Ind$living_together(c(NA, sample(rand_ids)[2:10]), c(sample(rand_ids)[1:9], NA)),
            class = "logical")
})

test_that("remove_relationship", {
  create_toy_population()

  Ind <- pop$get("Individual")

  ind_with_partners <- Ind$get_ids()[Ind$have_relationship(type = "partner")]

  Ind$remove_relationship(ind_with_partners)

  expect_true(all(is.na(Ind$get_attr("partner_id"))))

  checkmate::expect_set_equal(
    ind_with_partners,
    Ind$get_attr(".past_partner_id")[!is.na(Ind$get_attr(".past_partner_id"))]
  )

})

test_that("add partner" ,{
  # ignore gender and other rules
  create_toy_population()

  ind_ids <- sample(pop$get('Individual')$get_ids(), 100)

  ind_no_partners <- ind_ids[!pop$get('Individual')$have_relationship(ids = ind_ids, type = "partner")]
  proposer_ids <- sample(ind_no_partners, size = length(ind_no_partners) / 2 )
  proposee_ids <- ind_no_partners[!ind_no_partners %in% proposer_ids]
  proposee_ids <- proposee_ids[seq_along(proposer_ids)]

  dt <- data.table(
    proposer_id = proposer_ids,
    proposee_id = proposee_ids
  )

  proposer_idx <- pop$get('Individual')$get_idx(proposer_ids)
  proposee_idx <- pop$get('Individual')$get_idx(proposee_ids)

  pop$get('Individual')$add_relationship(ids = proposer_ids, target_ids = proposee_ids, type = "partner")
  proposer_after <- pop$get('Individual')$get_data()[proposer_idx, ]
  proposee_after <- pop$get('Individual')$get_data()[proposee_idx, ]
  expect_identical(proposer_ids, proposee_after$partner_id)
  expect_identical(proposee_ids, proposer_after$partner_id)
})

test_that("remove partner", {
  create_toy_population()
  ind_ids <- sample(pop$get('Individual')$get_ids(), 100)

  # test remove_relationship
  # the test is written to test if to see if after the remove_partner is ran
  # both individuals in ind_ids and their partners have no partner_id, which is
  # equipvalent to integer(0)
  ind <- pop$get('Individual')$get_data()
  my_partners <- ind[pid %in% ind_ids, partner_id] %>%
    unlist() %>%
    na.omit()
  my_partners_partner <- ind[pid %in% my_partners, partner_id] %>% unlist()
  pop$get('Individual')$remove_relationship(ids = ind_ids, type = "partner")
  ind_after <- pop$get('Individual')$get_data()
  my_partners_after <- ind_after[pid %in% my_partners_partner, partner_id] %>% unlist()
  my_partners_partner_after <- ind_after[pid %in% my_partners, partner_id] %>% unlist()
  expect_identical(my_partners_after, my_partners_partner_after)
})


test_that("invoke a derived variable and add new data", {
  create_toy_population()
  ind_ids <- sample(pop$get('Individual')$get_ids(), 50)
  Ind <- pop$get('Individual')
  Ind$remove_relationship(ids = ind_ids, type = "partner") # this creates .past_partner_id variable
  migrants <- pop_register(pop, ind_data = toy_individuals, hh_data = toy_households)
  n_before <- Ind$n()
  pop$add_population(ind_data = migrants$ind_data, hh_data = migrants$hh_data)
  expect_gt(Ind$n(), n_before)
})

test_that("get_parent_hid", {
  create_toy_population()
  Ind <- pop$get('Individual')
  self_f_hid <- Ind$get_data()[!is.na(father_id), hid]
  self_m_hid <- Ind$get_data()[!is.na(mother_id), hid]
  expect_setequal(self_f_hid, Ind$get_parent_hid(ids = Ind$get_data()[!is.na(father_id), pid])[["father_hid"]])
  expect_setequal(self_m_hid, Ind$get_parent_hid(ids = Ind$get_data()[!is.na(mother_id), pid])[["mother_hid"]])
})
