test_that("init", {
  Ntwk <-
    Network$new(
      toy_transport_network,
      id_col = "id",
      from_node = "fnode",
      to_node = "tnode",
      dist = "length"
    )
  if (requireNamespace("dodgr")) {
    d <- dodgr::dodgr_dists(Ntwk$get_dodgr_graph())
    n_unique_nodes <- uniqueN(c(Ntwk$get_data()$fnode,Ntwk$get_data()$tnode))
    checkmate::expect_matrix(d, nrows = n_unique_nodes, ncols = n_unique_nodes)
  }
  expect_error(Network$new(toy_transport_network, id_col = "nb_lanes"))
  expect_error(Network$new(toy_transport_network, id_col = "eid"))
  checkmate::expect_data_table(Ntwk$get_data())
})
