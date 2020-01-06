# make sure these packages are installed.
library(sf)
library(dplyr)
library(mapview)
library(pryr)
library(dodgr)
library(usethis)
library(data.table)
library(janitor)

# zone data
toy_zones <-
  sf::read_sf("data-raw/australia-2011-sa2-zones/SA2_2011_AUST.shp") %>%
  dplyr::filter(SA3_NAME11 == "Melbourne City") %>%
  janitor::clean_names(.) %>%
  dplyr::mutate(zid = sa2_main11)

# mapview::mapview(toy_zones)
pryr::object_size(toy_zones)
usethis::use_data(toy_zones, overwrite = TRUE)

# network data
union_toy_zones <-
  sf::st_union(toy_zones) %>%
  sf::st_buffer(., 0.0002)

toy_network <-
  sf::read_sf("data-raw/melbourne-network/sections.shp") %>%
  sf::st_transform(x = ., crs = st_crs(union_toy_zones)) %>%
  sf::st_intersection(., union_toy_zones) %>%
  dplyr::mutate(length = as.numeric(sf::st_length(geometry)),
                rd_type = as.factor(rd_type)) %>%
  dplyr::filter(!rd_type %in% c("8", "1")) %>%
  janitor::clean_names(.)

# view the intersected parts
# mapview::mapview(list(toy_network, union_toy_zones), zcol = list("rd_type", NULL))

# check that all lines are nodes reachable by any other nodes
toy_network_dodgr_graph <-
  toy_network %>%
  dplyr::select(from = fnode, to = tnode, dist = length, id) %>%
  sf::st_drop_geometry(.)

# only keep lines that can reach node 79150, it's a node in the CBD
dr <- dodgr::dodgr_dists(toy_network_dodgr_graph, to = "79150", parallel = T)
dr_dt <- as.data.table(as.table(dr)) %>%
  {setnames(., names(.), c("from", "to", "shortest_dist"))}
ids_of_connected_toy_network <-
  toy_network_dodgr_graph %>%
  dplyr::filter(!from %in% dr_dt[is.na(shortest_dist), from] &
                  !to %in% dr_dt[is.na(shortest_dist), to]) %>%
  .$id

# you can see that there are some line features that are not connected to the main
# road network but remain in the data as show by the following plot
# mapview::mapview(list(toy_network %>% filter(id %in% ids_of_connected_toy_network),
#                       toy_network))

# manually excluded some identified dangling lines
toy_network <-
  toy_network %>%
  dplyr::filter(id %in% ids_of_connected_toy_network & !id %in% c("38962", "38961")) %>%
  dplyr::select(-c("name", "eid")) %>%
  dplyr::mutate(
    id = as.integer(id),
    rd_type = as.character(rd_type)
  )

pryr::object_size(toy_network)
usethis::use_data(toy_network, overwrite = TRUE)

