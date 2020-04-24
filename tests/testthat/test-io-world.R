test_that("io world works", {
  create_toy_world()
  file_path <- paste0(tempdir(), "/world.rds")
  write_world_obj(world, file_path)
  world <- read_world_obj(file_path)
})
