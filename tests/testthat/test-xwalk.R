test_that("xwalk functions work", {
  skip_if_not_installed("sf")

  block_xwalk <- make_block_xwalk(
    state = "MD",
    county = "Kent County"
  )

  expect_s3_class(block_xwalk, "sf")

  area <- sf::st_buffer(block_xwalk[1, ], dist = 10000) |>
    dplyr::mutate(
      NAME = "Area name"
    ) |>
    dplyr::select(NAME)

  area_xwalk <- make_area_xwalk(
    area = area,
    block_xwalk = block_xwalk,
    coverage = TRUE
  )

  expect_s3_class(area_xwalk, "tbl_df")

  acs_data <- get_acs_tables(
    geography = "tract",
    state = "MD",
    county = "Kent County",
    table = "B02001",
    year = 2022
  )

  acs_data_interp <- use_area_xwalk(
    data = acs_data,
    area_xwalk = area_xwalk
  )

  expect_snapshot(
    acs_data_interp
  )
})
