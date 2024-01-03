test_that("get_acs_tables works", {
  acs_data <- get_acs_tables(
    geography = "county",
    county = "Baltimore city",
    state = "MD",
    table = c("B01003", "B19013"),
    quiet = TRUE
  )

  expect_s3_class(
    acs_data,
    "data.frame"
  )

  expect_identical(
    unique(acs_data[["table_id"]]),
    c("B01003", "B19013")
  )

  expect_named(
    acs_data,
    c(
      "GEOID", "NAME",
      "variable", "column_id",
      "table_id", "estimate",
      "moe", "perc_estimate",
      "perc_moe", "geography", "county", "state", "table_title",
      "simple_table_title", "subject_area",
      "universe", "denominator_column_id",
      "topics", "line_number",
      "column_title", "indent",
      "parent_column_id", "denominator_estimate",
      "denominator_moe", "denominator_column_title"
    )
  )

  acs_geo_data <- get_acs_geographies(
    geography = c("county", "state"),
    county = "Baltimore city",
    state = "MD",
    table = "B01003",
    quiet = TRUE
  )

  expect_s3_class(
    acs_geo_data,
    "data.frame"
  )

  expect_identical(
    acs_geo_data[["geography"]],
    c("county", "state")
  )

  expect_identical(
    acs_geo_data[["county"]],
    c("Baltimore city", NA_character_)
  )

  acs_ts_data <- get_acs_ts(
    geography = "state",
    county = "Baltimore city",
    state = "MD",
    table = "B01003",
    year = 2021,
    quiet = TRUE
  )

  expect_s3_class(
    acs_ts_data,
    "data.frame"
  )

  expect_identical(
    acs_ts_data[["year"]],
    c(2021, 2016, 2011)
  )

  expect_identical(
    acs_ts_data[["variable"]],
    rep("B01003_001", 3)
  )
})
