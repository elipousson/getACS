test_that("gt_acs works", {
  md_acs_data <- get_acs_tables(
    geography = "county",
    state = "MD",
    table = "B01003"
  )

  baltimore_acs_data <- md_acs_data |>
    dplyr::filter(NAME == "Baltimore city, Maryland")

  tbl1 <- gt_acs(baltimore_acs_data)

  expect_snapshot(
    tbl1[["_options"]]
  )

  tbl2 <- gt_acs_compare(md_acs_data)

  expect_snapshot(
    tbl2[["_options"]]
  )
})


test_that("gt_acs_compare works", {
  county_acs_data <- get_acs_geographies(
    geography = c("county", "state"),
    county = "Baltimore city",
    state = "MD",
    table = "B01003"
  )

  tbl3 <- gt_acs_compare(county_acs_data)

  expect_snapshot(
    tbl3[["_boxhead"]]
  )
})

