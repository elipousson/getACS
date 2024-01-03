test_that("select_acs works", {
  acs_data <- get_acs_tables(
    geography = "county",
    county = "Baltimore city",
    state = "MD",
    table = "B01003",
    quiet = TRUE
  )

  expect_snapshot(select_acs(acs_data))
})
