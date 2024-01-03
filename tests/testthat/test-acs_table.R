test_that("acs_table_* functions work", {
  tbl_vars <- acs_table_variables("B15003")

  expect_snapshot(tbl_vars)

  race_iteration_vars <- acs_table_race_iteration("B25003")

  expect_snapshot(race_iteration_vars)
})
