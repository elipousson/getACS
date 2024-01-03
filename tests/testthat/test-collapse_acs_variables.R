test_that("multiplication works", {
  edu_data <- get_acs_tables(
    "county",
    table = "B15003",
    state = "MD",
    county = "Baltimore city"
  )

  table_vars <- acs_table_variables("B15003")

  edu_collapse_data <- collapse_acs_variables(
    edu_data,
    "Total" = table_vars[1],
    "5th Grade or less" = table_vars[5:9],
    "6th to 8th Grade" = table_vars[10:12],
    "9th to 11th Grade" = table_vars[13:15],
    other_level = "Other"
  )

  expect_snapshot(edu_collapse_data)
})
