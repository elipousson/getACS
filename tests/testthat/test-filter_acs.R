test_that("filter_acs works", {
  edu_data <- get_acs_geographies(
    c("county", "state"),
    table = "B15003",
    state = "MD",
    county = "Baltimore city"
  )

  expect_s3_class(
    filter_acs(edu_data, vars = "B15003_017"),
    "data.frame"
  )

  expect_s3_class(
    filter_acs(edu_data, vars = 17),
    "data.frame"
  )

  expect_s3_class(
    filter_acs(edu_data, drop_vars = 1),
    "data.frame"
  )

  expect_s3_class(
    filter_acs(edu_data, drop_vars = 1),
    "data.frame"
  )

  expect_s3_class(
    filter_acs(edu_data, geography = "county"),
    "data.frame"
  )

  expect_s3_class(
    filter_acs(edu_data, column = "Master's degree"),
    "data.frame"
  )
})
