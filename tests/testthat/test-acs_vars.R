test_that("load_acs_vars works", {
  vars_data <- load_acs_vars(year = 2022)

  expect_s3_class(
    vars_data,
    "data.frame"
  )

  # expect_snapshot(
  #   vars_data
  # )
})
