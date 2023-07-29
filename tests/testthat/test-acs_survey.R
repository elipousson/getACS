test_that("acs_survey works", {
  expect_identical(
    acs_survey_match("acs1"),
    "acs1"
  )

  expect_identical(
    acs_survey_sample("acs3"),
    "3"
  )

  expect_identical(
    acs_survey_ts("acs5", 2020),
    c(2020, 2015, 2010)
  )

  expect_identical(
    acs_survey_label(),
    "2017-2021 ACS 5-year Estimates"
  )

  expect_identical(
    acs_survey_label_table(table = c("B19013", "B01003"), oxford_comma = FALSE),
    "2017-2021 ACS 5-year Estimates, Tables B19013 and B01003."
  )
})
