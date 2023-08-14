test_that("fmt_acs works", {
  data <- data.frame(
    "NAME" = "Baltimore County, Maryland"
  )

  comparison <- data.frame(
    "NAME" = "Baltimore County"
  )

  expect_identical(
    fmt_acs_county(
      data,
      state = "Maryland"
    ),
    comparison
  )

  expect_s3_class(
    fmt_acs_county(
      gt::gt(data),
      state = "Maryland"
    ),
    "gt_tbl"
  )

  data <- data.frame(
    "column_title" = "10 minutes"
  )

  comparison <- data.frame(
    "column_title" = "10"
  )

  expect_identical(
    fmt_acs_minutes(
      data
    ),
    comparison
  )
})
