test_that("ggplot2 functions work", {
  acs_data <- get_acs_geographies(
    geography = c("county", "state"),
    state = "MD",
    table = "B01003",
    quiet = TRUE
  )

  acs_data <- acs_data |>
    fmt_acs_county()

  county_pop_plot <- acs_data |>
    filter_acs(geography = "county") |>
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(estimate, NAME)) +
    labs_acs_survey(table = "B01003")

  expect_snapshot(
    county_pop_plot$labels$caption
  )
})
