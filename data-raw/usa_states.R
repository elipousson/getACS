states <- tigris::states()

divisions <- tigris::divisions()

regions <- tigris::regions()

usa_states <- states |>
  sf::st_drop_geometry() |>
  select(
    state = NAME,
    state_abb = STUSPS,
    STATE_GEOID = GEOID,
    DIVISION_GEOID = DIVISION,
    REGION_GEOID = REGION
  ) |>
  left_join(
    divisions |>
      sf::st_drop_geometry() |>
      select(
        DIVISION_GEOID = GEOID,
        division = NAME
      )
  ) |>
  left_join(
    regions |>
      sf::st_drop_geometry() |>
      select(
        REGION_GEOID = GEOID,
        region = NAME
      )
  ) |>
  relocate(
    DIVISION_GEOID,
    .after = division
  ) |>
  relocate(
    REGION_GEOID,
    .after = region
  )

usethis::use_data(usa_states, overwrite = TRUE)
