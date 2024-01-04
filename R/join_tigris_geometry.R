#' Join geometry from `{tigris}` to an ACS data frame by GeoID
#'
#' @keywords internal
#' @noRd
get_tigris_geometry <- function(data = NULL,
                                geography = NULL,
                                state = NULL,
                                county = NULL,
                                year = 2022,
                                by = "GEOID",
                                suffix = c("", "_geometry"),
                                crs = NULL,
                                ...) {
  check_installed("sf")

  params <- get_geography_params(
    geography = geography,
    year = year,
    state = state,
    county = county
  )

  params[["geography"]] <- NULL

  if (inherits(data, "sf")) {
    params[["filter_by"]] <- sf::st_bbox(data)
    data <- sf::st_drop_geometry(data)
  }

  geometry <- switch(geography,
    "block" = exec(tigris::blocks, !!!params, ..., year = year),
    "block group" = exec(tigris::block_groups, !!!params, ..., year = year),
    "tract" = exec(tigris::tracts, !!!params, ..., year = year),
    "county" = exec(tigris::counties, !!!params, ..., year = year),
    "state" = exec(tigris::tracts, !!!params, ..., year = year)
  )

  if (is.null(crs)) {
    return(geometry)
  }

  sf::st_transform(geometry, crs = sf::st_crs(crs))
}


join_tigris_geometry <- function(data = NULL,
                                 geography = NULL,
                                 state = NULL,
                                 county = NULL,
                                 year = 2022,
                                 ...) {
  if (!is.null(data)) {
    stopifnot(
      has_name(data, "geography"),
      any(has_name(data, c("state", "county")))
    )

    geometry_params <- dplyr::select(
      data,
      dplyr::any_of(c("geography", "state", "county"))
    )

    geometry_params <- dplyr::distinct(geometry_params)
  } else {
    geometry_params <- get_geography_params(
      geography = geography,
      year = year,
      state = state,
      county = county
    )

    geometry_params <- vctrs::data_frame(!!!geometry_params)
  }

  tigris_data <- purrr::pmap(
    geometry_params,
    \(geography, state, county) {
      switch(geography,
        "block" = tigris::blocks(state = state, county = county, ...),
        "block group" = tigris::block_groups(
          state = state,
          county = county,
          ...
        ),
        "tract" = tigris::tracts(state = state, county = county, ...),
        "county" = tigris::counties(state = state, ...),
        "state" = tigris::states(...)
      )
    }
  )

  tigris_data <- set_names(tigris_data, geography_data[["geography"]])

  # FIXME: Add list_rbind to utilities
  tigris_data <- purrr::list_rbind(
    tigris_data,
    names_to = "geography"
  )

  tigris_data <- sf::st_as_sf(tigris_data)

  if (is.null(data)) {
    return(tigris_data)
  }

  data <- dplyr::left_join(
    data,
    dplyr::select(tigris_data, geography, GEOID),
    by = dplyr::join_by(geography, GEOID)
  )

  sf::st_as_sf(data)
}
