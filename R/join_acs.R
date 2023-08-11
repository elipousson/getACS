#' Join percent estimates to ACS data based on denominator values
#'
#' [join_acs_percent()] uses the denominator_column_id value from the column
#' metadata added with [label_acs_metadata()] to calculate the estimate as a
#' percent share of the denominator value. [tidycensus::moe_prop()] is used to
#' calculate the margin of error for the percentage.
#'
#' @param data A data frame with column names including "column_id", "column_title",
#' "denominator_column_id", "estimate", and "moe".
#' @param geoid_col A GeoID column name to use if perc is `TRUE`, Defaults to
#'   'GEOID'.
#' @param denominator_col "denominator_column_id"
#' @inheritParams base::round
#' @inheritParams dplyr::left_join
#' @seealso [tidycensus::moe_prop()], [camiller::calc_shares()]
#' @export
#' @importFrom dplyr filter select left_join mutate join_by
#' @importFrom tidycensus moe_prop
join_acs_percent <- function(data,
                             geoid_col = "GEOID",
                             column_col = "column_id",
                             denominator_col = "denominator_column_id",
                             na_matches = "never",
                             digits = 2) {
  data <- join_acs_denominator(
    data = data,
    geoid_col = geoid_col,
    column_col = column_col,
    denominator_col = denominator_col,
    na_matches = na_matches,
    digits = digits
  )

  dplyr::mutate(
    data,
    perc_estimate = round(estimate / denominator_estimate, digits = digits),
    perc_moe = round(
      tidycensus::moe_prop(
        estimate, denominator_estimate,
        moe, denominator_moe
      ),
      digits = digits
    ),
    .after = all_of("moe")
  )
}

#' @noRd
join_acs_denominator <- function(data,
                                 geoid_col = "GEOID",
                                 column_col = "column_id",
                                 denominator_col = "denominator_column_id",
                                 na_matches = "never",
                                 digits = 2) {
  stopifnot(
    all(has_name(data, c(
      geoid_col, column_col, "column_title",
      denominator_col, "estimate"
    )))
  )

  if (!has_name(data, "moe")) {
    data[["moe"]] <- NA_integer_
  }

  if (nrow(dplyr::filter(
    data,
    .data[[denominator_col]] %in% data[[column_col]],
    .data[["indent"]] > 0
  )) > 1) {
    cli_alert_warning(
      "{.arg data} may not contain all of the denominator values needed
      to calculate the percent estimates for each variables.",
      wrap = TRUE
    )
  }

  denominator_data <- data |>
    dplyr::filter(column_id %in% data[[denominator_col]]) |>
    dplyr::select(
      {{ geoid_col }},
      denominator_estimate = estimate,
      denominator_moe = moe,
      denominator_column_title = column_title,
      "{denominator_col}" := dplyr::all_of(column_col)
    )

  if (inherits(denominator_data, "sf")) {
    denominator_data <- sf::st_drop_geometry(denominator_data)
  }

  dplyr::left_join(
    data,
    denominator_data,
    by = dplyr::join_by({{ geoid_col }}, {{ denominator_col }}),
    na_matches = na_matches
  )
}

#' Join ACS data from a single reference geography by variable to calculate a
#' ratio value based on the reference geography data
#'
#' [join_acs_prop()] uses data from [get_acs_geographies()] to support the
#' calculation of proportions join parent column titles to a data frame of ACS
#' data.
#'
#' @param data A data frame with column names matching the supplied parameters.
#' @param column_col Variable column name to join as join variable, Default:
#'   'variable'
#' @param value_col,moe_col Estimate and margin of error column names,
#'   Default: 'estimate' and 'moe'
#' @param geography Value in geography column to use as comparison values,
#'   Default: 'county'
#' @inheritParams dplyr::left_join
#' @inheritParams base::round
#' @seealso [tidycensus::moe_ratio()]
#' @returns A data frame with new estimate and moe columns prefixed with
#'   "ratio_".
#' @rdname join_acs_geography_ratio
#' @export
#' @importFrom dplyr filter select left_join join_by mutate across all_of
#' @importFrom tidycensus moe_ratio
join_acs_geography_ratio <- function(data,
                                     variable_col = "variable",
                                     value_col = "estimate",
                                     moe_col = "moe",
                                     geography = "county",
                                     na_matches = "never",
                                     digits = 2) {
  stopifnot(
    all(has_name(data, c(variable_col, value_col, moe_col, "geography")))
  )

  check_string(geography, allow_empty = FALSE)

  geography_value_col <- paste0(geography, "_", value_col)
  geography_moe_col <- paste0(geography, "_", moe_col)

  comparison_data <- data |>
    dplyr::filter(.data[["geography"]] %in% {{ geography }}) |>
    dplyr::select(
      all_of(variable_col),
      "{geography_value_col}" := all_of(value_col),
      "{geography_moe_col}" := all_of(moe_col)
    )

  stopifnot(
    nrow(comparison_data) > 0
  )

  if (inherits(comparison_data, "sf")) {
    comparison_data <- sf::st_drop_geometry(comparison_data)
  }

  data <- dplyr::left_join(
    data,
    comparison_data,
    by = dplyr::join_by({{ variable_col }}),
    na_matches = na_matches
  )

  ratio_value_col <- paste0("ratio_", value_col)
  ratio_moe_col <- paste0("ratio_", moe_col)

  if (any(has_name(data, c(ratio_value_col, ratio_moe_col)))) {
    cli_warn(
      "{.arg data} already contains columns named {ratio_value_col} or {ratio_moe_col}",
      "i" = "These values will be over-written by this function."
    )
  }

  data <- dplyr::mutate(
    data,
    "{ratio_value_col}" := .data[[value_col]] / .data[[geography_value_col]],
    "{ratio_moe_col}" := tidycensus::moe_ratio(
      .data[[value_col]], .data[[geography_value_col]],
      .data[[moe_col]], .data[[geography_moe_col]]
    ),
    .after = all_of(moe_col)
  )

  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(c(ratio_value_col, ratio_moe_col)),
      function(x) {
        round(x, digits = digits)
      }
    )
  )
}

#' Join parent column titles to ACS data based on parent column ID values
#'
#' [join_acs_parent_column()] uses data labelled with parent_column_id values to
#' join parent column titles to a data frame of ACS data.
#'
#' @param data A data frame with the specified column names. Expected to be
#'   labelled using [label_acs_metadata()].
#' @param column_id_col,column_title_col,parent_id_col Column ID, column title,
#'   and parent column ID.
#' @param suffix Suffix passed to [dplyr::left_join()], Default: `c("",
#'   "_parent")`
#' @inheritParams dplyr::left_join
#' @return A data frame with added parent column title.
#' @rdname join_acs_parent_column
#' @export
#' @importFrom dplyr distinct left_join
join_acs_parent_column <- function(data,
                                   column_id_col = "column_id",
                                   column_title_col = "column_title",
                                   parent_id_col = "parent_column_id",
                                   suffix = c("", "_parent"),
                                   na_matches = "never",
                                   relationship = "many-to-one") {
  parent_data <- data[data[[column_id_col]] %in% data[[parent_id_col]], ]

  stopifnot(
    nrow(parent_data) > 0
  )

  parent_data <- dplyr::distinct(
    parent_data,
    .data[[column_id_col]], .data[[column_title_col]]
  )

  by <- set_names(column_id_col, parent_id_col)

  dplyr::left_join(
    x = data,
    y = parent_data,
    by = by,
    suffix = suffix,
    na_matches = na_matches,
    multiple = "any",
    relationship = relationship
  )
}


#' Join geometry from `{tigris}` to an ACS data frame by GeoID
#'
#' @keywords internal
#' @noRd
join_tigris_geometry <- function(data,
                                 geography,
                                 state = NULL,
                                 county = NULL,
                                 year = 2021,
                                 by = "GEOID",
                                 suffix = c("", "_geometry"),
                                 crs = NULL,
                                 ...) {
  params <- get_geography_params(
    geography = geography,
    year = year,
    state = state,
    county = county
  )

  params[["geography"]] <- NULL

  geometry <- switch(geography,
    "tract" = exec(tigris::tracts, !!!params, ..., year = year),
    "county" = exec(tigris::counties, !!!params, ..., year = year)
  )

  data <- dplyr::left_join(
    data,
    geometry,
    by = by,
    suffix = suffix
  )

  data <- sf::st_as_sf(data)

  if (is.null(crs)) {
    return(data)
  }

  sf::st_transform(data, crs = sf::st_crs(crs))
}
