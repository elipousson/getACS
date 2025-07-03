#' Join percent estimates to ACS data based on denominator values
#'
#' [join_acs_percent()] uses the denominator_column_id value from the column
#' metadata added with [label_acs_metadata()] to calculate the estimate as a
#' percent share of the denominator value. [tidycensus::moe_prop()] is used to
#' calculate the margin of error for the percentage. [join_acs_percent_parent()]
#' is a variation that, by default, calculates the percentage values based on
#' the `"parent_column_id"` instead of the `"denomination_column_id"`.
#'
#' @inheritParams join_acs_denominator
#' @param perc If `FALSE`, return data joined with [join_acs_denominator()] and
#'   skip joining percent values. Defaults to `TRUE`.
#' @inheritParams acs_perc_cols
#' @seealso [tidycensus::moe_prop()], [camiller::calc_shares()]
#' @export
#' @importFrom dplyr filter select left_join mutate join_by
#' @importFrom tidycensus moe_prop
join_acs_percent <- function(
  data,
  geoid_col = "GEOID",
  column_id_col = "column_id",
  denominator_col = NULL,
  denominator_prefix = "denominator_",
  value_col = "estimate",
  moe_col = "moe",
  perc = TRUE,
  perc_prefix = "perc",
  perc_sep = "_",
  na_matches = "never",
  digits = 2
) {
  data <- join_acs_denominator(
    data = data,
    value_col = value_col,
    moe_col = moe_col,
    geoid_col = geoid_col,
    column_id_col = column_id_col,
    denominator_col = denominator_col,
    denominator_prefix = denominator_prefix,
    na_matches = na_matches,
    digits = digits
  )

  if (!perc) {
    return(data)
  }

  perc_cols <- acs_perc_cols(
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep
  )

  denominator_value_col <- paste0(denominator_prefix, value_col)
  denominator_moe_col <- paste0(denominator_prefix, moe_col)

  dplyr::mutate(
    data,
    "{perc_cols[[1]]}" := round(
      .data[[value_col]] / .data[[denominator_value_col]],
      digits = digits
    ),
    "{perc_cols[[2]]}" := round(
      tidycensus::moe_prop(
        .data[[value_col]],
        .data[[denominator_value_col]],
        .data[[moe_col]],
        .data[[denominator_moe_col]]
      ),
      digits = digits
    ),
    .after = dplyr::all_of(moe_col)
  )
}

#' @rdname join_acs_percent
#' @name join_acs_percent_parent
#' @export
join_acs_percent_parent <- function(
  data,
  geoid_col = "GEOID",
  column_id_col = "column_id",
  denominator_col = NULL,
  denominator_prefix = "parent_",
  value_col = "estimate",
  moe_col = "moe",
  perc_prefix = "perc_parent",
  perc_sep = "_",
  na_matches = "never",
  digits = 2
) {
  join_acs_percent(
    data,
    geoid_col = geoid_col,
    column_id_col = column_id_col,
    denominator_col = denominator_col,
    denominator_prefix = denominator_prefix,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    na_matches = na_matches,
    digits = digits
  )
}

#' Join denominator values based on a supplied denominator column
#'
#' Note that this function and the related [join_acs_percent()] function depends
#' on the column-level metadata supplied by [label_acs_metadata()].
#'
#' @param data A data frame with column names including "column_id",
#'   "column_title", "denominator_column_id", "estimate", and "moe".
#' @param geoid_col A GeoID column name to use if perc is `TRUE`, Defaults to
#'   'GEOID'.
#' @param column_id_col Column ID column name from Census Reporter metadata.
#'   Defaults to "column_id"
#' @param column_title_col Column title column name. Defaults to "column_title".
#' @param denominator_col Denominator column ID name from Census Reporter
#'   metadata. Defaults to `NULL`
#' @param denominator_prefix Prefix to use for denominator column names.
#' @param value_col Value column name
#' @param moe_col Margin of error column name
#' @inheritParams base::round
#' @inheritParams dplyr::left_join
#' @inheritParams rlang::args_error_context
#' @export
join_acs_denominator <- function(
  data,
  geoid_col = "GEOID",
  value_col = "estimate",
  moe_col = "moe",
  column_id_col = "column_id",
  column_title_col = "column_title",
  denominator_col = NULL,
  denominator_prefix = "denominator_",
  na_matches = "never",
  digits = 2,
  call = caller_env()
) {
  denominator_id_col <- denominator_col %||%
    paste0(denominator_prefix, column_id_col)

  nm <- c(
    geoid_col,
    column_id_col,
    column_title_col,
    denominator_id_col,
    value_col
  )

  check_has_name(data, nm = nm, call = call)

  if (!has_name(data, moe_col)) {
    data[[moe_col]] <- NA_integer_
  }

  if (
    nrow(dplyr::filter(
      data,
      !is.na(.data[[denominator_id_col]]),
      .data[[denominator_id_col]] %in% data[[column_id_col]],
      .data[["indent"]] > 0
    )) >
      1
  ) {
    cli_alert_warning(
      "{.arg data} may not contain all of the denominator values needed
      to calculate the percent estimates for each variables.",
      wrap = TRUE
    )
  }

  denominator_data <- data |>
    dplyr::filter(.data[[column_id_col]] %in% data[[denominator_id_col]])

  denominator_data <- denominator_data |>
    dplyr::select(
      {{ geoid_col }},
      "{denominator_prefix}{value_col}" := all_of(value_col),
      "{denominator_prefix}{moe_col}" := all_of(moe_col),
      "{denominator_prefix}{column_title_col}" := all_of(column_title_col),
      "{denominator_id_col}" := all_of(column_id_col)
    )

  if (inherits(denominator_data, "sf")) {
    check_installed("sf")
    denominator_data <- sf::st_drop_geometry(denominator_data)
  }

  dplyr::left_join(
    data,
    denominator_data,
    by = dplyr::join_by({{ geoid_col }}, {{ denominator_id_col }}),
    na_matches = na_matches
  )
}

#' Join ACS data from a single reference geography by variable to calculate a
#' ratio value based on the reference geography data
#'
#' [join_acs_geography_ratio()] uses data from [get_acs_geographies()] to
#' support the calculation of proportions join parent column titles to a data
#' frame of ACS data.
#'
#' @param data A data frame with column names matching the supplied parameters.
#' @param variable_col Variable column name to join as join variable, Default:
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
join_acs_geography_ratio <- function(
  data,
  variable_col = "variable",
  value_col = "estimate",
  moe_col = "moe",
  geography = "county",
  na_matches = "never",
  digits = 2
) {
  check_has_name(
    data,
    nm = c(variable_col, value_col, moe_col, "geography")
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
    check_installed("sf")
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
      "{.arg data} already contains columns named {ratio_value_col}
      or {ratio_moe_col}",
      "i" = "These values will be over-written by this function."
    )
  }

  data <- dplyr::mutate(
    data,
    "{ratio_value_col}" := .data[[value_col]] / .data[[geography_value_col]],
    "{ratio_moe_col}" := tidycensus::moe_ratio(
      .data[[value_col]],
      .data[[geography_value_col]],
      .data[[moe_col]],
      .data[[geography_moe_col]]
    ),
    .after = all_of(moe_col)
  )

  dplyr::mutate(
    data,
    dplyr::across(
      all_of(c(ratio_value_col, ratio_moe_col)),
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
join_acs_parent_column <- function(
  data,
  column_id_col = "column_id",
  column_title_col = "column_title",
  parent_id_col = "parent_column_id",
  suffix = c("", "_parent"),
  na_matches = "never",
  relationship = "many-to-one"
) {
  parent_data <- data[data[[column_id_col]] %in% data[[parent_id_col]], ]

  if (nrow(parent_data) == 0) {
    cli_abort(
      "{.arg parent_data} must have at least one row."
    )
  }

  parent_data <- dplyr::distinct(
    parent_data,
    .data[[column_id_col]],
    .data[[column_title_col]]
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
