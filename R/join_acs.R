#' Calculate estimates as a percent of the denominators for ACS data
#'
#' Use the denominator_column_id value from the column metadata added with
#' [label_acs_metadata()] to calculate the estimate as a percent share of the
#' denominator value. [tidycensus::moe_prop()] is used to calculate the margin
#' of error for the percentage. Typically, this function should only be used as
#' an internal function.
#'
#' @param data A data frame with column names including "column_id", "column_title",
#' "denominator_column_id", "estimate", and "moe".
#' @param geoid_col A GeoID column name to use if perc is `TRUE`, Defaults to
#'   'GEOID'.
#' @inheritParams base::round
#' @inheritParams dplyr::left_join
#' @seealso [tidycensus::moe_prop()]
#' @keywords internal
#' @export
#' @importFrom dplyr filter select left_join mutate join_by
#' @importFrom tidycensus moe_prop
join_acs_percent <- function(data,
                             geoid_col = "GEOID",
                             denominator_col = "denominator_column_id",
                             na_matches = "never",
                             digits = 2) {
  stopifnot(
    all(has_name(data, c(
      geoid_col, "column_id", "column_title",
      denominator_col, "estimate", "moe"
    )))
  )

  if (nrow(dplyr::filter(
    data,
    .data[[denominator_col]] %in% data[["column_id"]],
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
      "{denominator_col}" := column_id
    )

  data |>
    dplyr::left_join(
      denominator_data,
      by = dplyr::join_by({{ geoid_col }}, {{ denominator_col }}),
      na_matches = na_matches
    ) |>
    dplyr::mutate(
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

#' Use parent column IDs to join parent column titles to ACS data
#'
#' @param data A data frame with the specified column names. Expected to be
#'   labelled using [label_acs_metadata()].
#' @param column_id_col,column_title_col,parent_id_col Column ID, column title,
#'   and parent column ID.
#' @param suffix Suffix passed to [dplyr::left_join()], Default: `c("",
#'   "_parent")`
#' @inheritParams dplyr::left_join
#' @return A modified data frame.
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
