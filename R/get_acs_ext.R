#' Vectorized variant of [tidycensus::get_acs]
#'
#' @param ... Additional parameters passed to .fn.
#' @param .fn Function to call with parameters, Defaults to
#'   `tidycensus::get_acs`. Function must require a geography parameter and
#'   return a data frame.
#' @inheritParams vctrs::vec_recycle_common
#' @inheritParams rlang::args_error_context
#' @returns A list of data frames (using default .fn value or another function
#'   that returns a data frame).
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # TODO: Add examples
#' }
#' }
#' @rdname vec_get_acs
#' @returns A list of data frames.
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom vctrs vec_recycle_common list_drop_empty vec_rep vec_assign
#'   vec_slice
vec_get_acs <- function(...,
                        .fn = tidycensus::get_acs,
                        .size = NULL,
                        .call = caller_env()) {
  vec_tidycensus(
    ...,
    .fn = .fn,
    .size = .size,
    .call = .call
  )
}

#' @noRd
vec_tidycensus <- function(...,
                           .fn,
                           .size = NULL,
                           .call = caller_env()) {
  params <- vctrs::list_drop_empty(list2(...))
  params <- vctrs::vec_recycle_common(!!!params, .size = .size, .call = .call)

  if (is_empty(params)) {
    cli_abort("{.arg ...} can't be empty.", call = .call)
  } else {
    check_required(params[["geography"]], arg = "geography", call = .call)
  }

  check_function(.fn, call = .call)

  df_list <- vctrs::vec_rep(
    list(data.frame()),
    times = length(params[[1]]),
    error_call = .call
  )

  for (i in seq_along(params[[1]])) {
    df_list <- vctrs::vec_assign(
      x = df_list,
      i = i,
      value = list(
        exec(
          .fn = .fn,
          !!!lapply(params, vctrs::vec_slice, i = i, error_call = .call)
        )
      )
    )
  }

  df_list
}

#' Helper function for get_acs_tables
#'
#' @noRd
get_acs_table_alert <- function(...) {
  cli::cli_progress_step(
    "Downloading table {list2(...)[['table']]}"
  )

  suppressMessages(tidycensus::get_acs(...))
}

#' Get multiple tables or multiple geographies of ACS data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' These functions wrap [tidycensus::get_acs()] and [label_acs_metadata()] to
#' support downloading multiple tables and combining tables into a single data
#' frame or downloading data for multiple geographies. Note that while the
#' Census API does not have a specific rate or request limit when using a Census
#' API key, using these functions with a large number of tables or geographies
#' may result in errors or failed requests.
#'
#' CRAN policies require that tidycensus avoid caching by default, however, this
#' package sets `cache_table = TRUE` by default to avoid unecessary load on the
#' Census API.
#'
#' @param table A character vector of tables.
#' @inheritParams tidycensus::get_acs
#' @inheritDotParams tidycensus::get_acs
#' @param crs Coordinate reference system to use for returned sf tibble when
#'   `geometry = TRUE` is passed to [tidycensus::get_acs()]. Defaults to `NULL`.
#' @param label If `TRUE` (default), label the returned ACS data with
#'   [label_acs_metadata()] before returning the data frame.
#' @param keep_geography If `TRUE` (default), bind geography and any supplied
#'   county or state columns to the returned data frame.
#' @inheritParams label_acs_metadata
#' @inheritParams cli_quiet
#' @inheritParams rlang::args_error_context
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_acs_tables(
#'     geography = "county",
#'     county = "Baltimore city",
#'     state = "MD",
#'     table = c("B01003", "B19013")
#'   )
#'
#'   get_acs_geographies(
#'     geography = c("county", "state"),
#'     state = "MD",
#'     table = c("B01003", "B19013")
#'   )
#' }
#' }
#' @export
#' @importFrom vctrs vec_cbind
#' @importFrom cli cli_progress_along col_blue symbol pb_bar pb_percent
#' @importFrom tidycensus get_acs
get_acs_tables <- function(geography,
                           table = NULL,
                           cache_table = TRUE,
                           year = 2022,
                           survey = "acs5",
                           variables = NULL,
                           moe_level = 90,
                           ...,
                           crs = NULL,
                           label = TRUE,
                           perc = TRUE,
                           reliability = FALSE,
                           keep_geography = TRUE,
                           geoid_col = "GEOID",
                           quiet = FALSE,
                           call = caller_env()) {
  cli_quiet(quiet)

  survey_label <- acs_survey_label(
    survey = survey,
    year = year
  )

  .fn <- tidycensus::get_acs

  variable_col <- "variable"
  variable_labels <- NULL

  if (is_character(variables) && is_named(variables)) {
    variable_labels <- tibble::enframe(
      variables,
      name = "variable",
      value = "variable_id"
    )

    variable_col <- "variable_id"
  }

  params <- list2(...)

  if (!is.null(table)) {
    table <- unique(table)
    if (length(table) > 50) {
      cli::cli_warn(
        "{.arg table} vectors longer than 50 may return an error from the Census API."
      )
    }

    if (is_null(variables)) {
      .fn <- get_acs_table_alert
    }
  }

  geography <- unique(geography)
  check_string(geography, allow_empty = FALSE, call = call)

  acs_list <- vec_get_acs(
    geography = geography,
    table = table,
    cache_table = cache_table,
    year = year,
    survey = survey,
    variables = variables,
    moe_level = moe_level,
    ...,
    .fn = .fn,
    .call = call
  )

  acs_data <- list_rbind(acs_list)

  if (keep_geography) {
    geography_cols <- as.data.frame(
      do.call(
        cbind,
        get_geography_params(geography = geography, year = year, ...)
      )
    )

    acs_data <- vctrs::vec_cbind(
      acs_data,
      geography_cols,
      .error_call = call
    )
  }

  if (is_true(params[["geometry"]])) {
    check_installed("sf", call = call)
    acs_data <- sf::st_as_sf(acs_data)

    if (!is.null(crs)) {
      acs_data <- sf::st_transform(acs_data, crs = sf::st_crs(crs))
    }
  }

  if (identical(params[["output"]], "wide") && label) {
    cli::cli_warn(
      "{.arg label} can't be {.code TRUE} when {.code output = 'wide'}"
    )
    label <- FALSE
  }

  if (!label) {
    cli::cli_progress_step("Download complete")
    return(acs_data)
  }

  cli::cli_progress_step("Labelling data")

  if (!is.null(variable_labels)) {
    acs_data <- acs_data |>
      dplyr::left_join(variable_labels, by = dplyr::join_by(variable))
  }

  label_acs_metadata(
    data = acs_data,
    survey = survey,
    year = year,
    perc = perc,
    reliability = reliability,
    moe_level = moe_level,
    geoid_col = geoid_col,
    variable_col = variable_col
  )
}


#' @param geography Required character vector of one or more geographies. See
#'   <https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus>
#'   for supported options. Defaults to `c("county", "state")` for
#'   [get_acs_geographies()]. If a supplied geography does not support county
#'   and state parameters, these options are dropped before calling
#'   [tidycensus::get_acs()]. Any required parameters are also bound to the
#'   returned data frame as new columns.
#' @param msa Name or GeoID of a metro area that should be filtered from the
#'   overall list of metro areas returned when geography or geographies  is
#'   "metropolitan/micropolitan statistical area", "cbsa", or "metropolitan
#'   statistical area/micropolitan statistical area".
#' @rdname get_acs_tables
#' @name get_acs_geographies
#' @export
#' @importFrom purrr map list_rbind
#' @importFrom cli cli_progress_along
get_acs_geographies <- function(geography = c("county", "state"),
                                variables = NULL,
                                table = NULL,
                                cache_table = TRUE,
                                year = 2022,
                                state = NULL,
                                county = NULL,
                                msa = NULL,
                                survey = "acs5",
                                ...,
                                label = TRUE,
                                perc = TRUE,
                                geoid_col = "GEOID",
                                quiet = FALSE) {
  cli_quiet(quiet)

  survey_label <- acs_survey_label(
    survey = survey,
    year = year
  )

  check_character(geography)
  geography <- unique(geography)

  acs_data <- map(
    seq_along(geography),
    function(i) {
      cli::cli_progress_step(
        "Downloading data for {geography[[i]]} from {survey_label}"
      )

      get_acs_geography(
        geography = geography[[i]],
        variables = variables,
        table = table,
        cache_table = cache_table,
        year = year,
        state = state,
        county = county,
        msa = msa,
        ...,
        label = label,
        survey = survey,
        perc = perc,
        geoid_col = geoid_col
      )
    }
  )

  acs_data <- list_rbind(acs_data)

  if (is_true(list2(...)[["geometry"]])) {
    acs_data <- sf::st_as_sf(acs_data)
  }

  acs_data
}

#' @rdname get_acs_tables
#' @name get_acs_geography
#' @inheritParams get_geography_params
#' @export
#' @importFrom dplyr filter
#' @importFrom vctrs vec_cbind
get_acs_geography <- function(geography,
                              variables = NULL,
                              table = NULL,
                              cache_table = TRUE,
                              year = 2022,
                              state = NULL,
                              county = NULL,
                              msa = NULL,
                              survey = "acs5",
                              ...,
                              label = TRUE,
                              perc = TRUE,
                              geoid_col = "GEOID",
                              call = caller_env()) {
  params <- get_geography_params(
    geography,
    year = year,
    state = state,
    county = county,
    call = call
  )

  acs_data <- exec(
    get_acs_tables,
    variables = variables,
    table = table,
    cache_table = cache_table,
    year = year,
    !!!params,
    survey = survey,
    ...,
    keep_geography = FALSE,
    label = label,
    perc = perc,
    geoid_col = geoid_col
  )

  if (!is_null(msa) &&
    geography %in% c(
      "metropolitan/micropolitan statistical area",
      "cbsa", "metropolitan statistical area/micropolitan statistical area"
    )) {
    cli_alert_info(
      "Filtering data to {msa}"
    )

    params[["msa"]] <- msa

    acs_data <- dplyr::filter(
      acs_data,
      .data[[geoid_col]] %in% msa | .data[["NAME"]] %in% msa
    )

    stopifnot(
      nrow(acs_data) > 0
    )
  }

  acs_data <- vctrs::vec_cbind(
    acs_data,
    as.data.frame(do.call(cbind, params)),
    .error_call = call
  )

  if (is_true(list2(...)[["geometry"]])) {
    acs_data <- sf::st_as_sf(acs_data)
  }

  acs_data
}

#' Get geography parameters
#'
#' Get a named list of parameters including geography and, optionally, state and
#' county, which are dropped or checked depending on whether the value is
#' required to download data at the specified geography using the Census API.
#'
#' @param geography The geography of your data.
#' @param year Survey year.
#' @param state,county State and county. Defaults to `NULL`.
#' @param allow_decennial If `TRUE`, allow geography values of "block" or
#'   "voting district" that are only supported by [tidycensus::get_decennial()].
#'   If `FALSE` (default), error on those geographies that are not supported by
#'   [tidycensus::get_acs()].
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @export
#' @importFrom vctrs list_drop_empty
get_geography_params <- function(geography,
                                 year = 2022,
                                 state = NULL,
                                 county = NULL,
                                 allow_decennial = FALSE,
                                 ...,
                                 call = caller_env()) {
  check_string(geography, allow_empty = FALSE, call = call)

  if (!allow_decennial && (geography %in% c("block", "voting district"))) {
    cli_abort(
      "{.arg geography} can't be {.val {geography}} when downloading ACS data.",
      call = call
    )
  }

  if (has_length(year, 1) && (year < 2021) &&
    (identical(geography, "metropolitan/micropolitan statistical area"))) {
    # "metropolitan statistical area/micropolitan statistical area"
    cli_warn(
      c("{geography} us not a supported {.arg geography} for {year} data.",
        "i" = "Setting {.arg geography} to {.val cbsa}."
      )
    )

    geography <- "cbsa"
  }

  if (geography %in% geographies_drop_state) {
    state <- NULL
    county <- NULL
  }

  if (!(geography %in% geographies_allow_county)) {
    county <- NULL
  }

  if (geography %in% geographies_require_county) {
    check_required(county, call = call)
  }

  if (geography %in% geographies_require_state) {
    check_required(state, call = call)
  }

  vctrs::list_drop_empty(
    list(
      "geography" = geography,
      "county" = county,
      "state" = state
    )
  )
}
