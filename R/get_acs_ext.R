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
#'   # EXAMPLE1
#' }
#' }
#' @rdname vec_get_acs
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom vctrs vec_recycle_common vec_rep vec_assign vec_slice
vec_get_acs <- function(...,
                        .fn = tidycensus::get_acs,
                        .size = NULL,
                        .call = caller_env()) {
  params <- vctrs::vec_recycle_common(..., .size = .size, .call = .call)

  if (is_empty(params)) {
    cli_abort("{.arg ...} can't be empty.", call = .call)
  } else {
    check_required(params[["geography"]], arg = "geography", call = call)
  }

  check_function(.fn, call = .call)

  df_list <- vctrs::vec_rep(
    list(data.frame()),
    times = length(params[[1]]),
    error_call = call
  )

  for (i in seq_along(params[[1]])) {
    df_list <- vctrs::vec_assign(
      x = df_list,
      i = i,
      value = list(
        exec(
          .fn = .fn,
          !!!lapply(params, vctrs::vec_slice, i = i, error_call = call)
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

#' Get multiple ACS tables or multiple tables for multiple geographies
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
#' @param label If `TRUE` (default), label the returned ACS data with
#'   [label_acs_metadata()] before returning the data frame.
#' @inheritParams label_acs_metadata
#' @inheritParams rlang::args_error_context
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   get_acs_tables(
#'     geography = "county",
#'     county = "Baltimore city",
#'     state = "MD",
#'     tables = c("B01003", "B19013")
#'   )
#'
#'   get_acs_geographies(
#'     geographies = c("county", "state"),
#'     state = "MD",
#'     tables = c("B01003", "B19013")
#'   )
#' }
#' }
#' @export
#' @importFrom purrr map list_rbind
#' @importFrom cli cli_progress_along col_blue symbol pb_bar pb_percent
#' @importFrom tidycensus get_acs
get_acs_tables <- function(geography,
                           table,
                           cache_table = TRUE,
                           year = 2021,
                           survey = "acs5",
                           label = TRUE,
                           perc = TRUE,
                           geoid = "GEOID",
                           ...) {
  survey_label <- acs_survey_label(
    survey = survey,
    year = year
  )

  check_character(table)
  table <- unique(table)

  if (length(table) > 50) {
    cli::cli_warn(
      "{.arg table} vectors longer than 50 may return an error from the Census API."
    )
  }

  acs_list <- vec_get_acs(
    geography = geography,
    table = table,
    year = year,
    survey = survey,
    .fn = get_acs_table_alert,
    ...
  )

  acs_data <- purrr::list_rbind(acs_list)

  if (!label) {
    cli::cli_progress_step("Download complete")
    return(acs_data)
  }

  cli::cli_progress_step("Labelling data")

  label_acs_metadata(
    acs_data,
    survey = survey,
    year = year,
    perc = perc,
    geoid = geoid
  )
}


#' @param geographies A character vector of geographies. See
#'   <https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus>
#'   for supported options. Defaults to `c("county", "state")`. If a supplied
#'   geography does not support county and state parameters, these options are
#'   dropped before calling [tidycensus::get_acs()]. Any required parameters are
#'   also bound to the returned data frame as new columns.
#' @param msa Name or GeoID of a metro area that should be filtered from the
#'   overall list of metro areas returned when geography or geographies  is
#'   "metropolitan/micropolitan statistical area", "cbsa", or "metropolitan
#'   statistical area/micropolitan statistical area".
#' @rdname get_acs_tables
#' @name get_acs_geographies
#' @export
#' @importFrom purrr map list_rbind
#' @importFrom cli cli_progress_along
get_acs_geographies <- function(geographies = c("county", "state"),
                                state = NULL,
                                county = NULL,
                                msa = NULL,
                                table = NULL,
                                cache_table = TRUE,
                                year = 2021,
                                label = TRUE,
                                survey = "acs5",
                                perc = TRUE,
                                geoid = "GEOID",
                                ...) {
  survey_label <- acs_survey_label(
    survey = survey,
    year = year
  )

  acs_list <- purrr::map(
    cli::cli_progress_along(
      geographies,
      format = "Downloading tables from {survey_label}",
      type = "tasks"
    ),
    function(i) {
      get_acs_geography(
        geography = geographies[[i]],
        state = state,
        county = county,
        msa = msa,
        table = table,
        cache_table = cache_table,
        year = year,
        label = label,
        survey = survey,
        perc = perc,
        geoid = geoid,
        ...
      )
    }
  )

  purrr::list_rbind(acs_list)
}

#' @rdname get_acs_tables
#' @name get_acs_geography
#' @inheritParams get_geography_params
#' @export
#' @importFrom dplyr filter
get_acs_geography <- function(geography,
                              state = NULL,
                              county = NULL,
                              msa = NULL,
                              table = NULL,
                              cache_table = TRUE,
                              year = 2021,
                              label = TRUE,
                              survey = "acs5",
                              perc = TRUE,
                              geoid = "GEOID",
                              ...,
                              call = caller_env()) {
  params <- get_geography_params(
    geography,
    state = state,
    county = county,
    year = year,
    call = call
  )

  acs_data <- exec(
    get_acs_tables,
    !!!params,
    year = year,
    survey = survey,
    table = table,
    cache_table = cache_table,
    label = label,
    perc = perc,
    geoid = geoid,
    ...
  )

  if (!is_null(msa) &&
    geography %in% c(
      "metropolitan/micropolitan statistical area",
      "cbsa", "metropolitan statistical area/micropolitan statistical area"
    )) {
    cli_alert_info(
      "Filtering data to {msa}"
    )

    acs_data <- dplyr::filter(
      acs_data,
      .data[[geoid]] %in% msa | .data[["NAME"]] %in% msa
    )
  }

  cbind(
    acs_data,
    as.data.frame(do.call(cbind, params))
  )
}

#' Get geography parameters
#'
#' Get a named list of parameters including geography and, optionally, state and
#' county, which are dropped or checked depending on whether the value is
#' required to download data at the specified geography using the Census API.
#'
#' @param geography The geography of your data.
#' @param state,county State and county. Defaults to `NULL`.
#' @param year Survey year.
#' @param allow_decennial If `TRUE`, allow geography values of "block" or
#'   "voting district" that are only supported by [tidycensus::get_decennial()].
#'   If `FALSE` (default), error on those geographies that are not supported by
#'   [tidycensus::get_acs()].
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @export
#' @importFrom vctrs list_drop_empty
get_geography_params <- function(geography,
                                 state = NULL,
                                 county = NULL,
                                 year = 2021,
                                 allow_decennial = FALSE,
                                 call = caller_env()) {
  check_string(geography, allow_empty = FALSE, call = call)

  if (!allow_decennial && (geography %in% c("block", "voting district"))) {
    cli_abort(
      "{.arg geography} can't be {.val {geography}} when downloading ACS data.",
      call = call
    )
  }

  if ((year < 2021) &&
    (identical(geography, "metropolitan/micropolitan statistical area"))) {
    # "metropolitan statistical area/micropolitan statistical area"
    cli_warn(
      c("{geography} us not a supported {.arg geography} for {year} data.",
        "i" = "Setting {.arg geography} to {.val cbsa}."
      )
    )

    geography <- "cbsa"
  }

  drop_state <- c(
    "us", "region", "division", "metropolitan/micropolitan statistical area",
    "metropolitan statistical area/micropolitan statistical area", "cbsa",
    "urban area", "zip code tabulation area", "zcta"
  )

  if (geography %in% drop_state) {
    state <- NULL
    county <- NULL
  }

  allow_county <- c(
    "county", "county subdivision", "tract",
    "block group", "cbg", "block"
  )

  if (!(geography %in% allow_county)) {
    county <- NULL
  }

  require_county <- "block"

  if (geography %in% require_county) {
    check_required(county, call = call)
  }

  require_state <- c(
    "county subdivision", "tract", "block group", "cbg",
    "block", "school district (elementary)",
    "school district (secondary)", "school district (unified)",
    "state legislative district (upper chamber)",
    "state legislative district (upper chamber)",
    "state legislative district (lower chamber)",
    "voting district"
  )

  if (geography %in% require_state) {
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
