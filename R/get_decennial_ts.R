#' Get multiple years of decennial US Census data for time series analysis
#'
#' [get_decennial_ts()] is a wrapper for [tidycensus::get_decennial()] to handle
#' time series data.
#'
#' @param variables If any year value is 2020, variables must be the same length
#'   as year with each value corresponding to one of the years requested. This
#'   is a temporary requirement to address the mismatch between the available
#'   data for 2000 and 2010 relative to 2020.
#'   Default: `NULL`
#' @param year If year is length 1, it is treated as the max year and decennial
#'   Census years back to 2000, are added to the vector of requested years.
#'   Default: 2020
#' @inheritParams tidycensus::get_decennial
#' @inheritDotParams tidycensus::get_decennial
#' @returns A data frame with decennial Census data.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   md_counties <- get_decennial_ts(
#'     geography = "county",
#'     variables = c("P001001", "P001001", "P1_001N"),
#'     year = 2020,
#'     county = "Baltimore city",
#'     state = "MD",
#'     geometry = FALSE
#'   )
#' }
#' }
#' @seealso
#'  [tidycensus::get_decennial()]
#' @rdname get_decennial_ts
#' @export
#' @importFrom tidycensus get_decennial
#' @importFrom vctrs vec_cbind
get_decennial_ts <- function(geography,
                             variables = NULL,
                             table = NULL,
                             cache_table = TRUE,
                             year = 2020,
                             sumfile = NULL,
                             state = NULL,
                             county = NULL,
                             geometry = FALSE,
                             summary_var = NULL,
                             ...) {
  params <- get_geography_params(
    geography,
    year = year,
    state = state,
    county = county,
    allow_decennial = TRUE,
    call = call
  )

  if (has_length(year, 1)) {
    if (!all(year %in% c(2000, 2010, 2020))) {
      cli::cli_abort(
        c("{.arg year} can't include any values other than {c(2000, 2010, 2020)}.",
        "i" = "Try using NHGIS for earlier decennial Census data:
        {.url https://www.nhgis.org/}")
      )
    }
    year <- seq(2000, year, by = 10)
  }

  if ((2020 %in% year) && (length(year) != length(variables))) {
    cli_abort(
      "{.arg variables} must be the same length as {.arg year} if
      year values include 2020."
    )
  }

  # if (length(county) > 1) {
  #   # FIXME: Figure out how to handle multiple counties
  #   county_year_grid <- vctrs::vec_expand_grid(county = county, year = year)
  # }

  decennial_list <- vec_tidycensus(
    year = year,
    !!!params,
    variables = variables,
    table = table,
    cache_table = cache_table,
    sumfile = sumfile,
    geometry = geometry,
    summary_var = summary_var,
    ...,
    .fn = tidycensus::get_decennial
  )

  decennial_df <- list_rbind(
    set_names(decennial_list, year),
    names_to = "year"
  )

  vctrs::vec_cbind(
    decennial_df,
    as.data.frame(do.call(cbind, params)),
    .error_call = call
  )
}
